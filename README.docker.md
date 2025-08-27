# Dockerized MRDviz

This document provides instructions for running the MRDviz application in a Docker container. Dockerizing MRDviz makes it easy to deploy the application in various environments without worrying about R package dependencies or system configuration.

## Prerequisites

- [Docker](https://docs.docker.com/get-docker/) installed on your system
- [Docker Compose](https://docs.docker.com/compose/install/) (optional, but recommended)

## Quick Start

The simplest way to run the dockerized MRDviz application is using Docker Compose:

```bash
# Clone the repository (if you haven't already)
git clone https://github.com/abbvie-external/MRDviz
cd MRDviz

# Build and start the container
docker-compose up -d

# To stop the container
docker-compose down
```

Once the container is running, you can access the MRDviz application at:
http://localhost:3838

## Manual Docker Setup

If you prefer not to use Docker Compose, you can pull (or build) and run the container manually:

```bash
# Pull the Docker image
docker pull ghcr.io/abbvie-external/mrdviz

# Or build the Docker image
docker build -t mrdviz .

# Run the container
docker run -d --name mrdviz -p 3838:3838 mrdviz
```

## Customization

### Persistent Data Storage

To store data persistently, you can mount a volume to the container. This is useful for saving uploaded data or simulation results.

In the `docker-compose.yml` file, uncomment and modify the volume section:

```yaml
volumes:
  - ./data:/srv/shiny-server/data
```

Make sure the `./data` directory exists in your local filesystem.

### Using a Specific R Package Version

If you need to use a specific version of an R package, you can modify the Dockerfile:

```dockerfile
# Install specific package versions
RUN R -e "install.packages(c('package_name@version'))"
```

### Changing the Port

To use a different port, modify the `ports` section in the `docker-compose.yml` file:

```yaml
ports:
  - "8282:3838"  # Maps local port 8282 to container port 3838
```

Or when running with the `docker run` command:

```bash
docker run -d --name mrdviz -p 8282:3838 mrdviz
```

## How It Works

The Docker setup for MRDviz consists of:

1. A `Dockerfile` that:
   - Uses the `rocker/shiny` base image (which includes R and Shiny Server)
   - Installs all required R packages and system dependencies
   - Copies and installs the MRDviz package
   - Configures Shiny Server to serve the MRDviz application

2. A `docker-compose.yml` file that:
   - Defines the service configuration
   - Maps ports and volumes
   - Sets up container restart policy
   - Specifies the image name as `mrdviz:latest` (By default, Docker Compose would name it `[project_directory]_[service_name]:latest`, which could result in names like `mrdviz_mrdviz:latest`)

When the container starts, it launches Shiny Server, which serves the MRDviz application at the root URL path.

## Security Considerations

### Running with Non-Root User (Recommended)

For enhanced security, especially in production environments, you can run MRDviz without root privileges. This approach follows the principle of least privilege and reduces potential security risks.

#### Option 1: Using Docker's --user flag

The simplest approach is to use Docker's built-in user mapping:

```bash
# Create a local user directory for logs and data
mkdir -p ./mrdviz-data/{logs,bookmarks}

# Run with a non-root user (using your current user ID)
docker run -d --name mrdviz \
  --user $(id -u):$(id -g) \
  -p 3838:3838 \
  -v ./mrdviz-data/logs:/var/log/shiny-server \
  -v ./mrdviz-data/bookmarks:/var/lib/shiny-server/bookmarks \
  mrdviz
```

#### Option 2: Custom Non-Root Dockerfile

For more control, create a custom Dockerfile that runs as a non-root user:

```dockerfile
# Dockerfile.nonroot
FROM rocker/shiny:latest

# Create a non-root user
RUN groupadd -r mrdviz && useradd -r -g mrdviz -u 1001 mrdviz

# Install system dependencies (as root - unavoidable during build)
RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libssl-dev \
    libxml2-dev \
    libv8-dev \
    libsodium-dev \
    libcairo2-dev \
    libxt-dev \
    libglpk-dev \
    && rm -rf /var/lib/apt/lists/*

# Install R packages
RUN R -e "install.packages(c(\
    'shiny', 'shinyjs', 'highcharter', 'memoise', 'survival', \
    'jsonlite', 'DT', 'RColorBrewer', 'shinydashboard', 'ggplot2', \
    'tidyr', 'purrr', 'simsurv', 'data.table', 'dplyr', \
    'openxlsx', 'survminer', 'devtools' \
))"

# Create user directories
RUN mkdir -p /home/mrdviz/{app,logs,bookmarks} \
    && chown -R mrdviz:mrdviz /home/mrdviz

# Copy and install MRDviz package
COPY . /tmp/MRDviz
RUN R -e "devtools::install('/tmp/MRDviz', upgrade = 'never')"

# Create app file
RUN echo "library(MRDviz); run_mrdviz()" > /home/mrdviz/app/app.R \
    && chown mrdviz:mrdviz /home/mrdviz/app/app.R

# Create non-root shiny-server config
RUN echo 'run_as mrdviz;\n\
server {\n\
  listen 3838;\n\
  log_dir /home/mrdviz/logs;\n\
  app_init_timeout 180;\n\
  app_idle_timeout 600;\n\
  location / {\n\
    app_dir /home/mrdviz/app;\n\
    log_dir /home/mrdviz/logs;\n\
    bookmark_state_dir /home/mrdviz/bookmarks;\n\
  }\n\
}' > /home/mrdviz/shiny-server.conf \
    && chown mrdviz:mrdviz /home/mrdviz/shiny-server.conf

# Switch to non-root user
USER mrdviz
WORKDIR /home/mrdviz

EXPOSE 3838
CMD ["shiny-server", "--config-file", "/home/mrdviz/shiny-server.conf"]
```

Build and run the non-root version:

```bash
# Build the non-root image
docker build -f Dockerfile.nonroot -t mrdviz:nonroot .

# Run the non-root container
docker run -d --name mrdviz-secure \
  -p 3838:3838 \
  -v ./mrdviz-data/logs:/home/mrdviz/logs \
  -v ./mrdviz-data/bookmarks:/home/mrdviz/bookmarks \
  mrdviz:nonroot
```

#### Option 3: Docker Compose with Non-Root User

Create a `docker-compose.nonroot.yml` file:

```yaml
version: '3'

services:
  mrdviz:
    build: 
      context: .
      dockerfile: Dockerfile.nonroot
    image: mrdviz:nonroot
    container_name: mrdviz-secure
    user: "1001:1001"  # Use the mrdviz user created in Dockerfile
    ports:
      - "3838:3838"
    restart: unless-stopped
    volumes:
      - ./mrdviz-data/logs:/home/mrdviz/logs
      - ./mrdviz-data/bookmarks:/home/mrdviz/bookmarks
    environment:
      - SHINY_LOG_LEVEL=INFO
```

Run with:

```bash
# Create data directories
mkdir -p ./mrdviz-data/{logs,bookmarks}

# Start the secure container
docker-compose -f docker-compose.nonroot.yml up -d
```

### Additional Security Best Practices

1. **Read-only filesystem**: Run containers with read-only root filesystem where possible
2. **Resource limits**: Always set memory and CPU limits to prevent resource exhaustion
3. **Network isolation**: Use custom Docker networks to isolate containers
4. **Regular updates**: Keep base images and dependencies updated
5. **Secrets management**: Never include sensitive data in images; use Docker secrets or environment variables

## Deploying to Production

When deploying to a production environment, consider the following:

1. **Security**: 
   - Use the non-root approaches described above
   - If the application will be publicly accessible, add authentication or run behind a reverse proxy with SSL
   - Consider using Docker secrets for sensitive configuration

2. **Resource Allocation**: Allocate appropriate CPU and memory resources:

   ```bash
   docker run -d --name mrdviz -p 3838:3838 --memory=2g --cpus=2 mrdviz
   ```

3. **Monitoring**: Set up monitoring for the container and application health.

## Troubleshooting

### Container fails to start

Check the container logs:

```bash
docker logs mrdviz
```

### Application is not accessible

- Verify the container is running: `docker ps`
- Check if the port is correctly mapped: `docker port mrdviz`
- Ensure no firewall is blocking the port

### R package installation errors

If you encounter errors related to R package installation, you might need to add more system dependencies in the Dockerfile. Check the error logs and add the required packages:

```dockerfile
RUN apt-get update && apt-get install -y <additional-package>
```

## Additional Resources

- [Rocker Project Documentation](https://www.rocker-project.org/)
- [Shiny Server Documentation](https://docs.rstudio.com/shiny-server/)
- [Docker Documentation](https://docs.docker.com/)
