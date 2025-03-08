# Dockerized MRDviz

This document provides instructions for running the MRDviz application in a Docker container. Dockerizing MRDviz makes it easy to deploy the application in various environments without worrying about R package dependencies or system configuration.

## Prerequisites

- [Docker](https://docs.docker.com/get-docker/) installed on your system
- [Docker Compose](https://docs.docker.com/compose/install/) (optional, but recommended)

## Quick Start

The simplest way to run the dockerized MRDviz application is using Docker Compose:

```bash
# Clone the repository (if you haven't already)
git clone https://pig.abbvienet.com/choikx3/MRDviz.git
cd MRDviz

# Build and start the container
docker-compose up -d

# To stop the container
docker-compose down
```

Once the container is running, you can access the MRDviz application at:
http://localhost:3838

## Manual Docker Setup

If you prefer not to use Docker Compose, you can build and run the container manually:

```bash
# Build the Docker image
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

## Deploying to Production

When deploying to a production environment, consider the following:

1. **Security**: If the application will be publicly accessible, consider adding authentication or running it behind a reverse proxy with SSL.

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