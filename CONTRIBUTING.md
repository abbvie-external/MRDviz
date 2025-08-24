# Contributing to MRDviz

Thank you for your interest in contributing to MRDviz! This document provides guidelines for contributing to the project. MRDviz is an R package that provides a Shiny application for visualizing and simulating Minimal Residual Disease (MRD) data in clinical trials.

## Table of Contents

- [Code of Conduct](#code-of-conduct)
- [Getting Started](#getting-started)
- [Development Setup](#development-setup)
- [Contributing Workflow](#contributing-workflow)
- [Coding Standards](#coding-standards)
- [Testing](#testing)
- [Documentation](#documentation)
- [Submitting Changes](#submitting-changes)
- [Issue Reporting](#issue-reporting)
- [Feature Requests](#feature-requests)

## Code of Conduct

By participating in this project, you agree to maintain a respectful and inclusive environment for all contributors. Please be professional, constructive, and considerate in all interactions.

## Getting Started

### Prerequisites

Before contributing, ensure you have:

- R (>= 3.5.0)
- RStudio (recommended)
- Git
- GitHub account

### Fork and Clone

1. Fork the repository on GitHub
2. Clone your fork locally:
   ```bash
   git clone https://github.com/YOUR-USERNAME/MRDviz.git
   cd MRDviz
   ```
3. Add the upstream repository:
   ```bash
   git remote add upstream https://github.com/abbvie-external/MRDviz.git
   ```

## Development Setup

### Install Dependencies

1. Open the project in RStudio or your preferred R environment
2. Install development dependencies:
   ```R
   # Install required packages for development
   install.packages(c("devtools", "testthat", "roxygen2", "pkgdown"))
   
   # Install package dependencies
   devtools::install_deps(dependencies = TRUE)
   ```

3. If using `renv` (recommended):
   ```R
   renv::restore()
   ```

### Verify Installation

Test that the package loads and runs correctly:
```R
devtools::load_all()
run_mrdviz()
```

### Docker Development (Optional)

For containerized development:
```bash
docker-compose up -d
```

See [README.docker.md](README.docker.md) for detailed Docker instructions.

## Contributing Workflow

### 1. Create a Branch

Always create a new branch for your work:
```bash
git checkout -b feature/your-feature-name
# or
git checkout -b bugfix/issue-number-description
```

### 2. Make Changes

- Keep commits focused and atomic
- Write clear, descriptive commit messages
- Follow the coding standards outlined below

### 3. Test Your Changes

Before submitting:
```R
# Load and test the package
devtools::load_all()
devtools::test()
devtools::check()
```

### 4. Update Documentation

If you've added or modified functions:
```R
# Update documentation
devtools::document()
```

## Coding Standards

### R Code Style

- Follow the [tidyverse style guide](https://style.tidyverse.org/)
- Use meaningful variable and function names
- Include roxygen2 documentation for all exported functions
- Use `snake_case` for function and variable names
- Maximum line length: 80 characters

### Function Documentation

All exported functions must include roxygen2 documentation:

```R
#' Brief description of the function
#'
#' Longer description if needed, explaining what the function does,
#' its purpose, and any important details.
#'
#' @param param_name Description of the parameter
#' @param another_param Description of another parameter
#' @return Description of what the function returns
#' @export
#' @examples
#' \dontrun{
#' example_usage()
#' }
my_function <- function(param_name, another_param) {
  # Function implementation
}
```

### Shiny Code

- Organize UI and server logic clearly
- Use reactive programming principles appropriately
- Include input validation and error handling
- Follow Shiny best practices for performance

## Testing

### Writing Tests

- Write tests for all new functions using `testthat`
- Place tests in `tests/testthat/`
- Use descriptive test names
- Test both expected behavior and edge cases

Example test structure:
```R
test_that("function_name works correctly", {
  # Test normal case
  result <- my_function(valid_input)
  expect_equal(result, expected_output)
  
  # Test edge case
  expect_error(my_function(invalid_input))
})
```

### Running Tests

```R
# Run all tests
devtools::test()

# Run specific test file
devtools::test_file("tests/testthat/test-my-function.R")

# Check test coverage
covr::package_coverage()
```

## Documentation

### Types of Documentation

1. **Function Documentation**: Roxygen2 comments for all exported functions
2. **Vignettes**: For complex workflows or tutorials
3. **README Updates**: If adding major features
4. **NEWS.md**: Document changes for releases

### Building Documentation

```R
# Generate documentation
devtools::document()

# Build and preview pkgdown site
pkgdown::build_site()
```

## Submitting Changes

### Pull Request Process

1. **Sync with upstream**:
   ```bash
   git fetch upstream
   git checkout main
   git merge upstream/main
   ```

2. **Rebase your branch**:
   ```bash
   git checkout your-branch-name
   git rebase main
   ```

3. **Push to your fork**:
   ```bash
   git push origin your-branch-name
   ```

4. **Create Pull Request**:
   - Go to GitHub and create a pull request
   - Use a clear, descriptive title
   - Fill out the pull request template
   - Link any related issues

### Pull Request Guidelines

- **Title**: Use a clear, descriptive title
- **Description**: Explain what changes you made and why
- **Testing**: Describe how you tested your changes
- **Documentation**: Confirm documentation is updated
- **Breaking Changes**: Clearly note any breaking changes

### Pull Request Template

```markdown
## Description
Brief description of changes

## Type of Change
- [ ] Bug fix
- [ ] New feature
- [ ] Documentation update
- [ ] Performance improvement
- [ ] Other (please describe)

## Testing
- [ ] Tests pass locally
- [ ] Added tests for new functionality
- [ ] Manual testing completed

## Documentation
- [ ] Updated function documentation
- [ ] Updated README if needed
- [ ] Added/updated examples

## Checklist
- [ ] Code follows project style guidelines
- [ ] Self-review completed
- [ ] No breaking changes (or clearly documented)
```

## Issue Reporting

### Before Reporting

1. Check existing issues to avoid duplicates
2. Try to reproduce the issue with the latest version
3. Gather relevant information (R version, package version, etc.)

### Issue Template

When reporting bugs, include:

- **R Version**: Output of `R.version.string`
- **Package Version**: Output of `packageVersion("MRDviz")`
- **Operating System**: Your OS and version
- **Expected Behavior**: What you expected to happen
- **Actual Behavior**: What actually happened
- **Reproducible Example**: Minimal code to reproduce the issue
- **Error Messages**: Full error messages if any

## Feature Requests

When requesting new features:

1. **Check existing issues** for similar requests
2. **Describe the use case** clearly
3. **Explain the benefit** to users
4. **Consider implementation** complexity
5. **Provide examples** of desired functionality

## Development Tips

### Useful Commands

```R
# Load package for testing
devtools::load_all()

# Check package
devtools::check()

# Install package locally
devtools::install()

# Run Shiny app in development
run_mrdviz()

# Update documentation
devtools::document()

# Run tests
devtools::test()
```

### Debugging Shiny Apps

- Use `browser()` for interactive debugging
- Check browser console for JavaScript errors
- Use `reactlog` package for reactive debugging
- Test with different browsers and screen sizes

### Performance Considerations

- Profile code with `profvis` package
- Use `memoise` for expensive computations
- Consider data.table for large datasets
- Optimize reactive expressions

## Getting Help

- **Documentation**: Check function documentation and vignettes
- **Issues**: Search existing GitHub issues
- **Discussions**: Use GitHub Discussions for questions
- **Contact**: Reach out to maintainers for complex questions

## Recognition

Contributors will be acknowledged in:
- Package DESCRIPTION file (for significant contributions)
- Release notes
- Project documentation

Thank you for contributing to MRDviz! Your contributions help improve MRD data analysis for the research community.
