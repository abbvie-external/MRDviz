% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{is_categorical}
\alias{is_categorical}
\title{Check Categorical Variable}
\usage{
is_categorical(x, max_unique = 10)
}
\arguments{
\item{x}{Vector to check}

\item{max_unique}{Maximum number of unique values for categorical variables}
}
\value{
Logical indicating whether x should be treated as categorical
}
\description{
Checks if a variable should be treated as categorical
}
\keyword{internal}
