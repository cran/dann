% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/print.R
\name{print.dann}
\alias{print.dann}
\title{Print dann model}
\usage{
\method{print}{dann}(x, ...)
}
\arguments{
\item{x}{a dann model.}

\item{...}{arguments passed to other methods.}
}
\description{
Print dann model
}
\examples{
library(dann)
library(mlbench)
library(magrittr)
library(dplyr)

set.seed(1)
train <- mlbench.circle(300, 2) \%>\%
  tibble::as_tibble()
colnames(train) <- c("X1", "X2", "Y")

model <- dann(Y ~ X1 + X2, train)
print(model)
}
