% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sub_dann.R
\name{sub_dann}
\alias{sub_dann}
\title{Discriminant Adaptive Nearest Neighbor With Subspace Reduction}
\usage{
sub_dann(
  xTrain,
  yTrain,
  xTest,
  k = 5,
  neighborhood_size = max(floor(nrow(xTrain)/5), 50),
  epsilon = 1,
  probability = FALSE,
  weighted = FALSE,
  sphere = "mcd",
  numDim = ncol(xTrain)/2
)
}
\arguments{
\item{xTrain}{Train features. Something easily converted to a numeric matrix.
Generally columns should have mean zero and standard deviation one beforehand.}

\item{yTrain}{Train classes. Something easily converted to a numeric vector.}

\item{xTest}{Test features. Something easily converted to a numeric matrix.
Generally columns should be centered and scaled according to xTrain beforehand.}

\item{k}{The number of data points used for final classification.}

\item{neighborhood_size}{The number of data points used to calculate between and within class covariance.}

\item{epsilon}{Diagonal elements of a diagonal matrix. 1 is the identity matrix.}

\item{probability}{Should probabilities instead of classes be returned?}

\item{weighted}{weighted argument to ncoord. See \code{\link[fpc]{ncoord}} for details.}

\item{sphere}{sphere argument to ncoord. See \code{\link[fpc]{ncoord}} for details.}

\item{numDim}{Dimension of subspace used by dann. See \code{\link[fpc]{ncoord}} for details.}
}
\value{
A numeric vector containing predicted class or a numeric matrix containing class probabilities.
}
\description{
Discriminant Adaptive Nearest Neighbor With Subspace Reduction
}
\details{
This is an implementation of Hastie and Tibshirani's sub-dann in section 4.1 of
\href{https://web.stanford.edu/~hastie/Papers/dann_IEEE.pdf}{Discriminant Adaptive Nearest
Neighbor Classification publication.}. It uses package fpc's ncoord to find the subspace. Then calls
dann.

dann's performance suffers when noise variables are included in the model. Simulations show sub_dann
will generally be more performant in this scenario. However there is no replacement for good feature
selection.
}
\examples{
library(dann)
library(mlbench)
library(magrittr)
library(dplyr)
library(ggplot2)

######################
# Circle data with unrelated variables
######################
set.seed(1)
train <- mlbench.circle(300, 2) \%>\%
  tibble::as_tibble()
colnames(train)[1:3] <- c("X1", "X2", "Y")

# Add 5 unrelated variables
train <- train \%>\%
  mutate(
    U1 = runif(300, -1, 1),
    U2 = runif(300, -1, 1),
    U3 = runif(300, -1, 1),
    U4 = runif(300, -1, 1),
    U5 = runif(300, -1, 1)
  )

xTrain <- train \%>\%
  select(X1, X2, U1, U2, U3, U4, U5) \%>\%
  as.matrix()

yTrain <- train \%>\%
  pull(Y) \%>\%
  as.numeric() \%>\%
  as.vector()

test <- mlbench.circle(100, 2) \%>\%
  tibble::as_tibble()
colnames(test)[1:3] <- c("X1", "X2", "Y")

# Add 5 unrelated variables
test <- test \%>\%
  mutate(
    U1 = runif(100, -1, 1),
    U2 = runif(100, -1, 1),
    U3 = runif(100, -1, 1),
    U4 = runif(100, -1, 1),
    U5 = runif(100, -1, 1)
  )

xTest <- test \%>\%
  select(X1, X2, U1, U2, U3, U4, U5) \%>\%
  as.matrix()

yTest <- test \%>\%
  pull(Y) \%>\%
  as.numeric() \%>\%
  as.vector()

dannPreds <- dann(
  xTrain = xTrain, yTrain = yTrain, xTest = xTest,
  k = 3, neighborhood_size = 50, epsilon = 1,
  probability = FALSE
)
mean(dannPreds == yTest) # Not a good model

# Data suggests a subspace with 2 dimentions. The correct answer.
graph_eigenvalues(
  xTrain = xTrain, yTrain = yTrain, neighborhood_size = 50,
  weighted = FALSE, sphere = "mcd"
)

subDannPreds <- sub_dann(
  xTrain = xTrain, yTrain = yTrain, xTest = xTest,
  k = 3, neighborhood_size = 50, epsilon = 1,
  probability = FALSE,
  weighted = FALSE, sphere = "classical", numDim = 2
)
# sub_dan does much better when unrelated variables are present.
mean(subDannPreds == yTest)

rm(train, test)
rm(xTrain, yTrain)
rm(xTest, yTest)
rm(dannPreds, subDannPreds)
}
