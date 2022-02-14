## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----exampleP1----------------------------------------------------------------
 library(dann)
 library(mlbench)
 library(magrittr)
 library(dplyr, warn.conflicts = FALSE)
 library(ggplot2)

 ######################
 # Circle data with unrelated variables
 ######################
 set.seed(1)
 train <- mlbench.circle(500, 2) %>%
   tibble::as_tibble()
 colnames(train)[1:3] <- c("X1", "X2", "Y")
 train <- train %>%
  mutate(Y = as.numeric(Y))

 # Add 5 unrelated variables
 train <- train %>%
   mutate(
     U1 = runif(500, -1, 1),
     U2 = runif(500, -1, 1),
     U3 = runif(500, -1, 1),
     U4 = runif(500, -1, 1),
     U5 = runif(500, -1, 1)
   )
 
 test <- mlbench.circle(500, 2) %>%
   tibble::as_tibble()
 colnames(test)[1:3] <- c("X1", "X2", "Y")
 test <- test %>%
  mutate(Y = as.numeric(Y))

 # Add 5 unrelated variables
 test <- test %>%
   mutate(
     U1 = runif(500, -1, 1),
     U2 = runif(500, -1, 1),
     U3 = runif(500, -1, 1),
     U4 = runif(500, -1, 1),
     U5 = runif(500, -1, 1)
   )
 

## ----dann---------------------------------------------------------------------
 dannPreds <- dann_df(
  formula = Y~X1 + X2 + U1 + U2 + U3 + U4 + U5,
  train = train, test = test,
  k = 3, neighborhood_size = 50, epsilon = 1, probability = FALSE)
 mean(dannPreds == test$Y)

## ----graph--------------------------------------------------------------------
 graph_eigenvalues_df(formula = Y~X1 + X2 + U1 + U2 + U3 + U4 + U5, train = train, 
                   neighborhood_size = 50, weighted = FALSE, sphere = "mcd")

## ----subDann------------------------------------------------------------------
 subDannPreds <- sub_dann_df(formula = Y~X1 + X2 + U1 + U2 + U3 + U4 + U5, 
                             train = train, test = test,
                             k = 3, neighborhood_size = 50, epsilon = 1, 
                             probability = FALSE, 
                             weighted = FALSE, sphere = "mcd", numDim = 2)
 mean(subDannPreds == test$Y)

## ----dann2--------------------------------------------------------------------
 variableSelectionDann <- dann_df(formula = Y~X1 + X2, 
                               train = train, test = test,
                               k = 3, neighborhood_size = 50, epsilon = 1, probability = FALSE)
 
 mean(variableSelectionDann == test$Y)

