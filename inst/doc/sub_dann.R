## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----exampleP1-----------------------------------------------------------
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

 # Add 5 unrelated variables
 train <- train %>%
   mutate(
     U1 = runif(500, -1, 1),
     U2 = runif(500, -1, 1),
     U3 = runif(500, -1, 1),
     U4 = runif(500, -1, 1),
     U5 = runif(500, -1, 1)
   )

 xTrain <- train %>%
   select(X1, X2, U1, U2, U3, U4, U5) %>%
   as.matrix()

 yTrain <- train %>%
   pull(Y) %>%
   as.numeric() %>%
   as.vector()

 test <- mlbench.circle(500, 2) %>%
   tibble::as_tibble()
 colnames(test)[1:3] <- c("X1", "X2", "Y")

 # Add 5 unrelated variables
 test <- test %>%
   mutate(
     U1 = runif(500, -1, 1),
     U2 = runif(500, -1, 1),
     U3 = runif(500, -1, 1),
     U4 = runif(500, -1, 1),
     U5 = runif(500, -1, 1)
   )

 xTest <- test %>%
   select(X1, X2, U1, U2, U3, U4, U5) %>%
   as.matrix()

 yTest <- test %>%
   pull(Y) %>%
   as.numeric() %>%
   as.vector()
 
 dannPreds <- dann(xTrain, yTrain, xTest, 3, 50, 1, FALSE)
 mean(dannPreds == yTest) # Not a good model

## ----exampleP2-----------------------------------------------------------
 
 graph_eigenvalues(xTrain, yTrain, 50, FALSE, "mcd")
 subDannPreds <- sub_dann(xTrain, yTrain, xTest, 3, 50,
                          1, FALSE, FALSE,
                          "mcd", 2)
 mean(subDannPreds == yTest) # sub_dan does much better when unrelated variables are present.

## ----exampleP3-----------------------------------------------------------
 variableSelectionDann <- dann(xTrain[, 1:2], yTrain, xTest[, 1:2], 3, 50, 1, FALSE)
 
 mean(variableSelectionDann == yTest) # Best model found when only true predictors are used.

## ----exampleP4-----------------------------------------------------------
 rm(train, test)
 rm(xTrain, yTrain)
 rm(xTest, yTest)
 rm(dannPreds, subDannPreds, variableSelectionDann)

