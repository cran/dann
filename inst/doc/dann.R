## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----simData------------------------------------------------------------------
library(dann)
library(dplyr, warn.conflicts = FALSE)
library(ggplot2)
library(mlbench)

set.seed(1)
train <- mlbench.2dnormals(600, cl = 6, r = sqrt(2), sd = .5) %>%
  tibble::as_tibble()
colnames(train) <- c("X1", "X2", "Y")
train <- train %>%
  mutate(Y = as.numeric(Y))

ggplot(train, aes(x = X1, y = X2, colour = as.factor(Y))) + 
  geom_point() + 
  labs(title = "Train Data", colour = "Y")


test <- mlbench.2dnormals(600, cl = 6, r = sqrt(2), sd = .5) %>%
  tibble::as_tibble()
colnames(test) <- c("X1", "X2", "Y")
test <- test %>%
  mutate(Y = as.numeric(Y))

ggplot(test, aes(x = X1, y = X2, colour = as.factor(Y))) + 
  geom_point() + 
  labs(title = "Test Data", colour = "Y")

## ----model--------------------------------------------------------------------
dannPreds <- dann_df(formula = Y ~ X1 + X2, train = train, test = test,
                  k = 7, neighborhood_size = 150, epsilon = 1)
round(mean(dannPreds == test$Y), 2)

