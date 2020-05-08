library(magrittr)
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(grid)
library(gridExtra)

data <- read.csv("data/dataset.csv")

print(paste("Number of rows:", nrow(data)))
print(paste("Number of columns:", ncol(data)))

head(data)

tail(data)

str(data)

sapply(data, class)

sapply(data, typeof)

colSums(is.na(data))

trainData <- select(data, -c(domain, label))
exploringData <- select(data, -c(domain))
print(paste("The whole dataset:", length(data)))
print(paste("Total features:", length(trainData)))
print(paste("Total exploring data:", length(exploringData)))

library(ggplot2)

summary(exploringData)

plot <- function (data, column) {
    ggplot(exploringData, aes_string(x=column)) +
    geom_bar() + geom_bar(fill = "steelblue", color ="steelblue") +
  theme_minimal() + scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) + scale_x_continuous(labels = function(x) format(x, scientific = FALSE))

}

plots <- lapply(colnames(exploringData), plot, data=exploringData)
#plots

grid.arrange(grobs=plots, ncol=2, as.table=FALSE)

options(repr.plot.width=10, repr.plot.height=15)

dist_plots <- function (data, column) {
    ggplot(data, aes_string(x=column, y=column)) + 
    geom_boxplot(fill = "steelblue", color ="steelblue") +
    guides(fill=FALSE) + scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) + scale_x_continuous(labels = function(x) format(x, scientific = FALSE))


}

num_plots <- lapply(colnames(exploringData), dist_plots, data=exploringData)

grid.arrange(grobs=num_plots, ncol=1, as.table=FALSE)

trainRowCount <- floor(0.8 * nrow(trainData))
set.seed(1234)

trainIndex <- sample(1:nrow(trainData), trainRowCount)
train <- trainData[trainIndex,]
test <- trainData[-trainIndex,]

print(paste("Shape of training set:", toString(dim(train))))
print(paste("Shape of test set:", toString(dim(test))))

library(randomForest)

features <- c("ranking", "isIp", "valid", "activeDuration", "urlLen", "is.", "isredirect", "haveDash", "domainLen", "nosOfSubdomain")

rf_model <- randomForest(trainData[features], trainData$label)

#summary(rf_model)

predictions_rf <- predict(rf_model, test[features])
