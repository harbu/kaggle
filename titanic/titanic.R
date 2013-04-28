library(e1071)
library(imputation)
library(nnet)
library(tree)
library(randomForest)
library(caret)

# Impute missing numerical values of given data frame.
do.imputation <- function(df) {
    indices <- names(df) %in% c("name", "sex", "ticket", "cabin", "embarked")
    new.cols <- impute(df[ , !indices])
    df[ , !indices] <- new.cols
    return(df)
}

# Calculate prediction accuracy.
calculate.prediction.accuracy <- function(fit) {
    sum(predict(fit, tr) == tr$survived) / nrow(tr)
}

# Write test data predictions to disk with given learned model.
write.predictions.to.disk <- function(fit) {
    write.table(unname(predict(fit, test.data)),
                file="predictions.txt",
                quote=F, row.names=F, col.names=F)
}

# Graphically plot some basic statistics of the training data.
basic.stats <- function(df) {

    # Initialize graphical output.
    plot.new()
    par(ask=T)

    # Plot effect of passenger class on survival rate.
    frequency.barplot(df, "pclass")
    par(ask=T)

    # Plot effect of sex on survival rate.
    frequency.barplot(df, "sex")
    par(ask=T)
}

frequency.barplot <- function(df, lbl) {
    lvls <- levels(df[[lbl]])

    ys <- sapply(lvls, function(lvl) {
        condition <- tr[lbl] == lvl
        sum(tr[condition, ]$survived == 1) / sum(condition)
    })

    bp <- barplot(ys, ylim=c(0,1),
                  main=paste("Survival frequency by passenger", lbl),
                  ylab="Percentage survived",
                  xlab=paste("Passenger", lbl),
                  names.arg=lvls)

    text(bp, y=ys, label=format(ys), po=3)
}

# Read in training data and test data, do some manipulations.
tr <- do.imputation(read.csv("train.csv"))
non.imputed.tr <- read.csv("train.csv")
test.data <- do.imputation(read.csv("test.csv"))
non.imputed.test.data <- read.csv("test.csv")

tr$survived <- as.factor(tr$survived)
tr$pclass <- as.factor(tr$pclass)

test.data$pclass <- as.factor(test.data$pclass)
levels(test.data$embarked) <- c("", levels(test.data$embarked))

# Example of predicting using a Naive Bayes classifier.
fit <- naiveBayes(survived ~ pclass + sex + age, tr)
confusionMatrix(predict(fit, tr), tr$survived)

