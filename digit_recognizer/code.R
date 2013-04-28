library(nnet)
library(pixmap)

# Load up training data only if not already in memory.
if (!exists("tr")) {
    tr <- read.csv("train.csv")
    tr$label <- as.factor(tr$label)
}

# Load up test data.
load.test.data <- function() {
    td <<- read.csv("test.csv")
}

# Calculate the euclidean distance between two vectors.
euclidean.distance <- function(v1, v2) {
    sum((v1 - v2) ^ 2)
}

# Visualize a given handwritten digit.
visualize.row <- function(row) {

    if (length(row) == 785) {
        label <- paste("Handwritten digit representing", row[1], ".")
        row <- row[-1]
    } else {
        label <- paste("Handwritten digit, unknown label.")
    }

    plot.new()
    plot(pixmapGrey(row, 28, 28), main=label)
}

# Given a data frame (e.g. training data), calculate the mean average image for
# each class label (0-9).
mean.centroids <- function(df) {
    centroids <- t(sapply(levels(df$label), function(level) {
        colMeans(df[df$label == level, -1])
    }))
    centroids <- cbind(0:9, centroids)
    return(centroids)
}

centroids <- mean.centroids(tr)

# Given a matrix of centroids and a data frame, classify each row of the data
# frame to one of the ten centroids (classes 0-9).
my.predict <- function(centroids, df) {
    centroids <- centroids[,-1]

    if (ncol(df) == 785) {
        df <- df[,-1]
    }

    apply(df, 1, function(row) {
        which.min(apply(centroids, 1, euclidean.distance, row)) - 1
    })
}

# Write predictions to disk.
write.predictions <- function(predictions) {
    write.table(predictions, file="predictions.txt", quote=F, row.names=F,
                col.names=F)
}
