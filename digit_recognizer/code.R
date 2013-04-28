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

euclidean.distance <- function(v1, v2) {
    sum((v1 - v2) ^ 2)
}

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

mean.centroids <- function(df) {
    centroids <- t(sapply(levels(df$label), function(level) {
        colMeans(df[df$label == level, -1])
    }))
    centroids <- cbind(0:9, centroids)
    return(centroids)
}

centroids <- mean.centroids(tr)

my.predict <- function(centroids, rows) {
    centroids <- centroids[,-1]

    if (ncol(rows) == 785) {
        rows <- rows[,-1]
    }

    apply(rows, 1, function(row) {
        which.min(apply(centroids, 1, euclidean.distance, row)) - 1
    })
}

write.predictions <- function(predictions) {
    write.table(predictions, file="predictions.txt", quote=F, row.names=F,
                col.names=F)
}
