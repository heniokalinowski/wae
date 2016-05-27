#!/usr/bin/env Rscript
library('FNN')

knn_test <- function(learning_input, learning_output, test_input, test_output, num_neighbors, err_fun, do_plot) {
    train <- t(learning_input)
    test <- t(test_input)
    cl <- t(learning_output)

    minx <- min(learning_input, test_input)
    maxx <- max(learning_input, test_input)
    miny <- min(learning_output, test_output)
    maxy <- max(learning_output, test_output)

    train <- (train - minx) / (maxx - minx)
    test <- (test - minx) / (maxx - minx)
    cl <- (cl - miny) / (maxy - miny)

    knn_result <- knn(train, test, cl, k = num_neighbors, algorithm="cover_tree")
    predicted_indices <- attr(knn_result, "nn.index")
    predicted_dists <- attr(knn_result, "nn.dist")
    dist_sums <- rowSums(predicted_dists)
    neighbor_weights <- t(t(predicted_dists / dist_sums))
    neighbor_values <- apply(predicted_indices, c(1, 2), function(ind) learning_output[ind])
    weighted_neighbors <- neighbor_values * neighbor_weights
    predicted <- rowSums(weighted_neighbors)

    err_linear <- abs(as.vector(predicted) - as.vector(test_output))
    total_error <- sum(sapply(err_linear, err_fun))

    printf("KNN, error total: %.3f average: %.6f\n", total_error, total_error / ncol(learning_input))

    if(missing(do_plot)) {
        do_plot <- TRUE
    }

    if(do_plot) {
        xlim <- c(min(learning_input, test_input), max(learning_input, test_input))
        ylim <- c(min(test_output, predicted), max(test_output, predicted))
        par(new=FALSE)
        plot(test_input, test_output, xlim=xlim, ylim=ylim, type="l", col="green")
        par(new=TRUE)
        plot(test_input, predicted, xlim=xlim, ylim=ylim, type="l", col="red", xlab="", ylab="")
    }

    return(predicted)
}
