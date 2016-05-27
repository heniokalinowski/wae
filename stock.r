#!/usr/bin/env Rscript

source("normalization.r")
source("main.r")

eurpln <- read.csv(file="EUR_PLN_Week1.csv",head=TRUE,sep=",",colClasses=c("double", "double"))
xy <- na.omit(list(x=eurpln$time, y=eurpln$RateBid))

from = 100
to = 1000

x <- (as.vector(xy$x)[from:to])
y <- (as.vector(xy$y)[from:to]) * 10000

learning_set_indices <- sample(1:length(x), floor(length(x) * 0.7))
learning_set_indices <- learning_set_indices[order(learning_set_indices)]

learning_input_raw <- t(x[learning_set_indices])
learning_output_raw <- t(y[learning_set_indices])

test_input_raw <- t(x[-learning_set_indices])
test_output_raw <- t(y[-learning_set_indices])

num_iter <- 150
pop_size <- 100
err_fun <- function(x) return(x * x)
do_plot <- TRUE

#net_norm <- run_test(learning_input_raw, learning_output_raw, num_iter, err_fun, pop_size, do_plot)
#test_result(net_norm, test_input_raw, test_output_raw, err_fun, do_plot)

num_neighbors <- 4
knn_test(learning_input_raw, learning_output_raw, test_input_raw, test_output_raw, num_neighbors, err_fun, do_plot)
