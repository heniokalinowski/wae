#!/usr/bin/env Rscript

source("normalization.r")
source("main.r")

eurpln <- read.csv(file="EUR_PLN_Week1.csv",head=TRUE,sep=",",colClasses=c("double", "double"))
xy <- na.omit(list(x=eurpln$time, y=eurpln$RateBid))

from = 100
to = 1000

x <- t(as.vector(xy$x)[from:to])
y <- t(as.vector(xy$y)[from:to]) * 10000

print("START")
print(x[1:3])
print(min(x))
print(typeof(x))
print("--------------")
print(y[1:3])
print(min(y))
print(typeof(y))
print("END")

input_raw <- x
output_raw <- y
num_iter <- 150
pop_size <- 100
err_fun <- function(x) return(x * x)
do_plot <- TRUE

net_norm <- run_test(input_raw, output_raw, num_iter, err_fun, pop_size, do_plot)
