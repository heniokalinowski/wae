#!/usr/bin/env Rscript

source("normalization.r")
source("main.r")

eurpln <- read.csv(file="EUR_PLN_Week1.csv",head=TRUE,sep=",")
xy <- na.omit(list(x=eurpln$timestamp, y=eurpln$RateBid))

x <- t(as.vector(xy$x))
y <- t(as.vector(xy$y))

print("START")
print(min(x))
print("--------------")
print(min(y))
print("END")

input_raw <- x
output_raw <- y
num_iter <- 100
err_fun <- function(x) return(x * x)
do_plot <- TRUE

net_norm <- run_test(input_raw, output_raw, num_iter, err_fun, do_plot)
