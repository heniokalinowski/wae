#!/usr/bin/env Rscript

source("normalization.r")
source("main.r")

eurpln <- read.csv(file="EUR_PLN_Week1.csv",head=TRUE,sep=",")

x <- as.vector(eurpln$timestamp)
y <- as.vector(eurpln$RateBid)

print("START")
print(x)
print(min(x))
print("--------------")
print(y)
print(min(y))
print("END")

input_raw <- x
output_raw <- y
num_iter <- 100
err_fun <- function(x) return(x * x)
do_plot <- TRUE

net_norm <- run_test(input_raw, output_raw, num_iter, err_fun, do_plot)