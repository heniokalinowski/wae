#!/usr/bin/env Rscript
source("neural.r")
source("mutations.r")


input <- t(c(0.5, 1, 2, 3, 4))
network <- gen_network(1, 10, 1)
output <- feed_forward(network, input)
print(output)

print("Before")
network
added <- add_neuron(network)
print("After")
added
