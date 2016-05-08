#!/usr/bin/env Rscript

source("normalization.r")

startingVector <- c(1, 2, 3, 4)
normalized <- normalize(startingVector)
denormalized <- denormalize(normalized)

if(!all(startingVector == denormalized)) 
{
  warning("normalization test failed")
} 