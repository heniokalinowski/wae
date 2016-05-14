#!/usr/bin/env Rscript

normalize <- function(values)
{
  minimum <- min(values)
  values <- values - minimum
  
  maximum <- max(values)
  values <- values / maximum
  
  return(list(minimum=minimum, maximum=maximum, values=values))
}

renormalize <- function(minimum, maximum, values)
{
    values <- values - minimum
    values <- values / maximum
    
    return(values)
}

denormalize3 <- function(minimum, maximum, values)
{
  values <- values * maximum
  values <- values + minimum
  
  return(values)
}
  
denormalize <- function(minMaxValues)
{
  minimum <- minMaxValues$minimum
  maximum <- minMaxValues$maximum
  values <- minMaxValues$values
  
  return(denormalize3(minimum, maximum, values))
}
