library("stats")
source("neural.r")

add_neuron <- function(network) {
    max_index <- num_hidden(network)
    index <- sample(1:max_index, 1)
    len <- nrow(network$input_mat)
    network$input_mat <- rbind(network$input_mat[1:index,], rnorm(len), network$input_mat[index:len,])
    network$output_mat <- cbind(network$output_mat[,1:index], rnorm(len + 1), network$output_mat[,index:len+1])
    return(network)
}
