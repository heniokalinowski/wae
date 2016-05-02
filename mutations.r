library("stats")
source("neural.r")

add_neuron <- function(network) {
    nhidden <- num_hidden(network)
    noutput <- ncol(network$output_mat)
    index <- sample(1:nhidden, 1)

    pre_hidden <- network$input_mat[1:index,]
    new_hidden <- rnorm(ncol(network$input_mat))
    if(index < nhidden) {
        post_hidden <- network$input_mat[(index + 1):nhidden,]
    } else {
        post_hidden <- NULL
    }

    network$input_mat <- rbind(pre_hidden, new_hidden, post_hidden)

    pre_output <- network$output_mat[,1:index]
    new_output <- rnorm(nrow(network$output_mat))
    if(index < noutput) {
        post_output <- network$output_mat[,(index + 1):noutput]
    } else {
        post_output <- NULL
    }

    network$output_mat <- cbind(t(pre_output), t(new_output), t(post_output))
    return(network)
}
