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

    pre_output <- t(network$output_mat[,1:index])
    new_output <- t(rnorm(nrow(network$output_mat)))
    if(index < noutput) {
        post_output <- t(network$output_mat[,(index + 1):noutput])
    } else {
        post_output <- NULL
    }

    network$output_mat <- cbind(pre_output, new_output, post_output)
    return(network)
}

remove_neuron <- function(network) {
    nhidden <- num_hidden(network)
    if(nhidden < 3) {
        return(network)
    }
    noutput <- ncol(network$output_mat)
    index <- sample(1:nhidden, 1)

    if(index > 1) {
        pre_hidden <- network$input_mat[1:(index - 1),]
    } else {
        pre_hidden <- NULL
    }
    if(index < nhidden) {
        post_hidden <- network$input_mat[(index + 1):nhidden,]
    } else {
        post_hidden <- NULL
    }

    network$input_mat <- rbind(pre_hidden, post_hidden)

    if(index > 1) {
        pre_output <- t(network$output_mat[,1:(index - 1)])
    } else {
        pre_output <- NULL
    }
    if(index < noutput) {
        post_output <- t(network$output_mat[,(index + 1):noutput])
    } else {
        post_output <- NULL
    }

    network$output_mat <- cbind(pre_output, post_output)
    return(network)
}

randomize_input_weight <- function(network) {
    dm <- dim(network$input_mat)
    in_rows <- dm[1]
    in_cols <- dm[2]
    index_c <- sample(1:in_cols, 1)
    index_r <- sample(1:in_rows, 1)
    network$input_mat[index_r, index_c] <- rnorm(1)
    return(network)
}

crossover <- function(network) {
}

maybe_mutate <- function(network) {
    return(sample(c(add_neuron, remove_neuron, randomize_input_weight), 1)[[1]](network))
}

mutate_set <- function(net_set) {
    return(lapply(net_set, FUN = maybe_mutate))
}
