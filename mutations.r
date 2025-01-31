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

randomize_output_weight <- function(network) {
    dm <- dim(network$output_mat)
    out_rows <- dm[1]
    out_cols <- dm[2]
    index_c <- sample(1:out_cols, 1)
    index_r <- sample(1:out_rows, 1)
    network$output_mat[index_r, index_c] <- rnorm(1)
    return(network)
}

no_mutation <- function(network) {
    return(network)
}

crossover_factor <- 2

crossover_input_weight <- function(left, right) {
    dm <- dim(left$input_mat)
    in_rows <- dm[1]
    in_cols <- dm[2]
    index_c <- sample(1:in_cols, 1)
    index_r <- sample(1:in_rows, 1)
    left$input_mat[index_r, index_c] <- crossover_factor * left$input_mat[index_r, index_c] - right$input_mat[index_r, index_c]
    return(left)
}

crossover_output_weight <- function(left, right) {
    dm <- dim(left$output_mat)
    out_rows <- dm[1]
    out_cols <- dm[2]
    index_c <- sample(1:out_cols, 1)
    index_r <- sample(1:out_rows, 1)
    left$output_mat[index_r, index_c] <- crossover_factor * left$output_mat[index_r, index_c] - right$output_mat[index_r, index_c]
    return(left)
}

crossover_all <- function(left, right) {
    left$output_mat <- crossover_factor * left$output_mat - right$output_mat
    left$input_mat <- crossover_factor * left$input_mat - right$input_mat
    return(left)
}

crossover <- function(left, right) {
    return(sample(c(crossover_input_weight, crossover_output_weight, crossover_all), 1)[[1]](left, right))
}

mutate <- function(network) {
    return(sample(c(add_neuron, remove_neuron, randomize_input_weight, randomize_output_weight, no_mutation), 1)[[1]](network))
}

mutate_set <- function(net_set, best) {
    crossover_size <- length(net_set) * 0.5
    cross_set <- sample(1:length(net_set), crossover_size)
    for(set_index in cross_set) {
        #other <- sample(net_set, 1)[[1]]
        #if(num_hidden(set) == num_hidden(other)) {
        
        if(num_hidden(net_set[[set_index]]) == num_hidden(best)) {
            net_set[[set_index]] <- crossover(best, net_set[[set_index]])
        }
    }

    return(lapply(net_set, FUN = mutate))
}
