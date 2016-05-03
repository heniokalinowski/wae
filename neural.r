#!/usr/bin/env Rscript
library("stats")

printf <- function(...) cat(sprintf(...))

gen_matrix <- function(cols, rows) {
    res <- matrix(rnorm(rows * cols), ncol = cols)
    return(res)
}

gen_network <- function(input_size, hidden_size, output_size) {
    input_mat <- gen_matrix(input_size + 1, hidden_size)
    output_mat <- gen_matrix(hidden_size + 1, output_size)
    network <- list("input_mat" = input_mat, "output_mat" = output_mat)
    return(network)
}

# Funkcja aktywacji
# TODO: Inne opcje?
activation_fun <- function(x) {
    return(2 / (1 + exp(-2 * x)) - 1)
    #return(x)
}

act <- function(vec) {
    return(apply(vec, c(1, 2), activation_fun))
}

is_ok_input <- function(network, input) {
    input_cols <- dim(network$input_mat)[2]
    input_size <- dim(input)[1]
    # To +1 oznacza bias
    if(input_cols != input_size + 1) {
        printf("Invalid input matrix column number: %d != %d\n", input_cols, input_size + 1)
        printf("Input matrix must have (input vector size + 1) columns\n")
        return(FALSE)
    }

    return(TRUE)
}

num_hidden <- function(network) {
    input_rows <- nrow(network$input_mat)
    output_cols <- ncol(network$output_mat)

    if(output_cols != input_rows + 1) {
        printf("Input/output matrix rows/cols do not match: %d != %d\n", input_rows, output_cols + 1)
        printf("Output matrix must have (input matrix rows + 1) columns\n")
        return(0)
    }

    return(input_rows)
}

feed_forward <- function(network, input) {
    input <- as.matrix(input)
    if(!is_ok_input(network, input) || num_hidden(network) < 1) {
        return(0)
    }

    biased_input <- rbind(1.0, input)
    hidden_vals = network$input_mat %*% biased_input
    hidden_activated <- act(hidden_vals)

    hidden_biased <- rbind(1.0, hidden_activated)
    out_vals = network$output_mat %*% hidden_biased
    out_activated <- act(out_vals)
    return(out_activated)
}

#debug(feed_forward)
#options(error = browser)
