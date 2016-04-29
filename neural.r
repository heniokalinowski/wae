#!/usr/bin/env Rscript
library("stats")

printf <- function(...) cat(sprintf(...))

gen_matrix <- function(cols, rows) {
    res <- matrix(0, rows, cols)
    res <- apply(res, c(1, 2), function(x) return(rnorm(1)))
    return(res)
}

# Funkcja aktywacji
# TODO: Inne opcje?
activation_fun <- function(x) {
    return(2 / (1 + exp(-2 * x)) - 1)
}

act <- function(vec) {
    return(apply(vec, c(1, 2), activation_fun))
}

is_ok <- function(input_mat, output_mat, input) {
    input_rows <- dim(input_mat)[1]
    input_cols <- dim(input_mat)[2]
    output_cols <- dim(output_mat)[2]
    input_size <- dim(input)[1]

    # To +1 oznacza bias
    if(input_cols != input_size + 1) {
        printf("Invalid input matrix column number: %d != %d\n", input_cols, input_size + 1)
        printf("Input matrix must have (input vector size + 1) columns\n")
        return(FALSE)
    }
    if(output_cols != input_rows + 1) {
        printf("Input/output matrix rows/cols do not match: %d != %d\n", input_rows, output_cols + 1)
        printf("Output matrix must have (input matrix rows + 1) columns\n")
        return(FALSE)
    }

    return(TRUE)
}

feed_forward <- function(input_mat, output_mat, input) {
    if(!is_ok(input_mat, output_mat, input)) {
        #return(0)
    }

    biased_input <- rbind(1.0, input)
    hidden_vals = input_mat %*% biased_input
    hidden_activated <- act(hidden_vals)

    hidden_biased <- rbind(1.0, hidden_activated)
    out_vals = output_mat %*% hidden_biased
    out_activated <- act(out_vals)
    return(out_activated)
}

#debug(feed_forward)
#options(error = browser)
