#!/usr/bin/env Rscript
source("neural.r")
source("mutations.r")
source("selection.r")
source("normalization.r")

run_test <- function(input_raw, output_raw, tested_fun, num_iter, err_fun, do_plot) {
    input_norm_data <- normalize(input_raw)
    input <- input_norm_data$values

    output_norm_data <- normalize(output_raw)
    output <- output_norm_data$values

    net_set <- lapply(1:100, function(x) return(gen_network(1, 3, 1)))
    first <- net_set[[1]]

    if(missing(do_plot)) {
        do_plot <- TRUE
    }

    if(do_plot) {
        plot(input_raw, output_raw, type="l", col="green")
        par(new=TRUE)
        predicted_normalized <- feed_forward(first, input)
        predicted <- denormalize3(output_norm_data$minimum, output_norm_data$maximum, predicted_normalized)
        plot(input_raw, predicted, type="l", col="red", xlab="", ylab="")
    }

    if(missing(err_fun)) {
        err_fun <- function(x) return(x * x)
    }

    if(missing(num_iter)) {
        num_iter <- 100
    }

    for(iter in 1:num_iter) {
    #if(FALSE){
        net_set <- new_pop(net_set, input, output_raw, output_norm_data, err_fun)
        # Zachowujemy najlepszego
        best_from_set <- net_set[[1]]
        # Nie mutujemy najlepszego
        net_set <- mutate_set(net_set[2:length(net_set)])
        # Dodajemy go z powrotem
        net_set[[length(net_set) + 1]] <- best_from_set
        best_output <- feed_forward(best_from_set, input)
        printf("Iter %d, best error: %.3f\n", iter, get_error(best_from_set, input, output_raw, output_norm_data, err_fun))
        predicted_normalized <- feed_forward(best_from_set, input)
        predicted <- denormalize3(output_norm_data$minimum, output_norm_data$maximum, predicted_normalized)

        if(do_plot) {
            xlim <- c(min(input_raw), max(input_raw))
            ylim <- c(min(output_raw, predicted), max(output_raw, predicted))
            plot(input_raw, output_raw, xlim=xlim, ylim=ylim, type="l", col="green")
            par(new=TRUE)
            plot(input_raw, predicted, xlim=xlim, ylim=ylim, type="l", col="red", xlab="", ylab="")
        }
    }

    return(best_from_set)
}

input_raw <- t(seq(2,5,length=20))
output_raw <- apply(input_raw, 2, tested_fun)
tested_fun <- function(x) return(x*x*x/exp(x))
num_iter <- 100
err_fun <- function(x) return(x * x)

run_test(input_raw, output_raw, tested_fun, num_iter, err_fun, do_plot=TRUE)
