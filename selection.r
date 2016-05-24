source("neural.r")
library("parallel")

get_error <- function(net, input_normalized, output_raw, output_norm_data, err_fun) {
    predicted_normal <- feed_forward(net, input_normalized)
    predicted <- denormalize3(output_norm_data$minimum, output_norm_data$maximum, predicted_normal)
    err_linear <- abs(predicted - output_raw)
    
    return(sum(apply(err_linear, c(1, 2), err_fun)))
}

rate <- function(net, input_normalized, output_raw, output_norm_data, err_fun) {
    size_penalty <- (100.0 + 2 * num_hidden(net)) / 100
    error_penalty <- get_error(net, input_normalized, output_raw, output_norm_data, err_fun)
    return(1.0 / (size_penalty * error_penalty))
}

new_pop <- function(net_set, input_normalized, output_raw, output_norm_data, err_fun) {
    probs <- mclapply(net_set, function(x) return(rate(x, input_normalized, output_raw, output_norm_data, err_fun)), mc.cores = 4)
    rval <- sample(net_set, size = length(net_set) - 1, prob = probs)
    max_index <- which.max(probs)
    min_index <- which.min(probs)
    net_set[min_index] <- list(gen_network(1, sample(2:10, 1), 1))
    rval <- c(net_set[max_index], rval)
    return(rval)
}

#debug(new_pop)
