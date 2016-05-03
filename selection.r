source("neural.r")

get_error <- function(net, input, output) {
    predicted <- feed_forward(net, input)
    err_linear <- abs(predicted - output)
    #return(sum(err_linear))
    return(sum(apply(err_linear, c(1, 2), function(x) return(x * x))))
}

rate <- function(net, input, output) {
    size_penalty <- 1.0 / (100.0 + 1 * num_hidden(net))
    error_penalty <- get_error(net, input, output)
    return(1.0 / (size_penalty + error_penalty))
}

new_pop <- function(net_set, input, output) {
    probs <- lapply(net_set, function(x) return(rate(x, input, output)))
    rval <- sample(net_set, size = length(net_set) - 1, prob = probs)
    max_index <- which.max(probs)
    rval <- c(net_set[max_index], rval)
    return(rval)
}
