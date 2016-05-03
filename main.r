#!/usr/bin/env Rscript
source("neural.r")
source("mutations.r")
source("selection.r")

options(error = recover)

tested_fun <- function(x) return(sin(x))

input <- t(seq(0,pi,length=20))
#output <- feed_forward(network, input)
output <- apply(input, 2, tested_fun)
#print(output)

net_set <- lapply(1:100, function(x) return(gen_network(1, 10, 1)))
first <- net_set[[1]]

plot(input, output, type="l", col="green")
par(new=TRUE)
predicted <- feed_forward(first, input)
plot(input, predicted, type="l", col="red", xlab="", ylab="")

for(iter in 1:1000) {
#if(FALSE){
    net_set <- new_pop(net_set, input, output)
    # Zachowujemy najlepszego
    best_from_set <- net_set[[1]]
    # Nie mutujemy najlepszego
    net_set <- mutate_set(net_set[2:length(net_set)])
    # Dodajemy go z powrotem
    net_set[[length(net_set) + 1]] <- best_from_set
    best_output <- feed_forward(best_from_set, input)
    printf("Iter %d, best error: %.3f\n", iter, get_error(best_from_set, input, output))
    plot(input, output, type="l", col="green")
    par(new=TRUE)
    predicted <- feed_forward(best_from_set, input)
    plot(input, predicted, type="l", col="red", xlab="", ylab="")
    #print(best_output)
    
}
