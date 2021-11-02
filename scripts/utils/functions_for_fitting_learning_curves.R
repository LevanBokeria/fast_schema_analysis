fit_learning_and_asymptote <- function(p,t,y){
        # print(p)
        
        y_hat <- p[1]*(1-exp(-p[2]*(t-1)))
        sse <- sum((y-y_hat)^2)
        return(sse)
}

fit_learning_only <- function(p,t,y){
        # print(p)
        
        y_hat <- 1-exp(-p*(t-1))
        sse <- sum((y-y_hat)^2)
        return(sse)
}