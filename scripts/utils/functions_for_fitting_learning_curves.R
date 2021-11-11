fit_learning_and_intercept <- function(p,t,y,ret){

        # Define the function:
        #y_hat <- p[1]*(1-exp(-p[2]*(t-1)))
        y_hat <- p[1] + (1 - exp(-p[2]*(t-1)))*(1-p[1])
        
        sse <- sum((y-y_hat)^2)
        
#        return(list("sse"=sse,"fit"=y_hat)) # doesn't work with "optim"
        if (ret=='fit') {return(y_hat)} 
        else {return(sse)}
}


fit_learning_only <- function(p,t,y){
        # p: parameter to estimate. Just the learning rate. 
        # Here the asymptote is assumed to be 1
        
        # Asymptote
        a <- 1
        
        
        # Given the above redefined and simplified formula: y_hat = a*(1-e^(-c*t))
        y_hat <- 1-exp(-p*(t-1))
        sse <- sum((y-y_hat)^2)
        return(sse)
}