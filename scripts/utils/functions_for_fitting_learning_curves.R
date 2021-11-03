fit_learning_and_asymptote <- function(p,t,y){
        # Description:
        # p: parameters. Vector of size 2, p[1] is the asymptote and p[2] is the learning rate
        # t: trials, usually seq(1,8)
        # y: data
        
        # Using the general formula: y_hat = a - (1-intercept)*e^(-c*t)
        
        # The intercept is assumed to start at 0.
        # But in this formula, if you change "a" the intercept changes too! 
        # The only way of making sure the intercept stays at 0 is to redefine 
        # (1-intercept) as being the same as "a".
        # Then we get: y_hat = a - a * e^(-c*t)
        # Simplified further: y_hat = a*(1 - e^(-c*t))
        
        # Define the function:
        y_hat <- p[1]*(1-exp(-p[2]*(t-1)))
        
        sse <- sum((y-y_hat)^2)
        
        return(sse)
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