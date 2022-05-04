fit_learning_and_intercept <- function(p,t,y,ret,measure,print_output){

        # Define the function:

        
        if (measure[1] == 'mouse_dist_euclid'){
                y_hat <- p[1] * (exp(-p[2]*(t-1)))
                
        } else {
                y_hat <- p[1] + (1 - exp(-p[2]*(t-1)))*(1-p[1])      
                
        }
        
        
        sse <- sum((y-y_hat)^2)

        if (print_output){
                print(paste0('params: ',p))
                print(paste0('measure: ',measure[1]))
                print(paste0(y,collapse = ' '))
                print(paste0(y_hat,collapse = ' '))
                # print(paste0('sse: ',sse))
        }

                
        if (ret=='fit') {
                return(y_hat)
        } else {
                return(sse)
        } 
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