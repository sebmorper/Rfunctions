##----------------------------------DECAY---------------------------------------

decay <- function(x, factor) {
    if(factor >= 0 & factor <= 1) {
        for(i in 2:length(x)) {
            x[i] <- (x[i-1] * factor) + x[i]
        }
        return (x)
    } else {
        return ("Error: The factor of decay need to be between 0 an 1")
    }    
    
}

ejemplo <- c(100,0,0,0,0,0,0,0,0,0,0,0)
decay(ejemplo, .2)

plot(ejemplo, type = "l")
plot(decay(ejemplo, .3), type = "l")
