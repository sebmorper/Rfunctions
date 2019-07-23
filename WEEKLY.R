## ---------------------------- WEEK FUNCTION ---------------------------------

weekly.fun <- function(week, date) {
    week.out <- data.frame(week, 0)
    
    for(i in seq_along(date[,1])) {
        for(j in seq_along(week)) {
            if(date[i, 1] >= week[j] & date[i, 1] < week[j+1]) {
                week.out[j,2] <- week.out[j,2] + date[i,2]
            } else {
                week.out[j,2] <- week.out[j,2]
            }
        }
    }
    return(week.out)
}


a <- c("01/01/2015", "07/01/2015","14/01/2015", "21/01/2015", "28/01/2015")
b <- c("31/12/2014", "01/01/2015", "02/01/2015", "05/01/2015", "25/01/2015", "02/02/2015")
a <- as.Date(a, "%d/%m/%Y")
b <- as.Date(b, "%d/%m/%Y")
c <- c(rep(1, 5), 0)
d <- data.frame(b,c)

weekly.fun(a, d)
