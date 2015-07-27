date.format <- function(x) {
    new.date <- as.Date(x, "%d/%m/%Y")
    return(new.date)
}