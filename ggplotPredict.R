
require(ggplot2)

# Plot function
ggplot.predict <- function(df) {
  p <- ggplot(df, aes(x = date, y = number, group = setting))
  p + geom_line(aes(colour = setting, linetype = setting))
}


# EXAMPLE
date <- seq.Date(from = as.Date("2016-01-01"), to = as.Date("2016-01-31"), by = 1)
number <- rnorm(31)
setting <- c(rep("current", 15), rep("model", 16))

example <- data.frame(date, number, setting)

date <- seq.Date(from = as.Date("2016-01-16"), to = as.Date("2016-01-31"), by = 1)
number <- rnorm(16)
setting <- c(rep("model2", 16))
example2 <- data.frame(date, number, setting)

example <- rbind(example, example2)


ggplot.predict(example)
