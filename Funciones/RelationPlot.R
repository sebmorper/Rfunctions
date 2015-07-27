##----------------------------RELATION PLOT---------------------------------------
# a scater plot of the relation between two variables
# the linear model between this relationship
# the Adjusted R-Squared coefficient

data(mtcars)
head(mtcars)

relation.plot <- function(x, y) {
    require(ggplot2)
    require(stats)
    qplot(x, y) +
        stat_smooth(method="lm", se = FALSE) +
        annotate("text",
                 x = quantile(x, .75),
                 y = max(y),
                 label = paste("R.Squared = ",
                               round(summary(lm(y~x))$adj.r.squared, 2)))
}

relation.plot(mtcars$wt, mtcars$mpg)
