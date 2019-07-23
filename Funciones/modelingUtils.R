#######################################################################################################
#### Packages ####
#######################################################################################################
# install.packages("ggplot2")
# install.packages("plotly")
# install.packages("reshape2")

require(ggplot2)
require(plotly)
require(reshape2)

#######################################################################################################
#### Modeling functions ####
#######################################################################################################

MAPE <- function(real, predicted) {
  mean(abs((real - predicted)/real))
}

SORT_CORRELATION <- function(x, y) {
  index <- order(cor(x, y), decreasing = TRUE)
  return(x[,index])
}

STEP_STEP_LM <- function(x, y) {
  for(i in 1:ncol(x)) {
    print(paste("... ", i, " regression ..."))
    fit <- lm(y~x[,1:i])
    print(summary(fit))
    print(paste("MAPE: ", round(MAPE(y, predict(fit)), digits = 4)))
    print("...")
    print("...")
  }
  return(fit)
}

CONTRIBUTION_DF <- function(dataFrame, modelObject) {
  newData <- data.frame(Base = 1, dataFrame)
  newData <- t(t(newData) * modelObject$coefficients)
  return(newData)
}

ATTRIBUTION_DF <- function(dataFrame, modelObjet) {
  newData <- CONTRIBUTION_DF(dataFrame, modelObjet)
  total <- sum(newData)
  attribution <- newData / total
  return(attribution)
}

ATTRIBUTION <- function(dataFrame, modelObjet) {
  newData <- ATTRIBUTION_DF(dataFrame, modelObjet)
  attribution <- colSums(newData)
  return(attribution)
}

MELT_ATTRIBUTION_DATA <- function(dataFrame, modelObject) {
  newData <- ATTRIBUTION(dataFrame, modelObject)
  newData <- melt(newData)
  newData$variable <- row.names(newData)
  newData <- newData[order(newData$value, decreasing = TRUE),]
  return(newData)
}

LOG_LOG_REGRESSION <- function(x, y) {
  x.var <- x
  y.var <- y
  
  x.var[x.var == 0] <- 1e-320
  
  x.var <- cbind(log(x.var))
  y.var <- cbind(log(y.var))
  
  log.log.model <- lm(y.var ~ x.var)
  
  return(log.log.model)
}

LOG_LOG_PREDICT <- function(loglogModelObject, newData = NULL) {
  return(exp(predict(loglogModelObject, newData)))
}
#######################################################################################################
#### Plot functions ####
#######################################################################################################

ACTUAL_DATA <- function(var, dates = NULL) {
  if(is.null(dates) == TRUE) {
    newData <- data.frame(var)
    newData$Index <- as.numeric(row.names(newData))
    newData <- newData[,2:1]
  } else {
    newData <- data.frame(Date = dates, var)
  }
  return(newData)
}

PREDICTED_DATA <- function(modelObject, newDataFrame = NULL, dates = NULL, logModel = FALSE) {
  if(logModel == FALSE) {
    if(is.null(newDataFrame) == TRUE) {
      prediction <- predict(modelObject)
    } else {
      prediction <- predict(modelObject, newDataFrame)
    }
    newData <- ACTUAL_DATA(prediction, dates)
    if(is.null(dates) == TRUE) {
      names(newData) <- c("Index", "Predict")
    } else {
      names(newData) <- c("Date", "Predict")
    }
    
  } else {
    
    if(is.null(newDataFrame) == TRUE) {
      prediction <- LOG_LOG_PREDICT(modelObject)
    } else {
      prediction <- LOG_LOG_PREDICT(modelObject, newDataFrame)
    }
    newData <- ACTUAL_DATA(prediction, dates)
    if(is.null(dates) == TRUE) {
      names(newData) <- c("Index", "Predict")
    } else {
      names(newData) <- c("Date", "Predict")
    }
  }
  
  return(newData)
}

ACTUAL_PREDICTED_DATA <- function(real, modelObject, newDataFrame = NULL, dates = NULL, logModel = FALSE) {
  actualData <- ACTUAL_DATA(real, dates)
  predictedData <- PREDICTED_DATA(modelObject, newDataFrame, dates, logModel)
  
  if(is.null(dates) == TRUE) {
    newData <- merge(actualData, predictedData, by = "Index")
    newData <- melt(newData, id.vars = "Index")
  } else {
    newData <- merge(actualData, predictedData, by = "Date")
    newData <- melt(newData, id.vars = "Date")
  }
  
  return(newData)
}


ACTUAL_PREDICTED_PLOT <- function(real, modelObject, dates = NULL, logModel = FALSE) {
  
  rSquared <- round(summary(modelObject)$adj.r.squared, digits = 4)
  
  if(logModel == FALSE) {
    mape <- round(MAPE(real, predict(modelObject)), digits = 4)
  } else {
    mape <- round(MAPE(real, LOG_LOG_PREDICT(modelObject)), digits = 4)
  }
  
  a <- list()
  
  if(is.null(dates) == TRUE) {
    
    newData <- ACTUAL_PREDICTED_DATA(real, modelObject, logModel = logModel)
    
    a[[1]] <- list(text = paste("R squared = ", rSquared),
                   x = min(newData$Index),
                   y = max(newData$value),
                   showarrow = FALSE)
    a[[2]] <- list(text = paste("MAPE = ", mape),
                   x = max(newData$Index),
                   y = max(newData$value),
                   showarrow = FALSE)
    return(
      plot_ly(
        data = newData,
        x = Index,
        y = value,
        group = variable) %>%
        layout(title = "Actual vs Model") %>%
        layout(annotations = a)
    )
    
  } else {
    newData <- ACTUAL_PREDICTED_DATA(real, modelObject, dates = dates, logModel = logModel)
    
    a[[1]] <- list(text = paste("R squared = ", rSquared),
                   x = min(newData$Date),
                   y = max(newData$value),
                   showarrow = FALSE)
    a[[2]] <- list(text = paste("MAPE = ", mape),
                   x = max(newData$Date),
                   y = max(newData$value),
                   showarrow = FALSE)

    return(
      plot_ly(
        data = newData,
        x = Date,
        y = value,
        group = variable) %>%
        layout(title = "Actual vs Model") %>%
        layout(annotations = a)
    )
  }
}

TOTAL_ATTRIBUTION_PLOT <- function(var.df, modelObject) {
  return(
    plot_ly(
      data = MELT_ATTRIBUTION_DATA(var.df, modelObject),
      x = variable,
      y = value,
      type = "bar",
      color = variable
    ) %>%
      layout(title = "Total Attribution")
  )
}

CONTRIBUTION_PERCENT_DATA <- function(dataFrame, modelObject) {
  newData <- data.frame(CONTRIBUTION_DF(dataFrame, modelObject))
  newData <- newData / predict(modelObject)
  
  return(newData)
}

CONTRIBUTION_FINAL_DATA <- function(percentData, total) {
  newData <- percentData * total
  
  return(newData)
}

STACKED_DATA <- function(dataFrame) {
  newData <- dataFrame
  
  for(i in 2:ncol(newData)) {
    newData[,i] <- newData[,i] + newData[,i-1]
  }
  
  return(newData)
}


CONTRIBUTION_PLOT <- function(dataFrame, modelObject, total = NULL, dates = NULL) {
  newData <- CONTRIBUTION_PERCENT_DATA(dataFrame, modelObject)
  
  if(is.null(total) == TRUE) {
    newData <- CONTRIBUTION_FINAL_DATA(newData, predict(modelObject))
  } else {
    newData <- CONTRIBUTION_FINAL_DATA(newData, total)
  }
  
  stackedData <- STACKED_DATA(newData)
  
  if(is.null(dates) == TRUE) {
    Index <- 1:nrow(stackedData)
    plt <- plot_ly(x = Index, y = stackedData[,1],
                   fill = "tonexty", mode = "lines", text = newData[,1],
                   name = names(stackedData)[1], hoverinfo = "x+text+name")
    for(i in 2:ncol(stackedData)) {
      print(plt)
      plt <- add_trace(x = Index, y = stackedData[,i],
                       fill = "tonexty", mode = "lines", text = newData[,i],
                       name = names(stackedData)[i], hoverinfo = "x+text+name")
    }
  } else {
    Date <- dates
    plt <- plot_ly(x = Date, y = stackedData[,1],
                   fill = "tonexty", mode = "lines", text = newData[,1],
                   name = names(stackedData)[1], hoverinfo = "x+text+name")
    for(i in 2:ncol(stackedData)) {
      print(plt)
      plt <- add_trace(x = Date, y = stackedData[,i],
                       fill = "tonexty", mode = "lines", text = newData[,i],
                       name = names(stackedData)[i], hoverinfo = "x+text+name")
    }
  }
  
  return(plt)
}



#######################################################################################################
#### Author: Sebastian Morales Peralta ####
#######################################################################################################

#######################################################################################################
#### Examples ####
#######################################################################################################

###
# Linear
###
X1 <- rnorm(n = 100, mean = 120, sd = 10)
X2 <- rnorm(n = 100, mean = 80, sd = 15) 
X3 <- rnorm(n = 100, mean = 200, sd = 30)

Y <- (.4 * X1) + (.6 * X2) + (1.2 * X3) + 50

X1 <- X1 + rnorm(n = 100, mean = 0, sd = 1)
X2 <- X2 + rnorm(n = 100, mean = 0, sd = 1)
X3 <- X3 + rnorm(n = 100, mean = 0, sd = 1)

X <- cbind(X1, X2, X3)
Y <- cbind(Y)

lm.model <- lm(Y~X)

summary(lm.model)

MAPE(Y, predict(lm.model))


lm.model <- STEP_STEP_LM(SORT_CORRELATION(X, Y), Y)
ATTRIBUTION(SORT_CORRELATION(X,Y), lm.model)

lm.model <- STEP_STEP_LM(X, Y)
lm.standarized.model <- STEP_STEP_LM(scale(Y), scale(X))

ATTRIBUTION(X, lm.model)


dates <- seq.Date(as.Date("2015-01-01"), by = 7, length.out = 100)

ggplotly(
  ggplot(ACTUAL_DATA(Y), aes(x = Index, y = Y)) + geom_line()
)

ACTUAL_PREDICTED_PLOT(Y, lm.model, dates = dates)

TOTAL_ATTRIBUTION_PLOT(X, lm.model)

CONTRIBUTION_PLOT(X, lm.model)

df <- ATTRIBUTION_DF(X, lm.model)
df <- data.frame(df)
dfstack <- STACKED_DATA(df)
dfstack <- data.frame(dfstack)

pexp <- plot_ly(x = dates, y = dfstack[,1], fill= "tonexty", mode = "lines",
        text = df[,1], name=names(dfstack)[1], hoverinfo = "x+text+name") 

pexp <-  add_trace(p = pexp,x = dates, y = dfstack[,2], fill= "tonexty", mode = "lines",
                  text = df[,2], name=names(dfstack)[2], hoverinfo = "x+text+name")
pexp <- add_trace(p = pexp, x = dates, y = dfstack[,3], fill= "tonexty", mode = "lines",
                  text = df[,3], name=names(dfstack)[3], hoverinfo = "x+text+name")


pexp



df$Date <- dates
df <- melt(df, id.vars = "Date")


ggplotly(
  ggplot(data = df, aes(x = Date, y = value)) +
    geom_area(aes(fill = variable), position = "stack") +
    ggtitle("Contribution") +
    theme_minimal()
)

plot_ly(
  data = df,
  x = Date,
  y = Base,
  fill = "tonexty",
  mode = "lines",
  text = df$Base,
  name = "Base",
  hoverinfo = "x+text+name") %>%
  add_trace(data = df, x = Date, y = df[,2], fill = "tonexty",
            mode = "lines",
            text = df[,2],
            name = names(df)[,2],
            hoverinfo = "x+text+name") %>%
  add_trace(data = df, x = Date, df[,3], fill = "tonexty",
            mode = "lines",
            text = df[,3],
            name = "X2",
            hoverinfo = "x+text+name")



###
# log log
###

X1 <- rpois(200, 12)
X2 <- rpois(200, 120)
X3 <- rpois(200, 300)

Y <- exp((.3 * log(X1)) + (.25 * log(X2)) + (.37 * log(X3)) + 4)

X1 <- X1 + rnorm(200)
X2 <- X2 + rnorm(200)
X3 <- X3 + rnorm(200)

Y <- cbind(Y)
X <- cbind(X1, X2, X3)


loglogmodel <- LOG_LOG_REGRESSION(X, Y)
summary(loglogmodel)


ACTUAL_PREDICTED_PLOT(Y, loglogmodel, logModel = TRUE)

TOTAL_ATTRIBUTION_PLOT(log(X), loglogmodel)


plot((1e-320:(10*max(X1)))*150, exp(loglogmodel$coefficients[2] * log((1e-320:(10*max(X1)))))*2000, type = "l")

abline(v = 5000)
abline(h = 5000)