
########################
# Paired T Test Source #
########################

library(openintro)
library(ggplot2)
library(tidyverse)

UCLA_Books <- ucla_textbooks_f18 %>%
  select(bookstore_new, amazon_new) %>%
  drop_na()

# presets
Presets <- list("Custom","Upload","UCLA_Books","mtcars","iris","chickwts","births14")
DataPresets <- list(UCLA_Books, mtcars, iris, chickwts, as.data.frame(births14))
DataStrings <- Presets[3:length(Presets)]

# custom data integration
X <- rep(0,10)
Y <- rep(0,10)
X <- data.table(X)
Y <- data.table(Y)

# theme for plots
plaintheme <- theme_bw() + 
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank() ) +
  theme(axis.line.x = element_line(color="black", size = 1),
        axis.line.y = element_line(color="black", size = 1))+
  theme(legend.position = "none")

# axis theme for plots
axistheme <- theme(plot.title = element_text(
  hjust = 0.5, color = "black", face = "bold", size=20)) +
  theme(axis.title = element_text(color = "black", size = 16)) +
  theme(axis.text.x = element_text(size = 14, color = "black")) +
  theme(axis.text.y = element_text(size = 14, color = "black"))

c_inner <- "snow4" # dotplot dot fill color
c_outer <- "red3" # color for cutoff values
c_select <- "grey10" # color for selected sample 

Paired_CI <- function(x,y) {
  diff <- x-y
  new_mean <- mean(sample(diff, replace = T))
  return (new_mean)
}

Paired_T_Test <- function(x, y) {
  x1 <- 0
  x2 <- 0
  for (i in 1:length(x)) {
    pair <- sample(c(x[i], y[i]), replace = F)
    x1 <- x1 + pair[1]
    x2 <- x2 + pair[2]
  }
  diff <- (x1 - x2)/length(x)
  return (diff)
}

ColinDotPlot <- function(data, n.bins, n.cols) {
  
  # data is data frame containing 2 columns, means and color for confidence level
  # n.bins is an integer that is the number of bins of dots in the plot
  # n.cols is an integer that is the number of columns of dots per bin
  # sort the table by it's data from least to greatest
  data <- data %>%
    arrange(samples)
  
  # calculate the binwidth, this is number of bins in the plot
  xMean <- mean(data[,1])
  xSD <- sd(data[,1])
  line_distance <- xSD # points are on the line or around the line
  
  # positions of the lines as vector
  # find maximum number of standard deviations away from the mean data has
  # calculate zscore for each sample mean, calculate the max of ceil of abs
  n <- data %>%
    mutate(zscore = ceiling(abs((samples - xMean)/xSD))) %>%
    summarise(n = max(zscore))
  n = n[[1]]
  
  # calculate the lower and upper bound of the area of data
  lower = xMean - (n*xSD)
  upper = xMean + (n*xSD)
  
  # calculate the boundaries for each
  lines <- seq(lower, upper, length.out = n.bins+1)[-1]
  lineDist <- lines[2]-lines[1]
  xCenterLines <- signif((lines - 0.5*(lineDist)), 4)
  
  data <- data %>%
    mutate(xline = 0)
  
  # determine the bin each point is in in the sample data
  for(i in length(lines):1) {
    data <- data %>%
      mutate(xline = ifelse(
        samples <= lines[i],
        xCenterLines[i], data$xline))
  }
  
  xBase <- data %>%
    select(xline)
  frequencies <- table(xBase)
  
  # phantom.cols <- 2*(floor(n.cols/2)+1)
  x_vals <- vector()
  y_vals <- vector()
  x.locs <- as.numeric(names(frequencies))
  
  pointDist <- min(diff(x.locs))/(n+2)
  x.coord <- sapply(x.locs, function(x) x + ((1:n.cols)-(n.cols+1)/2)*pointDist)
  
  for(i in 1:length(frequencies)) {
    
    if (n == 1){
      x_vals <- c(x_vals, rep(x.coord[i], frequencies[i]/n.cols))
    } else {
      x_vals <- c(x_vals, rep(x.coord[, i], frequencies[i]/n.cols),
                  x.coord[0:(frequencies[i] %% n.cols), i])
    }
    
    
    
    if (frequencies[i] > n.cols){
      y_vals <- c(y_vals, sort(rep(1:(frequencies[i]/n.cols), n.cols)),
                  rep(ceiling(frequencies[i]/n.cols), frequencies[i] %% n.cols))
    } else {
      y_vals <- c(y_vals, sort(rep(1:(frequencies[i]/n.cols), frequencies[i])))
    }
    
  }
  
  data
  
  data <- data %>%
    select(!xline)
  data['x_vals'] = x_vals
  data['y_vals'] = y_vals * n.cols
  
  return(data)
}