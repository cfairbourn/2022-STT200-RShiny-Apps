#theme for plots
plaintheme <- theme_bw() + 
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank() ) +
  theme(axis.line.x = element_line(color="black", size = 1),
        axis.line.y = element_line(color="black", size = 1))+
  theme(legend.position = "none")

#axis theme for plots
axistheme <- theme(plot.title = element_text(hjust = 0.5, color = "black", face = "bold", size=20)) +
  theme(axis.title = element_text(color = "black", size = 16)) +
  theme(axis.text.x = element_text(size = 14, color = "black")) +
  theme(axis.text.y = element_text(size = 14, color = "black"))

c.inner <- "snow4" # dotplot dot fill color
c.outer <- "#F6491A" # color for cutoff values
c.select <- "grey10" # color for selected sample 

#determines the number of decimal places of a number
# decimalcount<-function(x){
#   stopifnot(class(x)=="character")
#   x<-gsub("(.*)(\\.)|([0]*$)","",x)
#   as.numeric(nchar(x))
# }

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
  xVals <- vector()
  yVals <- vector()
  x.locs <- as.numeric(names(frequencies))

  pointDist <- min(diff(x.locs))/(n+2)
  x.coord <- sapply(x.locs, function(x) x + ((1:n.cols)-(n.cols+1)/2)*pointDist)

  for(i in 1:length(frequencies)) {

    if (n == 1){
      xVals <- c(xVals, rep(x.coord[i], frequencies[i]/n.cols))
    } else {
      xVals <- c(xVals, rep(x.coord[, i], frequencies[i]/n.cols),
                 x.coord[0:(frequencies[i] %% n.cols), i])
    }



    if (frequencies[i] > n.cols){
      yVals <- c(yVals, sort(rep(1:(frequencies[i]/n.cols), n.cols)),
                    rep(ceiling(frequencies[i]/n.cols), frequencies[i] %% n.cols))
    } else {
      yVals <- c(yVals, sort(rep(1:(frequencies[i]/n.cols), frequencies[i])))
    }

  }

  data

  data <- data %>%
    select(!xline)
  data['xVals'] = xVals
  data['yVals'] = yVals * n.cols
  
  return(data)
}
# 
# ScottDotPlot <- function(data, n.cols, bound, mn) {
#   # Scott's code, not sure what it's doing
#   n <- n.cols
#   counts <- table(data)
#   x.locs <- as.numeric(names(counts))
#   
#   # distance between points in one column if n > 1
#   #   
#   x.coords <- vector()
#   y.coords <- vector()
#   to.color <- vector()
#   names.counts <- as.numeric(names(counts))
#   
#   num.decimals <- decimalcount(as.character(bound))
#   # error term for rounded cutoff values
#   error <- ifelse(num.decimals <= 2, 0, 0.1^num.decimals/2)
#   
#   # loop through counts table
#   for (i in 1:length(counts)){ 
#     if (n == 1){
#       x.coords <- c(x.coords, rep(x.coord[i], counts[i]/n))
#     } else {
#       
#     }
#     
#     if (counts[i] > n){
#       
#     } else {
#       
#     }
#     
#     dist <- abs(mn - (abs(as.numeric(bound))-error))
#     if ((names.counts[i] <= mn - dist) |
#         (names.counts[i] >= mn + dist)){
#       to.color <- c(to.color, rep(c.outer, counts[i]))
#     } else {
#       to.color <- c(to.color, rep(c.inner, counts[i]))
#     }
#   }
#   
#   df <- data.frame("x" = x.coords, "y" = y.coords*n,
#                    "fill.color" = to.color, 
#                    stringAsFactors = TRUE) # <- Mandatory
#   
#   return(df)
# }

# sampleToDF <- function(s, n) {
#   f <- n-s
#   
#   cols <- max(1,ceiling(n^(0.5)/3))
#   
#   n.sDots <- as.integer(s / cols)
#   n.fDots <- as.integer(f / cols)
#   n.sRmdr <- s %% cols
#   n.fRmdr <- f %% cols
#   
#   x.vals <- vector()
#   y.vals <- vector()
#   colors <- vector()
#   
#   for (i in 1:cols) {
#     dots <- n.fDots
#     
#     if (n.fRmdr > 0) {
#       n.fRmdr = n.fRmdr - 1
#       dots <- n.fDots + 1
#     }
#     
#     x.col <- vector("list", dots)
#     x.col <- lapply(x.col, function(x) x = i)
#     
#     y.col <- seq(from = cols, to = (dots*cols), by = abs(cols))
#     
#     c.val <- vector("list", dots)
#     c.val <- lapply(c.val, function(x) x = "grey100")
#     
#     x.vals <- c(x.vals, x.col)
#     y.vals <- c(y.vals, y.col)
#     colors <- c(colors, c.val)
#   }
#   
#   for (i in -cols:-1) {
#     dots <- n.sDots
#     if (n.sRmdr > 0) {
#       n.sRmdr = n.sRmdr - 1
#       dots <- n.sDots + 1
#     }
#     x.col <- vector("list", dots)
#     x.col <- lapply(x.col, function(x) x = i)
#     
#     y.col <- seq(from = cols, to = (dots*cols), by = abs(cols))
#     
#     c.val <- vector("list", dots)
#     c.val <- lapply(c.val, function(x) x = "darkgreen")
#     
#     x.vals <- c(x.vals, x.col)
#     y.vals <- c(y.vals, y.col)
#     colors <- c(colors, c.val)
#   }
#   
#   x.vals <- as.numeric(unlist(x.vals))
#   colors <- as.factor(unlist(colors))
#   manual.colors <- factor(colors, levels = c("darkgreen","grey100"))
#   
#   if (all(length(x.vals) == length(y.vals),
#           length(y.vals) == length(colors))) {
#     
#     df <- data.frame(
#       "x" = x.vals,
#       "y" = y.vals,
#       "c" = colors,
#       stringsAsFactors = TRUE)
#     
#     ggplot(df, aes(x, y)) +
#       geom_point(aes(fill = as.factor(c),
#                      size = max(0.1, round(cols^(1/3)/7, 3))),
#                  pch = 21, color = "grey10") +
#       scale_fill_manual(
#         name = "", 
#         values = levels(manual.colors), 
#         labels = c(paste("Yes =", s),paste("No =", f))) +
#       theme(
#         # remove x and y axis in their entirety
#         axis.title.x = element_blank(),
#         axis.text.x = element_blank(),
#         axis.ticks.x = element_blank(),
#         axis.title.y = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks.y = element_blank()) +
#       theme(
#         # make background more bland, add border, top side
#         panel.border = element_rect(colour = "black", fill=NA, size=1.5),
#         plot.background = element_blank(),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank()) +
#       theme(
#         # make the Yes No legend larger
#         legend.text = element_text(size = 20)) +
#       guides(fill = guide_legend(override.aes = list(size=5)))+
#       guides(size = FALSE, color = FALSE)
#   }
# }