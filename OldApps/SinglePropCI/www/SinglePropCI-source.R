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

c.inner <- "grey70" # dotplot dot fill color
c.outer <- "#F6491A" # color for cutoff values
c.select <- "grey10" # color for selected sample 

#determines the number of decimal places of a number
# decimalcount<-function(x){
#   stopifnot(class(x)=="character")
#   x<-gsub("(.*)(\\.)|([0]*$)","",x)
#   as.numeric(nchar(x))
# }



SimpleDotPlot <- function(data) {
  
  data <- data %>%
    arrange(samples)
  
  x.vals <- vector()
  y.vals <- vector()
  
  frequency <- table(data$samples)
  values <- as.numeric(names(frequency))
  
  for(i in 1:length(frequency)) {
    n <- frequency[[i]]
    x.i <- rep(values[i], n)
    y.i <- c(1:n)
    
    x.vals <- c(x.vals, x.i)
    y.vals <- c(y.vals, y.i)
  }
  
  data['x.vals'] <- x.vals
  data['y.vals'] <- y.vals
  
  return(data)
  
}


OGSampPlotMaker = function(counts, samp.size) {
  
  x1 <- rnorm(counts, 0, 1)
  y1 <- rnorm(counts, 0, 1)
  c1 <- rep("darkgreen", counts)
  
  f <- samp.size - counts
  x2 <- rnorm(f, 0, 1)
  y2 <- rnorm(f, 0, 1)
  c2 <- rep("grey10", f)
  
  x <- c(x1,x2)
  y <- c(y1,y2)
  c <- c(c1,c2)
  
  manual.colors <- factor(c, levels = c("darkgreen","grey100"))
  df <- data.frame(x,y,c)
  df <- df[sample(1:nrow(df)),]
  
  ggplot(df, aes(x, y)) +
    geom_point(aes(fill = c,
                   size = 0.3),
               pch = 21, color = "grey10") +
    scale_fill_manual(
      name = "", 
      values = levels(manual.colors), 
      labels = c(paste("Yes =", counts),paste("No =", f))) +
    theme(
      # remove x and y axis in their entirety
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()) +
    theme(
      # make background more bland, add border, top side
      panel.border = element_rect(colour = "black", fill=NA, size=1.5),
      plot.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()) +
    theme(
      # make the Yes No legend larger
      legend.text = element_text(size = 20)) +
    guides(fill = guide_legend(override.aes = list(size=5)))+
    guides(size = "none", color = "none")
  
}



sampleToDF <- function(s, n) {
  f <- n-s
  
  cols <- max(1,ceiling(n^(0.5)/3))
  
  n.sDots <- as.integer(s / cols)
  n.fDots <- as.integer(f / cols)
  n.sRmdr <- s %% cols
  n.fRmdr <- f %% cols
  
  x.vals <- vector()
  y.vals <- vector()
  colors <- vector()
  
  if (f > 0) {
    for (i in 1:cols) {
      dots <- n.fDots
      
      if (n.fRmdr > 0) {
        n.fRmdr <- n.fRmdr - 1
        dots <- n.fDots + 1
      }
      
      x.col <- rep(i, dots)
      
      y.col <- seq(from = cols, to = (dots*cols), by = abs(cols))
      
      c.val <- rep("grey100", dots)
      
      x.vals <- c(x.vals, x.col)
      y.vals <- c(y.vals, y.col)
      colors <- c(colors, c.val)
    }
  }
  
  if (s > 0){
    for (i in -cols:-1) {
      dots <- n.sDots
      if (n.sRmdr > 0) {
        n.sRmdr <- n.sRmdr - 1
        dots <- n.sDots + 1
      }
      x.col <- rep(i, dots)
      
      y.col <- seq(from = cols, to = (dots*cols), by = abs(cols))
      
      c.val <- rep("darkgreen", dots)
      
      x.vals <- c(x.vals, x.col)
      y.vals <- c(y.vals, y.col)
      colors <- c(colors, c.val)
    }
  }
  
  x.vals <- as.numeric(x.vals)
  colors <- as.factor(colors)
  
  manual.colors <- factor(colors, levels = c("darkgreen","grey100"))
  legend <- c(paste("Yes =", s),paste("No =", f))
  if (f <= 0) {
    manual.colors <- factor(colors, levels = c("darkgreen"))
    legend <- c(paste("Yes =", s))
  }
  if (s <= 0) {
    manual.colors <- factor(colors, levels = c("grey100"))
    legend <- c(paste("No =", f))
  }
  
  if (all(length(x.vals) == length(y.vals),
          length(y.vals) == length(colors))) {
    
    df <- data.frame(
      "x" = x.vals,
      "y" = y.vals,
      "c" = colors,
      stringsAsFactors = TRUE)
    
    ggplot(df, aes(x, y)) +
      geom_point(aes(fill = as.factor(c),
                     size = max(0.1, round(cols^(1/3)/7, 3))),
                 pch = 21, color = "grey10") +
      scale_fill_manual(
        name = "",
        values = levels(manual.colors), 
        labels = legend) +
      theme(
        # remove x and y axis in their entirety
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
      theme(
        # make background more bland, add border, top side
        panel.border = element_rect(colour = "black", fill=NA, size=1.5),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
      theme(
        # make the Yes No legend larger
        legend.text = element_text(size = 20)) +
      guides(fill = guide_legend(override.aes = list(size=5)))+
      guides(size = FALSE, color = FALSE)
  }
}