# correct file

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

