
library(ggplot2)
library(tidyverse)

Input_Types = c("Proportion","Counts")

Sample_Limit = 10000

Presets = list()
# Presets['name'] = (proportion, counts, sample.size)
Presets$`Medical Consultant` = c(1, signif(3/62,3), 3, 62)
Presets$`Tappers and Listeners` = c(1, signif(3/120,3), 3, 120)
Presets$`Outside Youtube Videos` = c(1, signif(37/128,3), 37, 128)
Presets$`Twitter Users and News` = c(2, 0.52, round(0.52*763), 763)
Presets$`Custom` = c(2,.5,50,100) # = Change Default Value



# theme for plots
plaintheme = theme_bw() + 
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank() ) +
  theme(axis.line.x = element_line(color="black", size = 1),
        axis.line.y = element_line(color="black", size = 1))+
  theme(legend.position = "none")

# axis theme for plots
axistheme = theme(plot.title = element_text(hjust = 0.5, color = "black", face = "bold", size=20)) +
  theme(axis.title = element_text(color = "black", size = 16)) +
  theme(axis.text.x = element_text(size = 14, color = "black")) +
  theme(axis.text.y = element_text(size = 14, color = "black"))

# colors
c_inner = "grey70" # dotplot dot fill color
c_outer = "#F6491A" # color for cutoff values
c_select = "grey10" # color for selected sample 

man_fills = factor(c(c_outer, c_inner), levels = c(c_outer, c_inner))
man_color = factor(c(c_outer, c_inner, c_select), 
                   levels = c(c_outer, c_select, c_inner))


DotplotDataMaker = function(data) {
  data = data %>% arrange(samples)
  
  xvals = vector()
  yvals = vector()
  
  frequency = table(data$samples)
  values = as.numeric(names(frequency))
  
  xvals = unlist(Map(rep, values, frequency))
  yvals = unlist(Map(seq, from = 1, to = frequency))
  
  data['xvals'] = xvals
  data['yvals'] = yvals
  
  data = data %>% arrange(index)
  
  return(data)
}
