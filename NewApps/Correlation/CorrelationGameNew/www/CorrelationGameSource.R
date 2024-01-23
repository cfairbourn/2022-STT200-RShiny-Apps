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