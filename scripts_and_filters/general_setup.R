options(
  tinytex.verbose = TRUE
)

knitr::opts_chunk$set(
  echo = F, 
  warnings = F, 
  messages = F
)

library(ggplot2)

theme_set(
  theme_light() +
    theme(
      panel.background = element_blank(),
      panel.border = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      axis.line = element_line(colour = "black"),
      text = element_text(size = 14),
      legend.position = "top"
    )
)