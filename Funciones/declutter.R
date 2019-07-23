# Helpers for a personalized plots themes
# using the tidiverse philosophy, the ggplot tool and declutter plots.

# For more reference:
# Cole Naussbaumer 
# https://www.youtube.com/watch?v=X79o46W5plI
# Hadley Wickham

# Author: Sebastián Morales Peralta


library(ggplot2)

general.theme <- theme(
  axis.text = element_text(size = 12, colour = grey(0.4)),
  axis.title = element_text(size = 12, colour = grey(0.3),face = "bold"),
  plot.title = element_text(size = 16, colour = grey(0.1), face = "bold"),
  plot.subtitle = element_text(size = 14, colour = grey(0.3), face = "plain"),
  plot.caption = element_text(size= 10, colour = grey(0.5), hjust = 0),
  legend.title = element_text(size = 8, colour = grey(0.5)),
  legend.text = element_text(size = 6, colour = grey(0.5)),
  plot.margin = unit(c(1,1,1,1), "cm"), # top, right, bottom, left
  panel.background = element_rect(fill = "white")
)

theme.scatter <- theme(
  axis.line = element_line(size = 0.7, colour = grey(0.4)),
  axis.ticks = element_line(size = 0.7, colour = grey(0.4))
) +
  general.theme

theme.point <- theme.scatter

theme_declutter <- theme.scatter

theme.vcol <- theme(
  axis.line.y = element_line(size = 0.7, colour = grey(0.4)),
  axis.ticks.y = element_line(size = 0.7, colour = grey(0.4)),
  axis.ticks.x = element_line(colour = "white"),
  axis.ticks.length = unit(0.1, "cm")
) +
  general.theme

theme.hcol <- theme(
  axis.line.x = element_line(size = 0.7, colour = grey(0.4)),
  axis.ticks.x = element_line(size = 0.7, colour = grey(0.4)),
  axis.ticks.y = element_line(colour = "white"),
  axis.ticks.length = unit(0.1, "cm")
) +
  general.theme



# # Examples
# # Don´t run
# 
# library(tidyverse)
# 
# data("diamonds")
# 
# diamonds %>%
#   sample_n(1000) %>%
#   group_by(cut) %>%
#   summarise(avg = mean(price)) %>%
#   ggplot +
#   geom_col(aes(cut,avg, fill = cut)) +
#   theme.hcol+
#   labs(x = "Eje x", y = "Eje y", title = "Gráfica de barras",
#        subtitle = "Base de datos Diamonds", caption = "Caption") +
#   scale_color_manual(values = c("red", "red", grey(.4),"blue", "blue"),
#                      aesthetics = "fill")+
#   scale_y_continuous(expand = c(0,5)) +
#   coord_flip() +
#   guides(fill = FALSE)
# 
# diamonds %>%
#   sample_n(1000) %>%
#   group_by(cut) %>%
#   summarise(avg = mean(price)) %>%
#   ggplot +
#   geom_col(aes(cut,avg)) +
#   theme.vcol+
#   labs(x = "Eje x", y = "Eje y", title = "Gráfica de barras",
#        subtitle = "Base de datos Diamonds", caption = "Caption") +
#   scale_y_continuous(expand = c(0,5))
#   
# 
# diamonds %>%
#   sample_n(1000) %>%
#   ggplot +
#     geom_point(aes(x,y)) +
#     labs(x = "Eje x", y = "Eje y", title = "Gráfica de dispersión",
#          subtitle = "Base de datos Diamonds", caption = "Caption")+
#     theme.point
# 
# diamonds %>%
#   sample_n(1000) %>%
#   ggplot +
#   geom_histogram(aes(x)) +
#   labs(title = "Histograma de x",
#        subtitle = "Base de datos Diamonds", caption = "Caption")+
#   scale_y_continuous(expand = c(0,0))+
#   theme_declutter
# 
# diamonds %>%
#   sample_n(1000) %>%
#   ggplot +
#   geom_density(aes(x), color = grey(.2), fill = grey(.2)) +
#   labs(title = "Densidad de x",
#        subtitle = "Base de datos Diamonds", caption = "Caption") +
#   scale_y_continuous(expand = c(0,0))+
#   theme_declutter
# 
# diamonds %>%
#   sample_n(1000) %>%
#   ggplot +
#   geom_boxplot(aes(cut,x)) +
#   labs(title = "Boxplot de x",
#        subtitle = "Base de datos Diamonds", caption = "Caption") +
#   theme_declutter
# 
# diamonds %>%
#   sample_n(1000) %>%
#   ggplot +
#   geom_boxplot(aes(cut,x)) +
#   labs(title = "Boxplot de x",
#        subtitle = "Base de datos Diamonds", caption = "Caption") +
#   theme_declutter+
#   coord_flip()
