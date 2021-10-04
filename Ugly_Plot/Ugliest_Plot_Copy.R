library(tidyverse)
library(ggimage)
library(palmerpenguins)
library(png)
library(ggplot2)
library(grid)
# patchwork doesn't work with my version of R?
library(ggforce)
library(ggpubr)
library(gganimate)
library(transformr)
library(jpeg)

df <- penguins  

img <- readJPEG("./face_foot.jpeg")
jpeg <- readJPEG("./angry_penguin.jpeg")

# this is the plot that I can export as a jpg
plot3 <- ggplot(df, aes(x=bill_depth_mm,y=flipper_length_mm)) +
  annotation_custom(rasterGrob(image = jpeg, 
                               width = unit(1,"npc"), 
                               height = unit(1,"npc")),  
                    -Inf, Inf, -Inf, Inf) +
  geom_image(mapping = aes(x=df$3,y=df$4), image = img) +
  geom_smooth() +
  labs(x="Bill_M g", 
       y="Flip_L mm", 
       title = "Plot 14: After many attempts I finally ofund a way to include an angry macaroni penguin who is featured on every facet. 
       \nThis is to ensure that we understand how angry penguins can be about being small. 
       \nIf you pretend the flying feet-faces are insects, it makes the plots a little more interesting :D",
       subtitle = "Feet faced penguins",
       caption = "Penguin body part comparison: aes(x=bill_depth_mm,y=flipper_length_mm)",
       color = "Penguin\nspecies") +
  theme(plot.caption = element_text(size = 10, color = "tomato3"), 
        plot.title = element_text(hjust = 0.5, colour = "yellowgreen", face = "bold.italic"),
        plot.subtitle = element_text(color = "thistle4"),
        axis.text.x = element_text(angle = 168, hjust = 0.75), 
        axis.text.y = element_text(size = 4, angle = 40, color = "violet"),
        plot.background = element_rect(fill = "Green", color = "royalblue4", size = 5, linetype = 12),
        legend.text = element_text(angle=190)) +
  facet_wrap(~island)

# this simply places and image behind the whole plot rather than duplicating it per facet
ggbackground(plot3, jpeg)
