library(tidyverse)

# this set of functions creates the concatenations that will constitute my fake data set
F_Class <- c("Agarico", "Entorrhizo", "Eurotio", "Eurotio", "Entorrhizo", "Sordario", "Agarico", "Sordario", "Eurotio", "Agarico", "Agarico", "Entorrhizo", "Entorrhizo", "Sordario", "Entorrhizo", "Eurotio")
B_Genus <- c("Pleurocapsa", "Marinomonas", "Halomonas", "Tunicatimonas", "Halomonas", "Tunicatimonas", "Halomonas", "Pleurocapsa", "Halomonas", "Halomonas", "Halomonas", "Tunicatimonas", "Pleurocapsa", "Pleurocapsa")
Plant_Part <- c("Fruit", "Sediment", "Leaf", "Sediment", "Pneumatophore", "Sediment", "Sediment", "Pneumatophore", "Leaf", "Fruit", "Fruit", "Leaf", "Pneumatophore", "Pneumatophore", "Fruit", "Leaf")
Group <- c("Sonneratia", "Sonneratia", "Sonneratia", "Sonneratia", "Sonneratia", "Avicennia", "Avicennia", "Sonneratia", "Sonneratia", "Avicennia", "Avicennia", "Avicennia", "Avicennia", "Avicennia", "Sonneratia", "Avicennia")

# fakdat is the new of my fake data set, using cbind() and data.frame()
fakdat <- data.frame(cbind(F_Class, B_Genus, Plant_Part, Group))

# having 3 categorical variables to correlate has been challenging to represent well... 
# boxplots weren't helping, scatterplots didn't make much sense, bubble charts used 3+ if they were numerical, 
# pairwaise scatterplots and stacked bar plots (like in Lee et al.) used at least one numerical variable.
# this ggplot utilizes geom_point() and separates into 4 graphs by plant part with intersections representing the presence of fungi & bacteria
ggplot(fakdat, mapping = aes(x=B_Genus, y=F_Class, color = Group)) +
  geom_point(size = 3) +
  facet_wrap(~Plant_Part) +
  labs(subtitle="Bacteria and Fungi by Plant Part",
       caption = "Correlation as an indicator of microbiome diversity in plant parts. \nSediment appears to be highest in diversity and several fungal classes \nconsistently overlap with the same bacterial genera.",
       x="Bacterial Genus",
       y="Fungal Class",
       title="Microbiome Correlation:") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90))

# this function exports the fake data to a csv file. I removed the full file path, so it should work on any computer.
write.csv(fakdat, "fake_data.csv", row.names = FALSE)
