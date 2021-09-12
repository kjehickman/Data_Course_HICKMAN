library(tidyverse)

F_Class <- c("Agarico", "Entorrhizo", "Eurotio", "Sordario", "Entorrhizo", "Sordario")
B_Genus <- c("Pleurocapsa", "Marinomonas", "Halomonas", "Tunicatimonas", "Halomonas", "Tunicatimonas")
Plant_Part <- c("Fruit", "Sediment", "Leaf", "Sediment", "Pneumatophore", "Sediment")

fakdat <- data.frame(cbind(F_Class, B_Genus, Plant_Part))

g <- ggplot(data = fakdat, aes(x=F_Class, y=B_Genus, color=Plant_Part))
g + geom_jitter(width = 2, size = 2) +
  labs(subtitle="Bacteria and Fungi by Plant Part",
       x="Fungal Class",
       y="Bacterial Genus",
       title="Microbiome Correlation:")

write.csv(fakdat, "/Users/mac/Desktop/Data_Course_HICKMAN/Assignment_4/fake_data.csv", row.names = FALSE)

