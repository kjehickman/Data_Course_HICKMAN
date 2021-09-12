library(tidyverse)

F_Class <- c("Agaricomyctes", "Entorrhizomycetes", "Eurotiomycetes", "Sordariomycetes", "Entorrhizomycetes", "Sordariomycetes")
B_Genus <- c("Pleurocapsa", "Marinomonas", "Halomonas", "Tunicatimonas", "Halomonas", "Tunicatimonas")
Plant_Part <- c("Fruit", "Sediment", "Leaf", "Sediment", "Pneumatophore", "Sediment")

fakdat <- data.frame(cbind(F_Class, B_Genus, Plant_Part))

ggplot(data = fakdat, aes(F_Class, Plant_Part, B_Genus), color = 'green', size = 3)

