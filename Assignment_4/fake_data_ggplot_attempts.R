library(tidyverse)

# essentially failed attempts

# boxplot sucks with these... non-numerical
ggplot(fakdat, aes(x=F_Class, y=B_Genus)) +
  geom_boxplot() +
  facet_wrap(~Group)

ggplot(fakdat, aes(x=F_Class, y=B_Genus)) +
  pairs(~F_Class + B_Genus + Plant_Part, col = factor(fakdat$F_Class, fakdat$B_Genus, fakdat$Plant_Part, fakdat$Group), data = fakdat) +
  facet_wrap(~Group)

fakdat %>% 
  ggplot(mapping = aes(x=F_Class, y=Plant_Part)) +
  geom_point(size = 3, alpha = 0.25) +
  facet_wrap(~Group)  
# how do you have two graphs with different variables next to each other? 
# I obvs can't use 2 ggplot functions but I want different aes(). 
# Can I do that by piping data to ggplot, then defining aes for each indv. gemo_?

ggplot(fakdat, aes(x=F_Class, y=B_Genus, color=Plant_Part) + 
         geom_jitter(width = 2, size = 2) +
         labs(subtitle="Bacteria and Fungi by Plant Part",
              x="Fungal Class",
              y="Bacterial Genus",
              title="Microbiome Correlation:"))

# assignment 4 draft:

library(tidyverse)

# this set of functions creates the concatenations that will constitute my fake data set
F_Class <- c("Agarico", "Entorrhizo", "Eurotio", "Eurotio", "Entorrhizo", "Sordario", "Agarico", "Sordario", "Eurotio", "Agarico", "Agarico", "Entorrhizo", "Entorrhizo", "Sordario", "Entorrhizo", "Eurotio")
B_Genus <- c("Pleurocapsa", "Marinomonas", "Halomonas", "Tunicatimonas", "Halomonas", "Tunicatimonas", "Halomonas", "Pleurocapsa", "Halomonas", "Halomonas", "Halomonas", "Tunicatimonas", "Pleurocapsa", "Pleurocapsa")
Plant_Part <- c("Fruit", "Sediment", "Leaf", "Sediment", "Pneumatophore", "Sediment", "Sediment", "Pneumatophore", "Leaf", "Fruit", "Fruit", "Leaf", "Pneumatophore", "Pneumatophore", "Fruit", "Leaf")
Group <- c("Sonneratia", "Sonneratia", "Sonneratia", "Sonneratia", "Sonneratia", "Avicennia", "Avicennia", "Sonneratia", "Sonneratia", "Avicennia", "Avicennia", "Avicennia", "Avicennia", "Avicennia", "Sonneratia", "Avicennia")

# fakdat is the new of my fake data set, using cbind() and data.frame()
fakdat <- data.frame(cbind(F_Class, B_Genus, Plant_Part, Group))

# having 3 categorical variables to correlate has been challenging to represent well... 
# boxplots weren't helping, scatterplots didn't make much sense, bubble charts used 2 if they were numerical, 
# pairwaise scatterplots and stacked bar plots (like in Lee et al.) used at least one numerical variable.
# this ggplot utilizes geom_point() and separtes into 4 graphs by plant part with intersections representing the presence of fungi & bacteria
ggplot(fakdat, mapping = aes(x=B_Genus, y=F_Class, color = Group)) +ggplot(fakdat, mapping = aes(x=B_Genus, y=F_Class, color = Group)) +
  geom_point(size = 3) +
  facet_wrap(~Plant_Part) +
  labs(subtitle="Bacteria and Fungi by Plant Part",
       caption = "Correlation as an indicator of microbiome diversity in plant parts.",
       x="Bacterial Genus",
       y="Fungal Class",
       title="Microbiome Correlation:") +
  theme_light()

# this function exports the fake data to a csv file
write.csv(fakdat, "/Users/mac/Desktop/Data_Course_HICKMAN/Assignment_4/fake_data.csv", row.names = FALSE)

