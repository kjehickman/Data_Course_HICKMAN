library(tidyverse)
library(dplyr)
library(janitor)
library(tidyr)
options(scipen = 999)

ls <- read_csv("./landdata-states.csv")

# Step I. 
  # here I re-created fig1.png and exported it to a .jpg
ls %>% 
  ggplot(aes(x=Year, y=Land.Value, color=region)) +
  geom_smooth() +
  labs(y="Land Value (USD)",
       x="Year",
       color="Region") +
  theme_minimal()

ggsave("./HICKMAN_Fig_1.jpg")

# Step II.
  # This displays which states are under "NA" in the region variable
na <- ls %>% 
  filter(is.na(region))

# Step III.
  # Here I've loaded and converted the dataset to long type
uu <- read_csv("./unicef-u5mr.csv")

colnames(uu) <- make_clean_names(colnames(uu))

uu <- uu %>% 
  pivot_longer(cols = starts_with("u5mr_"),
               names_to = "Year",
               values_to = "MortalityRate")

  # here I'm fixing the Year variable and listing it as numeric
uu$Year <- sub("u5mr_", "", uu$Year)
uu$Year <- as.numeric(uu$Year)
class(uu$Year)

# Step IV. [figure 2]
  # These functions will graph uu to match fig2.png
uu %>% 
  ggplot(aes(x=Year, y=MortalityRate, color=continent)) +
  geom_point() +
  labs(color="Continent") +
  theme_minimal()
# MAKE TEXT AND POINTS LARGER

# this function exports the graph to a .jpg
ggsave("./HICKMAN_Fig_2.jpg")

# Step IV. [figure 3]
  # These functions give me the average mortality rate over time (annually) for each continent
uu %>% 
  group_by(continent, Year, MortalityRate) %>% 
  summarise(MeanMR = mean(MortalityRate)) %>% 
  View()



