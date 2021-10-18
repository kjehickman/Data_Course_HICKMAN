library(tidyverse)
library(dplyr)
library(janitor)
library(tidyr)
options(scipen = 999)

ls <- read_csv("./landdata-states.csv")

# Step I. ####
  # here I re-created fig1.png and exported it to a .jpg
ls %>% 
  ggplot(aes(x=Year, y=Land.Value, color=region)) +
  geom_smooth() +
  labs(y="Land Value (USD)",
       x="Year",
       color="Region") +
  theme_minimal()

ggsave("./HICKMAN_Fig_1.jpg")

# Step II. ####
  # This displays which states are under "NA" in the region variable
na <- ls %>% 
  filter(is.na(region)) 
  # SUGGESTED using unique() to make more interesting
  unique(ls$State, incomparables = TRUE, fromLast = FALSE)

# Step III. ####
  # Here I've loaded and converted the dataset to long type
uu <- read_csv("./unicef-u5mr.csv")

colnames(uu) <- make_clean_names(colnames(uu))

uu <- uu %>% 
  pivot_longer(cols = starts_with("u5mr_"),
               names_to = "Year",
               values_to = "MortalityRate",
               names_prefix = "u5mr_")

  # here I'm fixing the Year variable and listing it as numeric
uu$Year <- as.numeric(uu$Year)
class(uu$Year)

# Step IV. [figure 2] ####
  # These functions will graph uu to match fig2.png
uu %>% 
  ggplot(aes(x=Year, y=MortalityRate, color=continent)) +
  geom_point() +
  labs(color="Continent") +
  theme_minimal()

  # this function exports the graph to a .jpg
ggsave("./HICKMAN_Fig_2.jpg")

# Step IV. [figure 3] ####
  # These functions give me the average mortality rate over time (annually) for each continent
uu %>%  
  filter(!is.na(MortalityRate), ) %>% 
  group_by(Year, continent) %>% 
  summarise(MeanMR = mean(MortalityRate), MortalityRate, 
            n=n()) %>%
  ggplot(aes(x=Year, y=MeanMR, color=continent)) +
  geom_line(size=2) +
  labs(y="Mean Mortality Rate (deaths per 1000 live births)",
       x="Year",
       color="Continent") +
  theme_minimal()

  # this function will export the graph as a .jpg
ggsave("./HICKMAN_Fig_3.jpg")

# Step V. ####
  # This is the set of functions to complete graph 4
  # filtering out rows without values, creating a mean variable and using it for the mr proportion
uu %>% 
  filter(!is.na(MortalityRate), ) %>% 
  summarise(MRMean = mean(MortalityRate), 
            MortalityRate, region, Year,
            n=n()) %>% 
  mutate(MRregion = sum(MortalityRate), 
         MRProp = MortalityRate/MRMean) %>% 
  ggplot(aes(x=Year, y=MRProp)) +
  geom_point(color="blue", size = 0.5) +
  labs(y="Mortality Rate",
       x="Year") +
  facet_wrap(~region) +
  theme_bw()

# this function will export the graph as a .jpg
ggsave("./HICKMAN_Fig_4.jpg")
