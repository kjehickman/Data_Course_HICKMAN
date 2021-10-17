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
  # SUGGESTED throwing in a unique() to make more interesting
  unique(ls$State, incomparables = TRUE, fromLast = FALSE)

  ls[!duplicated(ls$State), ls$region]

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
  # the "proportion" should be the number of deaths that occurred in a given region, in a given year, 
  # divided by the total mortality rate for that year
uu %>% 
  filter(!is.na(MortalityRate), ) %>% 
  group_by(Year, region) %>% 
  summarise(Year, MortalityRate, 
            AnnSum = sum(MortalityRate * Year),
            MRProportions = proportions(MortalityRate * Year), 
            N=n()) %>% 
  ggplot(aes(x=Year, y=MRProportions)) +
  geom_point() +
  labs(y="Mortality Rate",
       x="Year") +
  facet_wrap(~region)



