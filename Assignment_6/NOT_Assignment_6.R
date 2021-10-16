library(tidyverse)
library(tidyr)
library(dplyr)
library(janitor)
library(readxl)
library(gganimate)
library(transformr)
library(sf)


gganimate::transition_time()

?sapply()
?aggregate()
# DONT run this
summarize(Mean_Absorbance = mean(Absorbance), Time) %>% 
  select(sample_id, rep, Time, Absorbance, Mean_absorbance, dilution) %>% 
  
  # OR this
  summarize(Mean_absorbance = mean(Absorbance),
            Absorbance, 
            Time,
            sample_id, 
            dilution,
            substrate,
            rep,
            na.rm = TRUE) %>% 

  meow <- dat %>%
  group_by(sample_id, rep, Time, dilution) %>% 
  filter(substrate == "Itaconic Acid") %>% 
  mutate(trip = factor(rep, levels = c(1,2,3))) %>% 
  summarize(Mean_absorbance = mean(Absorbance)) %>% 
  filter(!Mean_absorbance == 0) 

dat$Time <- as.numeric(dat$Time)

ggplot(meow, aes(x=Time, y=Mean_absorbance, color=sample_id)) +
  geom_smooth(method="lm", se = FALSE) +
  facet_wrap(~dilution) +
  theme_minimal() +
  transition_time(Time, range = c(24, 144)) + ease_aes('linear')

# Answers to ass 6: plot 1
dat <- dat %>% 
  pivot_longer(cols = c("hr_24", "hr_48", "hr_144"),
               names_to = "Time",
               values_to = "Absorbance",
               names_prefix = "hr_") %>% 
  mutate(Time = as.numeric(Time)) %>% 
  mutate(Type = case_when(sample_id == "Clear_Creek" ~ "water",
                          sample_id == "Waste_Water" ~ "water",
                          TRUE ~ "soil")) %>% 
  filter(dilution == 0.1) %>% 
  ggplot(aes(x=Time, y=Absorbance, color=Type)) +
  geom_smooth(method="loess", se = FALSE) +
  facet_wrap(~substrate) +
  theme_minimal() +
  labs(title="Just dilution 0.1")
