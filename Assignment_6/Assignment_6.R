library(tidyverse)
library(tidyr)
library(dplyr)
library(janitor)
library(readxl)
library(gganimate)
library(transformr)
library(sf)

# loaded untidy dataset
dat <- read_csv("../../Data_Course/Data/BioLog_Plate_Data.csv") 

# snured clean names for all variables
names(dat) <- make_clean_names(names(dat))
glimpse(dat)

# create animated plot: "Mean_Absorbance" (y) against time (x), color = Sample_ID

# this step combines the hourly observations into a variable "time"
dat <- dat %>% 
  pivot_longer(cols = c("hr_24", "hr_48", "hr_144"),
               names_to = "Time",
               values_to = "Absorbance")

# these functions removes prefixes "hr_" form observations and lists time as numeric
dat$Time <- sub("hr_", "", dat$Time)
dat$Time <- as.numeric(dat$Time)
class(dat$Time)

# this function will create a variable for water v soil samples
dat %>% 
  mutate(Type = case_when(sample_id == "Clear_Creek" ~ "water",
                          sample_id == "Waste_Water" ~ "water",
                          sample_id == "Soil_1" ~ "soil", 
                          sample_id == "Soil_2" ~ "soil")) %>% 
  filter(dilution == 0.1) %>% 
  ggplot(aes(x=Time, y=Absorbance, color=Type)) +
  geom_smooth(method="loess", se = FALSE) +
  facet_wrap(~substrate) +
  theme_minimal() +
  labs(title="Just dilution 0.1")
# this is the first plot to complete step 3


meow <- dat %>%
  group_by(sample_id, rep, Time, dilution) %>% 
  filter(substrate == "Itaconic Acid") %>% 
  mutate(trip = factor(rep, levels = c(1,2,3))) %>% 
  summarize(Mean_absorbance = mean(Absorbance)) %>% 
  filter(!Mean_absorbance == 0) 

meow %>% 
  ggplot(aes(x=Time, y=Mean_absorbance, color=sample_id)) +
  geom_smooth(method="lm", se = FALSE) +
  facet_wrap(~dilution) +
  theme_minimal() 


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

dat %>% 
  filter(substrate == "Itaconic Acid") %>% 
  group_by(sample_id, rep) %>% 
  select(Absorbance, 
         Time,
         sample_id, 
         dilution,
         substrate,
         rep) %>% 
  summarize(Mean_absorbance = mean(Absorbance),
            na.rm = TRUE) %>% 
  View()
?case_when()
?na.rm
?summarize()

# select(sample_id, dilution, Absorbance, Mean_absorbance, Time) %>% 
  class(dat$Absorbance)
  dat$Absorbance[dat$Absorbance == 0] <- NA
  dat$Absorbance <- as.numeric(dat$Absorbance)
  # these funcitons remove 0 from the dataset 
  
