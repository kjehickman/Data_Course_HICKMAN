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
               values_to = "Absorbance",
               names_prefix = "hr_")

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

# plot 2

plot2 <- dat %>% 
  filter(substrate == "Itaconic Acid") %>% 
  group_by(dilution, sample_id, Time) %>% 
  summarize(Mean_absorbance = mean(Absorbance), Time,
            na.rm = TRUE) %>% 
  ggplot(aes(x=Time, y=Mean_absorbance, color=sample_id)) +
  geom_line() +
  facet_wrap(~dilution)

plot2 + transition_reveal(Time) 

