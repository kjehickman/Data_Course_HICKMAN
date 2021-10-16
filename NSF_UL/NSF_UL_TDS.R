library(tidyverse)
library(dplyr)
library(tidyr)
library(janitor)
library(readxl)

# reading in the .csv file
ul <- read_excel("./NSF_TidyData_Soil.xlsx")

# cleaning the names
names(ul) <- make_clean_names(names(ul))
glimpse(ul)

# any tidying?

