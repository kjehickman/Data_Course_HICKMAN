library(tidyverse)
library(dplyr)
require(scales)

# Step I. 
  # This assigned the cleaned_covid_data.csv to a dataframe
df <- read_csv("cleaned_covid_data.csv")


# Step II. 
  # This is a subset of the previous dataframe that only includes states starting with "A"
Astates <- df[grepl("A", df$Province_State), ]


# Step III.
  # This is a plot of the previous subset, separated by state.
Astates %>% 
  ggplot(aes(x=Last_Update, y=Active)) +
  geom_point(size = 0.5, color = "black") +
  geom_smooth(se = FALSE, color = "violet") +
  facet_wrap(~Province_State, scales = "free") +
  labs(x="Date of Observation", 
       y="Active COVID-19 Cases", 
       title = "Active COVID-19 Cases by Date:",
       subtitle = "The 'A' States",
       caption = "Data shows COVID-19 cases increase in every region from 2020 to 2021.") +
  theme_get()


# Step IV. 
  # The first function creates the df subset using aggregate and max
state_max_fatality_ratio <- aggregate(Case_Fatality_Ratio ~ Province_State, data = df, max, na.rm = TRUE)
  # The second function changes the name of the second variable
names(state_max_fatality_ratio)[2] <- 'Maximum_Fatality_Ratio'
  # The third function reassigns the df subset to be sorted in descending order
state_max_fatality_ratio <- state_max_fatality_ratio[order(-state_max_fatality_ratio$Maximum_Fatality_Ratio), ]
 
 # Checking class type for each variable in state_max_fatality_ratio
str(state_max_fatality_ratio)
  # reassigned to a duplicate
state_max_fatality_ratio2 <- state_max_fatality_ratio
  # checked class of variable in-question
class(state_max_fatality_ratio$Province_State)
  # changed variable form chr to factor
state_max_fatality_ratio2$Province_State <- as.factor(state_max_fatality_ratio$Province_State)
  # double checked new df class types
str(state_max_fatality_ratio2)


# Step V. 
  # This function orders the variable "Province_State" in decreasing order
state_max_fatality_ratio2 <- state_max_fatality_ratio2[order(state_max_fatality_ratio$Maximum_Fatality_Ratio, decreasing = TRUE), ] 

# This creates a barplot with the desired specifications, by descending Maximum_Fatality_Ratio order
state_max_fatality_ratio2 %>% 
  ggplot(mapping = aes(x=Province_State, y=Maximum_Fatality_Ratio)) +
  geom_bar(mapping = aes_(x=reorder(state_max_fatality_ratio2$Province_State, -state_max_fatality_ratio$Maximum_Fatality_Ratio),  
                          y=state_max_fatality_ratio2$Maximum_Fatality_Ratio), 
           stat = "identity", fill="black", alpha=0.9) +
  labs(x="Region", y="Maximum Fatality Ratios", title = "Maximum Fatality Ratios by State",
       caption = "Max fatality rates vary greatly by region, 
       \ndefined as states and the District of Columbia.") +
  theme(plot.caption = element_text(size = 10), 
               plot.title = element_text(hjust = 0, colour = "blue", face = "bold"),
        axis.text.x = element_text(angle = 90), panel.background = element_rect(fill = "green"))


# Step VI. BONUS; Fix ticks
deathsplot <- df %>% 
  group_by(Last_Update, Deaths) %>% 
  summarize()

deathsplot %>% 
  ggplot(aes(x=Last_Update, y=Deaths)) +
    geom_bar(mapping = aes_(x=deathsplot$Last_Update, 
                            y=deathsplot$Deaths), 
             stat = "identity", fill="turquoise", alpha=0.75) +
    labs(x="Date", y="Deaths",  title = "COVID-19 Deaths Over Time",
         caption = "Deaths steadily increased over time throughout the US.") +
    scale_y_continuous(labels = comma) 
