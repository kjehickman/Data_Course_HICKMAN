library(tidyverse)
library(janitor)
library(tidyr)
library(dplyr)
library(transformr)
library(sf)
library(broom)

#### STEP I: Load and clean

  # Loaded .csv and assigned as an object in the Environment
dat <- read_csv("./FacultySalaries_1995.csv")

  # used pivot_longer to combine salary, compensation and number columns
  # then changed names in Rank variable
dat <- dat %>% 
  pivot_longer(cols = c("AvgFullProfSalary", "AvgAssocProfSalary", "AvgAssistProfSalary"),
               names_to = "Rank",
               values_to = "Salary") %>% 
  pivot_longer(cols = c("AvgFullProfComp", "AvgAssocProfComp", "AvgAssistProfComp"),
               names_to = "Rank_Comp",
               values_to = "Compensation") %>% 
  pivot_longer(cols = c("NumFullProfs", "NumAssocProfs", "NumAssistProfs", "NumInstructors"),
               names_to = "Rank_Num",
               values_to = "Staff_Number") %>% 
  mutate(Rank = case_when(Rank == "AvgFullProfSalary" ~ "Full",
                          Rank == "AvgAssocProfSalary" ~ "Assoc",
                          Rank == "AvgAssistProfSalary" ~ "Assist"))

# removed VIIB schools
dat <- dat[!(dat$Tier=="VIIB"), ]

# re-created fig1.png
dat %>% 
  ggplot(aes(x=Rank,y=Salary)) +
  geom_boxplot(aes(fill=Rank)) +
  facet_wrap(~Tier) +
  theme_minimal() +
  theme(legend.key.size = unit(1, "cm"),
        legend.title = element_text(size = 19, color = "black"),
        legend.text = element_text(size = 13, color = "black"),
        axis.text.x = element_text(angle = 65, size = 13, color = "black"),
        axis.text.y = element_text(size = 13, color = "black"),
        axis.title.x = element_text(size = 19, color = "black"),
        axis.title.y = element_text(size = 19, color = "black"),
        strip.text.x = element_text(size = 13, color = "black"))

# exported as HICKMAN_Fig_1.jpg
ggsave("./HICKMAN_Fig_1.jpg")

#### STEP II: Export ANOVA file
# create a model showing how state, tier and rank impact the salary
mod1 <- glm(data = dat, 
            formula = Salary ~ State + Tier + Rank)
summary(mod1)

# ANOVA of the formula
anova(mod1)

# export ANOVA table as Salary_ANOVA_Summary.txt
sink("./Salary_ANOVA_Summary.txt")
anova(mod1)
sink(NULL)

# IS THERE A BETTER WAY TO DO THIS? IT LOOKS VERY BAD

#### STEP III: Load and tidy Juniper_Oils.csv
# loaded .csv
dat2 <- read_csv("./Juniper_Oils.csv")

# combined chemicals into concentration and ID
dat2 <- dat2 %>% 
  pivot_longer(cols = c("alpha-pinene","para-cymene","alpha-terpineol","cedr-9-ene",
                      "alpha-cedrene","beta-cedrene","cis-thujopsene","alpha-himachalene",
                      "beta-chamigrene","cuparene","compound 1","alpha-chamigrene",
                      "widdrol","cedrol","beta-acorenol","alpha-acorenol","gamma-eudesmol",
                      "beta-eudesmol","alpha-eudesmol","cedr-8-en-13-ol","cedr-8-en-15-ol",
                      "compound 2","thujopsenal"),
             names_to = "ChemicalID",
             values_to = "Concentration")

#### STEP IV: Graph and export image
# graphed the tidy(er) dataset
dat2 %>% 
  ggplot(aes(x=YearsSinceBurn, y=Concentration)) +
  geom_smooth(method = loess) +
  facet_wrap(~ChemicalID, scales = "free_y") +
  theme_minimal() +
  labs(x = "Years Since Burn",
       title = "Concentration by Chemical") +
  theme(plot.title = element_text(hjust = 0.5))

# export to HICKMAN_Fig_2.jpg
ggsave("./HICKMAN_Fig_2.jpg")

#### STEP V: Use a generalized linear model to see which chemicals are significant
# create glm to show statistical significance of time and chemical on concentration
mod2 <- glm(data = dat2, formula = Concentration ~ ChemicalID * YearsSinceBurn)
mod2_sum <- summary(mod2)

# produce a data frame showing only statistically significant chemicals and models
  # include coefficient estimates, standard error, statistic and p value
# tidy tibble
tt <- tidy(mod2)

#tidy tibble subsetted to show only statistically significant ChemicalID's
ttss <- tt[c(tt$p.value <= 0.05), ]

#### STEP VI: Push all files to Github

#### STEP VII: Bonus Points
# Make an R-Markdown file contianing all this info and knit to html



