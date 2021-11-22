library(tidyverse); packageVersion("tidyverse")
library(phyloseq); packageVersion("phyloseq")
library(ShortRead); packageVersion("ShortRead")
library(ggplot2)

full <- readRDS("./bact_and_fungi_clean_ps_object")

# see this site: https://joey711.github.io/phyloseq/

full

plot_bar(full)

full@sam_data %>% View

# what do bacteria look like and fungi? Do they follow the same pattern
# check out MB script to walk-through alanyses

ntaxa(full)
nsamples(full)

ls(full@sam_data)

#separating the sample data
sample_df <- full@sam_data 
sample_df %>% View

# separating the out table
otu_df <- full@otu_table
otu_df %>% View

  # an OTU table: 
    # lists samples in rows, species in columns (indicated by DNA strings), 
    # the rows will contain how many individuals from each species were present in each sample
    # describes community composition of species (in numbers of individuals) from  given sample
      # use relative abundance: where you dived number of species X in sample X by the total number of all species' individuals in sample X

# separating 
taxonomy_df <- full@tax_table
taxonomy_df %>% View

# Make a relevant df
names(sample_df)
sample_df %>% 
  select(Host,Location,Run,Structure,Lat,Long) %>% 
  glimpse()

# do a logistic regression to see if you can predict X based off of presence and location
  # must find a better way to visualize
sample_df %>% 
  ggplot(aes(x=Microbe,y=Structure, color=Location)) +
  geom_point() 

# ANOVA permutational analysis variance for ordinance tables
vegan::adonis(otu_table(full) ~ full@sam_data$Status) 
# this is for 2D analysis, and is a test

unique(sample_df$Study)
  # this will tell me which studies I'm using ("Bacteria" is Zahn's)
# just want to know if fungi and bacteria follow same patterns for: Structure, Location, GPS

library(geosphere)
lat <- full@sam_data$Lat %>% as.numeric()
lon <- full@sam_data$Lon %>% as.numeric()

# make df of those
locations <- data.frame(lat, lon)

# here's our reference point
here <- c(-111.71781761327077, 40.279687174475804)
distancesfromUVU <- distHaversine(here,locations)

# if important, add it to phyloseq object
full@sam_data$DistfromUVU <- distancesfromUVU # DON'T RUN

dist(locations,method = "euclidean")
unique(locations)
#check out mantel test
# check documentation for vignette to see in geosphere: https://cran.r-project.org/web/packages/geosphere/vignettes/geosphere.pdf 
  # vignettes for phyloseq:
    # general: https://www.bioconductor.org/packages/devel/bioc/vignettes/phyloseq/inst/doc/phyloseq-analysis.html
    # basic: https://www.bioconductor.org/packages/devel/bioc/vignettes/phyloseq/inst/doc/phyloseq-basics.html
# geosphere r package see distance between all points as google search
  # to compare each of the locations to all other locations would entail a matrix..
    # 2 matrices: each sample and relative abundance, then each sample and it's distance form other samples
    # r multiple regression of matrices (package: ecodist)
    # convert out_table into matrix, convert distance matrix (after I create it) and compare?
      # permutation, bootstrapping method
      # will show how important distance from each other to show community difference
        # simplified is mantel.rtest()

# create distance matrix

loc1 <- c(41.5, 100.3)
loc2 <- c(42.1, 101.5)
loc3 <- c(44,105)
ps@sam_data$lat # and for long to make a new df of lat/long

library(vegan)

loc_dist <- rbind(loc1, loc2, loc3) %>% 
  vegan::vegdist()

library(geosphere)
lat <- full@sam_data$Lat %>% as.numeric()
lon <- full@sam_data$Lon %>% as.numeric()

# this is the format of the distance matrix for geographical space
mat_dist <- data.frame(Lat=lat,
                       Lon=lon) 

# this removes NAs and 
vegan::vegdist(x = mat_dist, na.rm = TRUE)

# exctract otu_table, then send to vegdist to make matrix
otu_dist <- otu_dist(ps) %>% vegan::vegdist()

# regress the 2 matirces against eachother using: mrm or mantel.test
ape::mantel.test(loc_dist, otu_dist)

# otu distance table
cardist1 <- mtcars %>% 
  select(where(is.numeric)) %>% 
  select(1:5) %>% 
  vegan::vegdist(diag = FALSE)
# gives relative similarity on scale of 0-1 where 0 is the perfect overlap, and 1 is none
  # for MB, 2 samples have same microbiome and distance is 0, with difference working to 1

# physical distance table
cardist2 <- mtcars %>% 
  select(where(is.numeric)) %>% 
  select(6:10) %>% 
  vegan::vegdist(diag = FALSE)

# otu ~ distance
  # does difference in proximity explain differnce in species composition?
mrm <- ecodist::MRM(cardist1 ~ cardist2)
# NEED THIS AND THE ONE BELOW
vegan::adonis()
# just like glm, but matrix instead of categorical
# i.e.
vegan::adonis(data = full,
              formula = otu_df ~ Lat + Lon + Structure)
# works to explain matrix rather than single variable (from logistic regression)
  # can define permutations, method, parallel, strata

lme4::lmer()
  # when you have 'structure' nested in tree or sample id, 
    # you can include this in the model using mixed effect
      # this won't return a p-value since they are misleading  so you need to know a lot about stats--interpret coefficients

# weird error--DON'T DO IT, and forget it
ape::mantel.test(cardist1, cardist2)

sample_df %>% 
  distHaversine(GPS, r = 6378137)

class(full@sam_data$Lat)


