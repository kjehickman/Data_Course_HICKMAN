library(tidyverse); packageVersion("tidyverse")
library(phyloseq); packageVersion("phyloseq")
library(ShortRead); packageVersion("ShortRead")
library(ggplot2)

full <- readRDS("./bact_and_fungi_clean_ps_object")

# see this site: https://joey711.github.io/phyloseq/

full

full@sam_data %>% View

## separate phyloseq parts
#separating the sample data
sample_df <- full@sam_data 
sample_df %>% View

# separating the out table
otu_df <- full@otu_table
otu_df %>% View

# separating 
taxonomy_df <- full@tax_table
taxonomy_df %>% View

# make a distance matrix for lat / lon
lat <- full@sam_data$Lat %>% as.numeric()
lon <- full@sam_data$Lon %>% as.numeric()

  # make df of distance of geographical similarity
location_dist <- data.frame(lat, lon) %>% dist()

# make a distance matrix using dist(): difference of community similarity
otu_dist <- otu_table(full) %>% dist()
otu_dist <- vegan::vegdist(otu_table(full), na.rm = TRUE)

MRM1 <- ecodist::MRM(otu_dist ~ location_dist) 

otu_dist %>% as.matrix() %>% heatmap()
# significant but weak correlation between proximity and bacterial communtiy structure
  # done with MRM, save output to file
  # give a brief explanation

# get on google scholar: microbiome, distance decay

tax_table(full)
full@sam_data %>% names
full %>% subset_samples(Structure != "Blank")

# This shows phylum distribution
plot_bar(full, fill="Phylum")

# create relative abundance: see 04 and abundance ps_ra: line 120

merge_samples(full, "Structure") %>% 
  transform_sample_counts(fun = function(x){x/sum(x)}) %>% 
  plot_bar(fill = "Phylum")

# can do alpha diversity plots: https://joey711.github.io/phyloseq/plot_richness-examples.html
# can look at overlap of phyla and structure, (facet wrap by structure and maybe microbe)





# regress 2 matrices against each other
  # using MRM: multiple regression on matrices

#### WE WANT TO KNOW:
  # does difference in proximity explain difference in species composition?
  # do fungi and bacteria follow the same distribution patterns?

## RECREATE AN ANALYSIS USING THE DATASET
  # bar charts, ordination plot, plot network
