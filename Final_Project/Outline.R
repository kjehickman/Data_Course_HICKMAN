library(tidyverse); packageVersion("tidyverse")
library(phyloseq); packageVersion("phyloseq")
library(ShortRead); packageVersion("ShortRead")
library(ggplot2)
library(geosphere)
library(vegan)
library(microbiome)
library(BiocManager)
library(devtools) 
install_github("microbiome/microbiome")

plot_bar2 <- function (physeq, x = "Sample", y = "Abundance", fill = NULL, 
                       title = NULL, facet_grid = NULL) 
{
  mdf = psmelt(physeq)
  p = ggplot(mdf, aes_string(x = x, y = y, fill = fill))
  p = p + geom_bar(stat = "identity", position = "stack")
  p = p + theme(axis.text.x = element_text(angle = -90, hjust = 0))
  if (!is.null(facet_grid)) {
    p <- p + facet_grid(facet_grid)
  }
  if (!is.null(title)) {
    p <- p + ggtitle(title)
  }
  return(p)
}

# QUESTIONS... ####
  # script 04, line 84, formula explained?


#### Final Project Outline ####
## Start .Rmd
### Intro to analysis, samples, and hypothesis / prediction ####

### methods?

# Setup ####

# load data
full <- readRDS("./bact_and_fungi_clean_ps_object")

# look at sample data
full@sam_data %>% View

### A1: how similar are the samples based on location? ####
## make 2 matricies: location and sample composition 
## run mantel test: ape::mantel.test(loc_dist, otu_dist)
# plot the lcoations and taxa?

# ANOVA permutational analysis variance for ordinance tables
      vegan::adonis(otu_table(full) ~ full@sam_data$Status) 

lat <- full@sam_data$Lat %>% as.numeric()
lon <- full@sam_data$Lon %>% as.numeric()

# make df of distance of geographical similarity
location_dist <- data.frame(lat, lon) %>% dist()

# make a distance matrix using dist(): difference of community similarity
otu_dist <- otu_table(full) %>% dist()
otu_dist <- vegan::vegdist(otu_table(full), na.rm = TRUE)

MRM1 <- ecodist::MRM(otu_dist ~ location_dist)

# double-check this
sink("./MRM_otu_dist.txt")
ecodist::MRM(otu_dist ~ location_dist)
sink(NULL)

otu_dist %>% as.matrix() %>% heatmap()

# sink and export table of mantel test and mrm: table 1 in Lee et al. 2020

# significant but weak correlation between proximity and bacterial communitiy structure
# done with MRM, save output to file
# give a brief explanation


# plot for kingdom level taxa

tax_table(full)
full@sam_data %>% names
full_structures <- full %>% subset_samples(Structure != "Blank")

# this shows phylum abundance by structure
merge_samples(full_structures, "Structure") %>% 
  transform_sample_counts(fun = function(x){x/sum(x)}) %>% 
  plot_bar(fill = "Phylum")
  # MAY go better in A2?

# This shows phylum distribution
plot_bar(full, fill="Phylum")


# Find and remove non-bacteria ####
full_nonarch <- subset_taxa(full, Kingdom != "Archaea")

# quick plot to look at kingdom-level taxonomy
full %>% transform_sample_counts(function(x){x/sum(x)}) %>%
  plot_bar(fill="Kingdom")
ggsave("./output/figs/16S_Kingdom-Level_Taxonomic_Proportions.png",dpi=300) # save figure for later use

# same plot, but non-bacteria, for sanity check
full_nonarch %>% 
  transform_sample_counts(function(x){x/sum(x)}) %>%
  plot_bar(fill="Kingdom")


### A2: Alpha diversity and relative class abundance v location ####
## looks like fig 3 in  Lee et al. 2019
## stacked bar plot of relative class abundance v location collected with a legend of taxa
  ## facet_wrap by structure

full_nonarch %>% 
  subset_samples(Host != "Blank") %>% 
  merge_samples("Host") %>% 
  transform_sample_counts(function(x){x/sum(x)}) %>% 
  plot_bar(fill="Phylum") +
  facet_wrap(~"Microbe")

# to merge based on multiple variables, make a new merging variable
newmerge <- paste0(full@sam_data$Host, "_", full@sam_data$Structure, "_", full@sam_data$Microbe)

full@sam_data$newmerge <- newmerge

full2 <- full %>% 
  subset_samples(Host != "Blank") %>% 
  merge_samples("newmerge") %>% 
  transform_sample_counts(function(x){x/sum(x)}) %>% 
  plot_bar(fill="Phylum") +
  scale_fill_viridis_d()

full2 <- full %>% 
  subset_samples(Host != "Blank") %>% 
  merge_samples("newmerge") %>% 
  transform_sample_counts(function(x){x/sum(x)})

full2@sam_data$Host <- row.names(full2@sam_data) %>% str_split("_") %>% map_chr(1)
full2@sam_data$Structure <- row.names(full2@sam_data) %>% str_split("_") %>% map_chr(2)
full2@sam_data$Microbe <- row.names(full2@sam_data) %>% str_split("_") %>% map_chr(3)


# you can see other variables were damaged and need to be repaired (since it didn't know how to merge)
# fixed with that code

full2 %>% 
  plot_bar2(x="Host", fill="Phylum")+
  scale_fill_viridis_d()+
  facet_wrap(~Structure)

bact_plot <- full2 %>% 
  subset_taxa(Kingdom =="Bacteria") %>% 
  plot_bar2(x="Host", fill="Phylum")+
  scale_fill_viridis_d()+
  facet_wrap(~Structure)

fung_plot <- full2 %>% 
  subset_taxa(Kingdom =="Fungi") %>% 
  plot_bar2(x="Host", fill="Phylum")+
  scale_fill_viridis_d()+
  facet_wrap(~Structure)

library(patchwork)
# this will combine the plots to show phyla sorted by kingdom--work done above
bact_plot / fung_plot


# setting up relative abundance
  # create relative abundance: see 04 and abundance ps_ra: line 120

# MB follow-along
estimate_richness(full, measures = "Observed") # explain output
names(sample_data(full))

full@sam_data %>% View

# tutorial
GP <- prune_species(speciesSums(full) > 0, full)
GP %>% 
  subset_samples(Host != "Blank") %>%
  plot_richness(x = "Structure", 
              measures=c("Chao1", "Shannon"),
              sortby = "Observed")
  # what is Chao1?
?plot_richness()

# colored by kingdom/microbe 
plot_richness(full, measures = c("Observed","Shannon"), 
              color = "Microbe", 
              sortby = "Observed") +
  theme(axis.text.x = element_blank())
# colored by structure
plot_richness(full, 
              measures = c("Observed","Shannon"), 
              color = "Structure", 
              sortby = "Observed") +
  theme(axis.text.x = element_blank())

# plot, grouped by Microbe with added boxplot
plot_richness(full, 
              x = "Microbe",
              measures = c("Observed","Shannon"), 
              color = "Structure", 
              sortby = "Observed") +
  geom_boxplot(alpha = .5) +
  theme_minimal()
?estimate_richness()
full@sam_data %>% View
# use estimate_richness to do th "glm" and find out significance
  # it looks like fungi are more speciose, and slightly more diverse
# CREATE FOLDERS FOR THIS
ggsave("./output/figs/alpha_diversity_boxplot.png",dpi=300,height = 4,width = 6)
# transform raw counts to relative abundance ####
full_ra <- transform_sample_counts(full, fun = function(x){x/sum(x)})

# Beta Diversity

# Ordination
dca <- ordinate(full_ra)
plot_ordination(full_ra,dca,color = "Structure") 
?ordinate()
(
  ord1 <- plot_ordination(full_ra,dca,color = "Structure",shape="Microbe") +
    geom_point(size=4)  + theme_minimal() +
    theme(legend.position = "top") +
    labs(title = "DCA - Bray")
)

# to test this closeness of ordination, use vegan::adonis



## permANOVA showing stat sig for bacterial community, as a function of location and structure
  # then show for fungi impact
  # these will be tables


### A3: anova test? on the stat sig of structure for: ####
## bateria,
## then for fungi, 
## then for them both interacting with forumla *
## make graph to show distributions of both?

### A4: unique taxa (by ESV (exact sequence variance)) by plant part in venn diagram ####
## show fungal and bacterial species only
# Lee et al. 2019, fig 4

### A5: show each locations individual diversity in fungi/bacteria ####
## use scatter plot, wrap by structure
## communities (fungi/bacteria) by structure, color by site
## barplot showing phyla assumbly by structure, all locations combined
  # could use heatmpa to show class by structure

#### A6..?  ####
### Shannon diversity box plot by structure and/or for location
## Alpha measures?
  # get table of alpha diversity measures
  estimate_richness(ps) # explain output
  names(sample_data(ps))
 # plot alpha diversity for every sample
  plot_richness(ps, 
               measures = c("Observed","Shannon","Simpson"), 
                color = "Status", 
                sortby = "Observed") +
    theme(axis.text.x = element_blank())
  
  # plot, grouped by Status with added boxplot
  plot_richness(ps, 
                x = "Status",
                measures = c("Observed","Shannon","Simpson"), 
                color = "Status", 
                sortby = "Observed") +
    geom_boxplot(alpha = .5) +
    theme_minimal()
  ggsave()
## fungi vs bacteria: which are more diverse/speciose? And where?
## show phylogenetic trees for fungi / bacteria
  # access the phylogenetic tree
  phy_tree(ps)
  plot_tree(ps,color="Class")
  # this tree sucks, but it was fast to build 
  # (you could spend more time refining and selecting a tree in script 02)


### Discussion / comparison ####

### Conclusion