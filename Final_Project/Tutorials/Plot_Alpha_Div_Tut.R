library("phyloseq"); packageVersion("phyloseq")
library("ggplot2"); packageVersion("ggplot2")

data("GlobalPatterns")

theme_set(theme_bw())
pal = "Set1"
scale_colour_discrete <-  function(palname=pal, ...){
  scale_colour_brewer(palette=palname, ...)
}
scale_fill_discrete <-  function(palname=pal, ...){
  scale_fill_brewer(palette=palname, ...)
}

# PREPARING data: can be valuable not to trim early on; hidden richness
GP <- prune_species(speciesSums(GlobalPatterns) > 0, GlobalPatterns)

# GRAPHING the data
plot_richness(GP)

# you can add specific measurements to the plot
plot_richness(GP, measures=c("Chao1", "Shannon"))

# you can specify a variable for the x-axis, which helps visulization when it's experimentally meanginaful (and categorical)
plot_richness(GP, x = "SampleType", measures=c("Chao1", "Shannon"))

# you can use an external variable when you first define it
sampleData(GP)$human <- getVariable(GP, "SampleType") %in% c("Feces", "Mock", "Skin", "Tongue")

# then you can map new var to x-axis and color by previous var
plot_richness(GP, x="human", color="SampleType", measures=c("Chao1", "Shannon"))

# you can merge variables, like sampletype in this example
GPst = merge_samples(GP, "SampleType")
# repair variables that were damaged during merge (coerced to numeric)
sample_data(GPst)$SampleType <- factor(sample_names(GPst))
sample_data(GPst)$human <- as.logical(sample_data(GPst)$human)

# plot envir-merged version by storing the graph, then adding a geom_point layer with size and transparency
p = plot_richness(GPst, x="human", color="SampleType", measures=c("Chao1", "Shannon"))
p + geom_point(size=5, alpha=0.7)

# some layers in ggplot2 may be distracting, and can be removed

# check the layers present
p$layers

# it looks like 1 has the original points that are distracting, so you can remove them
p$layers <- p$layers[-1]
p + geom_point(size=5, alpha=0.7)
  # this new graph is small-dot free


