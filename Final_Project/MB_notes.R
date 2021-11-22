#Microbiome workshop 

# to see how many sequence names are associated with different phyla, use @ to access differnt sets

# MY NOTES
tax_table(ps)[1,]
ps@tax_table[,2] %>% table()

ntaxa(ps)
nsamples(ps)
ps@sam_data$ # to check out columns and relevant info

ps_nonbact <- subset_taxa(ps, Kingdom != "Bacteria")

# can remove the proteobacteria: subsetting 

# MY NOTES END 

# That was for numer 03

# This is for 04
plot_richness(ps, x="Status") #lines 100
# lots of the work is about relative abundance, not absolute
# divide the abundance of X by the sum of the abundance in the sample to get a relative proportion
  # bc the numbers may be inflated by chance of sequencing