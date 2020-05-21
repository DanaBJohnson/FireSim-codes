# --------------------
# Title: first-look-seq-data
# Author: Dana Johnson
# Date: 2020-April-28
#
# Purpose: Make figures from seq data 


# --------------------
library(dplyr)
library(phyloseq)
library(ggplot2)

library(devtools)
# install_github("adw96/breakaway")
# install_github("adw96/DivNet")
library(DivNet)
# --------------------

setwd("C:/Users/danab/Box Sync/WhitmanLab/Projects/WoodBuffalo/FireSim2019/data/R_play_data")

# --------------------
# Import necessary seq data
ps <- readRDS('ps.play')
colnames(ps@sam_data)
colnames(ps@tax_table)
rank_names(ps)

# Take a quick look at how many sequences are in each sample.
# Define variable n_seq as the column sums from the OTU table.
n_seq = sample_sums(ps)
n_seq

# Creating a dataframe of the sample names
n_seq = data.frame(names(n_seq),n_seq)
n_seq

# Naming the columns
colnames(n_seq)=c("Sample.ID","Total.seq")

# Look at n_seq
min(n_seq$Total.seq)

# Lowest seq number = 

#Let's look at this in histogram form  to confirm. 
p = qplot(n_seq$Total.seq, geom="histogram", binwidth=100)
p = p + scale_x_continuous(labels = function(x) sprintf("%g", x))
p


# --------------------
# Transform to relative abundance (fraction of total community)
ps.norm <- transform_sample_counts(ps, function(x) x / sum(x) )
colnames(ps.norm@sam_data)
head(ps.norm@sam_data)
ps.norm@sam_data

# To test: whether there are signficant changes in the community composition for specific taxa or groups
# create "melted" phyloseq object
ps.melt <- psmelt(ps.norm)









# Look at the number of unique phyla in the dataset.
phylum_unique <- unique(ps.melt$Phylum)
phylum_unique
# 33



head(ps.melt)
colnames(ps.melt)

# Save this object so that next time we don't have to run the melt function. 
saveRDS(ps.melt, '')



# --------------------
# 
# --------------------
# --------------------
# --------------------
# --------------------
# --------------------