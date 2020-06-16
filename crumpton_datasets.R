# AUTHOR: BRADY NAHKALA
# LAST REVISED: 06 MAY 2020
# PURPOSE: host data from following pub for use in prairie pothole tool:

# PAPER(S):
# Green, McDeid, Crumpton (2019). Runoff storage potential of drained upland depressions on 
# the Des Moines Lobe of Iowa. JAWRA.
# 
# McDeid, Green and Crumpton (2019). Morphology of drained upland depressions on the Des 
# Moines Lobe of Iowa. Wetlands.
# 
# LIBRARY =====
library(dplyr)
library(tidyr)

# From McDeid =====

# Figure 7:
# N = 2.7 * 10^5*Amax ^-2.51
# N = 836.9 * Hmax ^-4.23

# Table 1:
morphology.McDeid <- read.csv(file = "Crumpton_data.csv", header=T, sep=",")
morphology.McDeid <- morphology.McDeid[ , c(1, 6:13)]
morphology.McDeid <- morphology.McDeid[ , c(1:3, 5:7, 9)]
colnames(morphology.McDeid) <- c("Region", "Mean Maximum Area (ha)","Median Maximum Area (ha)","Minimum Maximum Area (ha)",
                                 "Mean Maximum depth (m)","Median Maximum depth (m)","Minimum Maximum depth (m)")

 