#############################################################################################################
#                                                                                                           #
# Plots distances within the Indo-European (IE) family and within a distance list between random languages  #
#                                                                                                           #
#############################################################################################################

setwd("C:/Sprachgenetik/R")

df2 <- read.csv("Distances_within_IE.csv")  # CSV with distances between all IE languages
dataset <- as.numeric(df2[,1])
hist(dataset,breaks = 60,col=rgb(1,0,0,0.5),xlim = range(0,100),ylim = range(0,2500),xlab=”Pairwise comparisons' distance values”)

df3 <- read.csv("Distances_within_Dummies.csv")  # CSV with distances between Dummy languages -> same number as IE!
dataset2 <- as.numeric(df3[,1])
hist(dataset2,breaks = 17,col=rgb(0,0,1,0.5),add=TRUE)