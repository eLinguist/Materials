#############################################################################################################
#                                                                                                           #
# Plots distances within the Indo-European (IE) family and within a distance list between random languages  #
#                                                                                                           #
#############################################################################################################

setwd("C:/Forschung")

df <- read.csv("Distances_within_IE.csv")  # CSV with distances between all IE languages
df2 <- read.csv("Distances_within_Dummies.csv")  # CSV with distances between Dummy languages -> same number as IE!
df <- cbind(a = "Indo-European", df)
df2 <- cbind(a = "Random languages", df2)
df <- rbind(df,df2)

p <- ggplot(df, aes(x=Dist,fill=a,color=a)) +
  geom_histogram(alpha=0.5, position="identity") +
  scale_color_manual(values=c("#999999", "#E69F00")) +
  labs(title="Distribution of pairwise comparisons: between IE languages vs. between random languages",x="Pairwise comparisons values (distances)", y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.title=element_blank())
p