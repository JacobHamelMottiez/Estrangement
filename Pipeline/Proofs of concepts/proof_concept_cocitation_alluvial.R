library(igraph)
library(tidygraph)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggalluvial)

# Example dataset with year information
cocitations_time_1 <- data.frame(
  cited_id1 = c("A", "B", "C", "D", "E", "F"),
  year = rep(1990, 6),
  cluster = c(1,1,2,2,2,3)
)

cocitations_time_2 <- data.frame(
  cited_id1 = c("A", "B", "C", "D", "E", "F"),
  year = rep(1995, 6),
  cluster = c(1,5,4,4,5,5)
)

cocitations_time_3 <- data.frame(
  cited_id1 = c("A", "B", "C", "D", "E", "F"),
  year = rep(2000, 6),
  cluster = c(9,6,6,7,7,9)
)


cocitations_time <- rbind(cocitations_time_1, cocitations_time_2, cocitations_time_3)


# Arrange the results
clusters_over_time <- cocitations_time 

# Create an alluvial plot
ggplot(clusters_over_time, aes(x = year, stratum = cluster, alluvium = cited_id1, fill = as.factor(cluster))) +
  geom_flow(stat = "alluvium", aes.bind = TRUE, alpha = 0.6) +
  geom_stratum(alpha = 0.7) +
  scale_fill_brewer(type = "qual", palette = "Set3") +
  theme_minimal() +
  labs(title = "Cluster Changes Over Time", x = "Year", y = "N. refs. Cluster", fill = "Cluster") 
  

