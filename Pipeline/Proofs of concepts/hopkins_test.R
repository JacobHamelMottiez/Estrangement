# Author: Your Name
# Date: `r Sys.Date()`
# Description:

library(tidyverse)
library(ggrepel)
library(factoextra)
library(skimr)
library(ape)
library(clValid)
library(cluster)
library(gridExtra)
library(mixtools)
library(ggplot2)
library(patchwork)
library(tidygraph)
library(igraph)

dir <- "C:/Users/jacob/OneDrive - Université Laval/biophilo/Data/pybiblio/SPECIALIZED PHILOSOPHY OF BIOLOGY/" 
spec_philo_bio <- read_csv(paste0(dir, "ARTICLES_SPECIAL_PHILO_BIO.csv"))
refs_spec_philo_bio <- read_csv(paste0(dir, "REFERENCES_SPECIAL_PHILO_BIO.csv"))

refs_spec_philo_bio <-  refs_spec_philo_bio |> 
  group_by(cited_id) |> add_count(cited_id) |> 
  ungroup()

refs_spec_philo_bio <- refs_spec_philo_bio |> 
  filter(n > 5) |> 
  distinct()

spec_philo_bio |> select(citing_id)
# COCITATION  -------------------------------------------------------------
refs_spec_philo_bio <-  refs_spec_philo_bio |> 
  group_by(cited_id) |> add_count(cited_id) |> 
  ungroup()

refs_spec_philo_bio <- refs_spec_philo_bio |> 
  filter(n > 5) |> 
  distinct()


# Create a bipartite incidence matrix
incidence_matrix <- table(refs_spec_philo_bio$citing_id, refs_spec_philo_bio$cited_id)

# Compute cocitation matrix (Reference x Reference)
cocitation_matrix <- t(incidence_matrix) %*% incidence_matrix

# Convert to data frame
cocitation_df <- as.data.frame(as.table(cocitation_matrix))
colnames(cocitation_df) <- c("cited_id_1", "cited_id_2", "weight")

# Remove self-citations
cocitation_df <- cocitation_df %>% filter(cited_id_1 != cited_id_2)
cocitation_df <- cocitation_df |> filter(weight > 5)

## Create a Cocitation Network in igraph
# Convert to graph
g <- graph_from_data_frame(cocitation_df, directed = FALSE)


## Community Detection on the Cocitation Network
# Run Louvain community detection
E(g)$weight <- E(g)$weight^3
louvain <- cluster_louvain(g, resolution = 1)

# Get community membership
V(g)$community <- membership(louvain)
V(g)$weight <- degree(g)

graph <- g
louvain_clusters <- cluster_louvain(graph)
cluster_membership <- membership(louvain_clusters)

# Compute statistics for each cluster
cluster_stats <- data.frame(ClusterID = unique(cluster_membership)) %>%
  rowwise() %>%
  mutate(
    Size = sum(cluster_membership == ClusterID),
    Mean_Degree = mean(degree(graph, v = which(cluster_membership == ClusterID))),
    Modularity_Class = modularity(louvain_clusters),
    Density = edge_density(induced_subgraph(graph, which(cluster_membership == ClusterID))),
    Mean_Clustering_Coeff = mean(transitivity(graph, type = "local")[which(cluster_membership == ClusterID)], na.rm = TRUE)
  )

# Print results
print(cluster_stats)



h <- get_clust_tendency(cluster_stats |> select(Size, Modularity_Class, Density, Mean_Clustering_Coeff), 
                        nrow(cluster_stats)-1,
                        gradient = list(low = "black", mid = "gray", high = "red"))
ggplotly(h)


cluster_stats_dist <- cluster_stats |> dist()
hc_single <- hclust(cluster_stats_dist, 
                    method = "single")


fviz_dend(hc_single)
