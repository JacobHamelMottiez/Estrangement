# Author: Jacob Hamel-Mottiez
# Date: `r Sys.Date()`
# Description: This file make an alluvial based on a cocitation network. It enables to
# keep track of cluster evolution through the years.



# PACKAGES ----------------------------------------------------------------
library(tidyverse)
library(networkflow)
library(stopwords)
library(ggalluvial)
library(plotly)
library(viridis)
library(ggrepel)
library(ggsankey)

# TOY NETWORK -----------------------------------------------------------------
# Data
nodes <- networkflow::Nodes_stagflation |>
  dplyr::rename(ID_Art = ItemID_Ref) |>
  dplyr::filter(Type == "Stagflation")

references <- networkflow::Ref_stagflation |>
  dplyr::rename(ID_Art = Citing_ItemID_Ref)

# Network construction
temporal_networks <- networkflow::build_dynamic_networks(nodes = nodes,
directed_edges = references,
source_id = "ID_Art",
target_id = "ItemID_Ref",
time_variable = "Year",
cooccurrence_method = "coupling_similarity",
time_window = 20,
edges_threshold = 1,
overlapping_window = TRUE,
filter_components = TRUE,
verbose = FALSE)

temporal_networks <- networkflow::add_clusters(temporal_networks,
objective_function = "modularity",
clustering_method = "leiden",
verbose = FALSE)

temporal_networks <- networkflow::merge_dynamic_clusters(temporal_networks,
cluster_id = "cluster_leiden",
node_id = "ID_Art",
threshold_similarity = 0.51,
similarity_type = "partial")

temporal_networks <- networkflow::name_clusters(graphs = temporal_networks,
method = "tf-idf",
name_merged_clusters = TRUE,
cluster_id = "dynamic_cluster_leiden",
text_columns = "Title",
nb_terms_label = 5,
clean_word_method = "lemmatise")

temporal_networks <- networkflow::color_networks(graphs = temporal_networks,
column_to_color = "dynamic_cluster_leiden",
color = NULL)

alluv_dt <- networkflow::networks_to_alluv(temporal_networks,
intertemporal_cluster_column = "dynamic_cluster_leiden",
node_id = "ID_Art")

alluv_dt[1:5]


ggplot(alluv_dt, aes(x = window, y= y_alluv, stratum = dynamic_cluster_leiden, alluvium = ID_Art, fill = color, label = dynamic_cluster_leiden)) +
  geom_stratum(alpha =1, size=1/10) +
  geom_flow() +
  theme(legend.position = "none") + 
  theme_minimal() +
  scale_fill_identity() + labs(y = "Proportion of Economics", x = "Time Window of Economics") +
  ggrepel::geom_label_repel(stat = "stratum", size = 2) 



# REAL NETWORK ------------------------------------------------------------
dir <- "C:/Users/jacob/OneDrive - Université Laval/biophilo/Data/pybiblio/SPECIALIZED PHILOSOPHY OF BIOLOGY/" 
spec_philo_bio <- read_csv(paste0(dir, "ARTICLES_SPECIAL_PHILO_BIO.csv")) 
refs_spec_philo_bio <- read_csv(paste0(dir, "REFERENCES_SPECIAL_PHILO_BIO.csv"))
refs_spec_philo_bio <- refs_spec_philo_bio |> group_by(cited_id) |> add_count(cited_id) |> arrange(desc(n)) |> select(-position) |> distinct()

spec_philo_bio_filtered <- spec_philo_bio |> select(citing_title, authkeywords, citing_id, citing_year) |> rename(ID_Art = citing_id, Year = citing_year)
refs_spec_philo_bio_filtered <- refs_spec_philo_bio |> select(cited_id, citing_id, cited_year) |> rename(ID_Art = citing_id, ItemID_Ref = cited_id, Year = cited_year)


# Network building
temporal_networks <- networkflow::build_dynamic_networks(nodes = spec_philo_bio_filtered,
                                                         directed_edges = refs_spec_philo_bio_filtered,
                                                         source_id = "ID_Art",
                                                         target_id = "ItemID_Ref",
                                                         time_variable = "Year",
                                                         cooccurrence_method = "coupling_similarity",
                                                         time_window = 10,
                                                         edges_threshold = 1,
                                                         overlapping_window = TRUE,
                                                         filter_components = TRUE,
                                                         verbose = FALSE)
# Clusters
temporal_networks <- networkflow::add_clusters(temporal_networks,
                                               objective_function = "modularity",
                                               clustering_method = "louvain",
                                               verbose = FALSE)

# Merge similar clusters
temporal_networks <- networkflow::merge_dynamic_clusters(temporal_networks,
                                                         cluster_id = "cluster_louvain",
                                                         node_id = "ID_Art",
                                                         threshold_similarity = 0.51,
                                                         similarity_type = "partial")

# Name the clusters
temporal_networks <- networkflow::name_clusters(graphs = temporal_networks,
                                                method = "tf-idf",
                                                name_merged_clusters = TRUE,
                                                cluster_id = "dynamic_cluster_louvain",
                                                text_columns = "citing_title",
                                                nb_terms_label = 5,
                                                clean_word_method = "lemmatise")


# Color Network 
temporal_networks <- networkflow::color_networks(graphs = temporal_networks,
                                                 column_to_color = "dynamic_cluster_louvain",
                                                 color = NULL)

alluv_dt <- networkflow::networks_to_alluv(temporal_networks,
                                           intertemporal_cluster_column = "dynamic_cluster_louvain",
                                           node_id = "ID_Art")


# Visualization (alluvial)
ggplot(alluv_dt, aes(x = window, y= y_alluv, stratum = dynamic_cluster_louvain, alluvium = ID_Art, fill = color, label = dynamic_cluster_louvain)) +
  geom_stratum(alpha =1, size=1/10) +
  geom_flow() +
  theme(legend.position = "none") + 
  theme_minimal() +
  scale_fill_identity() + labs(y = "Proportion of Clusters", x = "Time Window") +
  ggrepel::geom_label_repel(stat = "stratum", size = 2)


# Save data
alluv_dt |> select(dynamic_cluster_louvain, cluster_label) |> distinct()
alluv_dt |> write_csv(paste0("C:/Users/jacob/OneDrive - Université Laval/biophilo/Data/alluv_dt.csv"))


# Visualization (sankey)
sankey <- alluv_dt |> select(window, dynamic_cluster_louvain,share_cluster_window) |> distinct()
s <- ggplot(sankey, aes(x = window,
               node = dynamic_cluster_louvain,
               fill = dynamic_cluster_louvain,
               value = share_cluster_window)) +
  geom_sankey_bump(space = 0, type = "alluvial", color = "transparent", smooth = 6) +
  theme_sankey_bump(base_size = 16) +
  labs(x = NULL,
       y = "share_cluster_window",
       fill = NULL,
       color = NULL) +
  theme(legend.position = "bottom") +
  labs(title = "GDP development per continent")
ggplotly(s)



test <- sankey |> filter(window == "1979-1988" |window == "1989-1998" | window == "1999-2008" | window == "2009-1999")
sankey |> distinct(window)

s2 <- ggplot(test, aes(x = window,
                        node = dynamic_cluster_louvain,
                        fill = dynamic_cluster_louvain,
                        value = share_cluster_window)) +
  geom_sankey_bump(space = 0, type = "alluvial", color = "transparent", smooth = 6) +
  theme_sankey_bump(base_size = 16) +
  labs(x = NULL,
       y = "GDP ($ bn)",
       fill = NULL,
       color = NULL) +
  theme(legend.position = "bottom") +
  labs(title = "GDP development per continent")
ggplotly(s2)


# Saving file to test visualization in Rawdata 2.0 website. 
test |> write_csv(paste0("C:/Users/jacob/OneDrive - Université Laval/biophilo/Data/test_rawgraph.csv"))
