# Load required libraries
library(igraph)
library(dplyr)
library(tidyverse)
library(tidygraph)
library(plotly)

# Simulated bibliographic coupling data (replace this with your actual data)
df_refs <- read_csv("Data/pybiblio/philpapers_data_pyblio_refs_pyblio.csv") |> distinct()
df_art <- read_csv("Data/pybiblio/philpapers_data_pyblio.csv") |> distinct()



clean_references_fct <- function(references) {  
  references$sourcetitle <- toupper(references$sourcetitle)
  references$citing_id <- str_extract(references$citing_eid, "(?<=2-s2\\.0-)\\d+")
  references <- rename(references, cited_id = id, cited_title = title)
  references$citing_eid <- NULL
  references <- rename(references, cited_year = publicationyear)
  return(references)
}

clean_articles_fct <- function(articles){
  articles$citing_id <- str_extract(articles$eid, "(?<=2-s2\\.0-)\\d+")
  articles$eid <- NULL
  articles <- rename(articles, citing_year = coverDate,
                     citing_category = subtypeDescription,
                     citing_title = title)
  articles <-  articles |> mutate(citing_year = as.numeric(format(citing_year, "%Y")))
  }




df_refs_cleaned <- clean_references_fct(df_refs) |> select(cited_id, citing_id, cited_year)
df_refs_cleaned <-  df_refs_cleaned |> group_by(cited_id) |> add_count(cited_id) |> ungroup()
df_refs_cleaned <- df_refs_cleaned |> filter(n > 10)

df_art_cleaned <- clean_articles_fct(df_art) |> select(citing_year, citing_id)
#df_art_cleaned <- df_art_cleaned |> group_by(citing_id) |> add_count(citing_id) |> ungroup()




complete_df <- left_join(df_refs_cleaned, df_art_cleaned, by = "citing_id")
# Create graph from edges



g <- graph_from_data_frame(complete_df, directed = FALSE)

graph <- graph_from_data_frame(complete_df, directed = FALSE)
co_graph <- cocitation(graph) |> graph_from_adjacency_matrix(mode = "undirected")

# Louvain clustering
leiden <- cluster_louvain(co_graph, resolution = 2)

# Add cluster labels to nodes
df_clusters <- data.frame(
  id = V(co_graph)$name,
  cluster = membership(leiden)
)

complete_df <- complete_df %>%
  left_join(df_clusters, by = c("cited_id" = "id")) %>%
  rename(cited_cluster = cluster) %>%
  left_join(df_clusters, by = c("citing_id" = "id")) %>%
  rename(citing_cluster = cluster)


complete_df <- complete_df %>%
  mutate(
    cited_decade = paste0(floor(cited_year / 10) * 10, "s"),
    citing_decade = paste0(floor(citing_year / 10) * 10, "s")
  )

df_counts <- complete_df %>% select(-n) |> 
  count(cited_decade, citing_decade, cited_cluster, citing_cluster, name = "n") %>%
  rename(decade = cited_decade, cluster = cited_cluster)


library(ggalluvial)
library(ggplot2)

ggplot(df_counts, aes(x = decade, stratum = cluster, alluvium = cluster, y = n, fill = as.factor(cluster))) +
  geom_flow(stat = "alluvium", alpha = 0.8) +  # Creates the smooth flow effect
  geom_stratum(color = "black") +              # Creates blocks at each decade
  scale_fill_viridis_d() +                     # Use a color palette
  theme_minimal() +
  labs(
    title = "Evolution of Louvain Clusters Over Decades",
    x = "Decade",
    y = "Number of References",
    fill = "Cluster"
  )



# Load necessary libraries
library(igraph)
library(tidyverse)
library(ggalluvial)

# Sample Data (Replace with your actual dataframe)
df <- complete_df  # Assuming you have columns: citing_id, cited_id, citing_year, cited_year

# Create Decade Column
df <- df %>%
  mutate(citing_decade = floor(citing_year / 10) * 10,
         cited_decade = floor(cited_year / 10) * 10)

df <-  df |> filter(n>100) 
df |> distinct(citing_cluster)

# Build Citation Network
g <- graph_from_data_frame(df, directed = FALSE)

# Apply Louvain Clustering
louvain_clusters <- cluster_louvain(g, resolution = 2)
df$cluster <- membership(louvain_clusters)[df$citing_id]

# Aggregate Data by Decade and Cluster
alluvial_data <- df %>%
  count(citing_decade, cluster) %>%
  rename(decade = citing_decade, count = n)

# Create Alluvial Plot
ggplot(alluvial_data, aes(x = factor(decade), y = count, stratum = factor(cluster), alluvium = factor(cluster), fill = factor(cluster))) +
  geom_flow(alpha = 0.5) +
  geom_stratum(alpha = 1) +
  theme_minimal() +
  labs(x = "Decade", y = "Number of Citations", fill = "Cluster") +
  theme(legend.position = "bottom") +
  scale_fill_viridis_d()














# Load necessary libraries
library(igraph)
library(tidyverse)
library(ggalluvial)

# Sample Data (Replace with your actual dataframe)
df <- complete_df  # Assuming columns: citing_id, cited_id, citing_year, cited_year

# Create Decade Column
df <- df %>%
  mutate(decade = floor(citing_year / 10) * 10)


df <-  df |> select(citing_year, citing_decade, citing_cluster) 
df$citing_cluster <- as.character(df$citing_cluster)
df <- df |> count(citing_decade, citing_cluster)

x <- ggplot(df, aes(x = citing_decade, y = n, group = citing_cluster, color = citing_cluster)) +
  geom_point() + 
  geom_line() 


ggplotly(x)

