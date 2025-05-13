# Author: Your Name
# Date: `r Sys.Date()`
# Description:

library(tidyverse)
library(tidyverse)
library(plotly)
library(readxl)
library(ggplot2)
library(tidygraph)
library(igraph)
library(ggalluvial)
library(ggsankey) # devtools::install_github("davidsjoberg/ggsankey")
library(shadowtext)
library(Matrix)
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


# Convert numeric membership to letters
community_labels <- LETTERS[as.numeric(factor(membership(louvain)))]
E(g)$color <- community_colors[match(ends(g, E(g))[,1], V(g)$name)]


# Assign as vertex attribute
V(g)$community <- community_labels

palette <- viridis::viridis(n = 58, begin =  0.1, option = "A") %>% # creating a color palette
  sample()

community_colors <- palette[membership(louvain)]


# Generate the node list
node_data <- data.frame(
  id = V(g)$name,
  community = V(g)$community,
  node_weight = V(g)$weight,
  node_color = community_colors
)


edge_list <- as_edgelist(g) 
edge_data <- as_tibble(edge_list) |> 
  rename(from = V1, to = V2) |> 
  mutate(edge_color = E(g)$color,
         edge_weight = E(g)$weight^3
         )  # Add edge colors
write_csv(edge_data, paste0(dir,"edges.csv"))


# Get back some info 
data <- rename(refs_spec_philo_bio, id = cited_id)
data <- data |> select(id, cited_title, authors, cited_year, sourcetitle)



node_data <- node_data |> left_join(data, by = "id") 
node_data <- node_data |>  mutate(info = paste0(authors, ", ", cited_year, ", ", cited_title,", ", sourcetitle)) |> distinct()
write_csv(node_data, paste0(dir,"node.csv"))

alluvial_data = node_data |> select(id,community, cited_year) |> distinct()

#nodes
node_data_cg <- node_data|> distinct() |> select(id, community, node_weight, info, node_color, cited_year)
node_data_cg <- rename(node_data_cg,  category = node_color, label = info, value = node_weight)
node_data_cg <- node_data_cg |> filter(label != "NANANANA") |> arrange(desc(value))
node_data_cg$cited_year <- as.character(node_data_cg$cited_year)

write_csv(node_data_cg, paste0(dir, "nodes_data_cg.csv"))

#edges
edge_data_cg <- rename(edge_data, source= from, target = to)
write_csv(edge_data_cg,paste0(dir, "edges_data_cg.csv"))



# ALLUVIAL  ---------------------------------------------------------------

full <- left_join(refs_spec_philo_bio |> select(cited_id, citing_id, cited_year), spec_philo_bio |> select(citing_year, citing_id), by = "citing_id")

full <- full |> mutate(decade = citing_year - (citing_year %% 10))

decade_2000 <- full |> filter(decade == 2000)
decade_2010 <- full |> filter(decade == 2010)
decade_2020 <- full |> filter(decade == 2020)



# FCT_cocit ---------------------------------------------------------------
# Create a bipartite incidence matrix

fct_cocit <- function(data, weight_filter, resolution_param, year){
  
incidence_matrix <- table(data$citing_id, data$cited_id)

# Compute cocitation matrix (Reference x Reference)
cocitation_matrix <- t(incidence_matrix) %*% incidence_matrix

# Convert to data frame
cocitation_df <- as.data.frame(as.table(cocitation_matrix))
colnames(cocitation_df) <- c("cited_id_1", "cited_id_2", "weight")

# Remove self-citations
cocitation_df <- cocitation_df %>% filter(cited_id_1 != cited_id_2)
cocitation_df <- cocitation_df |> filter(weight > weight_filter)

## Create a Cocitation Network in igraph
# Convert to graph
g <- graph_from_data_frame(cocitation_df, directed = FALSE)


## Community Detection on the Cocitation Network
# Run Louvain community detection
E(g)$weight <- E(g)$weight^3
louvain <- cluster_louvain(g, resolution = resolution_param)
print(louvain)
# Get community membership
V(g)$community <- membership(louvain)
V(g)$weight <- degree(g)

community_labels <- LETTERS[as.numeric(factor(membership(louvain)))]
E(g)$color <- community_colors[match(ends(g, E(g))[,1], V(g)$name)]


# Assign as vertex attribute
V(g)$community <- community_labels

palette <- viridis::viridis(n = 58, begin =  0.1, option = "A") %>% # creating a color palette
  sample()

community_colors <- palette[membership(louvain)]


# Generate the node list
node_data <- data.frame(
  id = V(g)$name,
  community = V(g)$community,
  node_weight = V(g)$weight,
  node_color = community_colors
)


edge_list <- as_edgelist(g) 
edge_data <- as_tibble(edge_list) |> 
  rename(from = V1, to = V2) |> 
  mutate(edge_color = E(g)$color,
         edge_weight = E(g)$weight^3
  )  # Add edge colors

write_csv(edge_data, paste0(dir, "edges_",year, ".csv"))


# Get back some info 
data <- rename(refs_spec_philo_bio, id = cited_id)
data <- data |> select(id, cited_title, authors, cited_year, sourcetitle)



node_data <- node_data |> left_join(data, by = "id") 
node_data <- node_data |>  mutate(info = paste0(authors, ", ", cited_year, ", ", cited_title,", ", sourcetitle)) |> distinct()
write_csv(node_data, paste0(dir,"node_", year, ".csv"))

}

fct_cocit(decade_2000, 5, 0.25, 2000)
fct_cocit(decade_2010, 5, 0.25, 2010)
fct_cocit(decade_2020, 5, 0.25, 2020)



# Convert numeric membership to letters
edges_2000 <- read_csv("Data/pybiblio/SPECIALIZED PHILOSOPHY OF BIOLOGY/edges_2000.csv")
edges_2010 <- read_csv("Data/pybiblio/SPECIALIZED PHILOSOPHY OF BIOLOGY/edges_2010.csv")
edges_2020 <- read_csv("Data/pybiblio/SPECIALIZED PHILOSOPHY OF BIOLOGY/edges_2020.csv")

node_2000 <- read_csv("Data/pybiblio/SPECIALIZED PHILOSOPHY OF BIOLOGY/node_2000.csv")
node_2010 <- read_csv("Data/pybiblio/SPECIALIZED PHILOSOPHY OF BIOLOGY/node_2010.csv")
node_2020 <- read_csv("Data/pybiblio/SPECIALIZED PHILOSOPHY OF BIOLOGY/node_2020.csv")



edges_2000 <- edges_2000 |> mutate(decade = 2000)
edges_2010 <- edges_2010 |> mutate(decade = 2010)
edges_2020 <- edges_2020|> mutate(decade = 2020)

total_edges <- rbind(edges_2000, edges_2010, edges_2020) |> select(from, to, decade)


node_2000 <- node_2000 |> mutate(decade = 2000, dynamic_community = paste0(community, decade))
node_2010 <- node_2010 |> mutate(decade = 2010, dynamic_community = paste0(community, decade))
node_2020 <- node_2020|> mutate(decade = 2020, dynamic_community = paste0(community, decade))
total_node <- rbind(node_2000,node_2010, node_2020) |> select(id, dynamic_community, decade, cited_year)

total_node |> filter(id == "0004149207")


df <- total_node %>%
  arrange(decade) %>%
  group_by(id) %>%
  mutate(
    next_decade = lead(decade),
    next_dynamic_community = lead(dynamic_community)
  ) %>%
  ungroup()

df_2020 <- df |> filter(decade == 2020)

df_filtered = df |> filter(!is.na(next_decade))
df_filtered <- rbind(df_filtered, df_2020)


ggplot(df_filtered, aes(x = decade, 
               next_x = next_decade, 
               node = dynamic_community, 
               next_node = next_dynamic_community,
               fill = factor(dynamic_community),
               label = dynamic_community)) +
  geom_sankey() + 
  geom_sankey_label()

df_filtered <- df_filtered |> group_by(dynamic_community) |> add_count(dynamic_community) |> ungroup()

df_filtered <- df_filtered %>%
  mutate(
    source = as.numeric(factor(dynamic_community)) - 1,  # 0-based index
    target = as.numeric(factor(next_dynamic_community)) - 1
  )

fig <- plot_ly(
  type = "sankey",
  orientation = "h",
  
  node = list(
    label = df_filtered$source,
    pad = 50,
    thickness = 10,
    line = list(
      color = "black",
      width = 0.5
    )
  ),
  
  link = list(
    source = df_filtered$source,
    target = df_filtered$target,
    value = df_filtered$n
  )
)
fig <- fig %>% layout(
  title = "Basic Sankey Diagram",
  font = list(
    size = 20
  ))
fig

