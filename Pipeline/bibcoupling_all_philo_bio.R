# Load required libraries
library(igraph)
library(dplyr)
library(tidyverse)
library(tidygraph)
library(data.table)

# Simulated bibliographic coupling data (replace this with your actual data)
df <- read_csv("C:/Users/jacob/OneDrive - Université Laval/biophilo/Data/pybiblio/SPECIALIZED PHILOSOPHY OF BIOLOGY/BIOLOGY_AND_PHILOSOPHY_refs_pyblio.csv")
df_art <- read_csv("C:/Users/jacob/OneDrive - Université Laval/biophilo/Data/pybiblio/SPECIALIZED PHILOSOPHY OF BIOLOGY/BIOLOGY_AND_PHILOSOPHY.csv")

clean_references_fct <- function(references) {  
  references$sourcetitle <- toupper(references$sourcetitle)
  references <- rename(references, cited_id = id, citing_id = citing_eid, cited_title = sourcetitle)
  references$citing_id <- str_extract(references$citing_id, "(?<=2-s2\\.0-)\\d+")
  references <- rename(references, cited_year = publicationyear)
  return(references)
}

data <- clean_references_fct(df) |> 
  group_by(cited_id) |> 
  add_count(cited_id) |> 
  ungroup() |> 
  select(citing_id, cited_id, authors, title, cited_year, n)


most_common <- function(x) {
  if (all(is.na(x))) return(NA)  # Handle all NA cases
  x[which.max(tabulate(match(x, unique(x))))]  # Find most frequent value
}

clean_references2 <- function(df) {
  df %>%
    group_by(cited_id) %>%
    mutate(
      title = most_common(title),
      sourcetitle = most_common(authors),
      publicationyear = most_common(cited_year)
    ) %>%
    ungroup()
}
data <- clean_references2(data) |> distinct()

data_filtered <- data |> filter(n>5) |> select(citing_id, cited_id) 
# Cleaning function


# Create an igraph object from the bibliographic coupling data
graph <- graph_from_data_frame(data_filtered, directed = FALSE)
bibc_graph <- bibcoupling(graph) |> graph_from_adjacency_matrix(mode = "undirected")
#graph <- bibc_graph |> as_tbl_graph()

community <- cluster_louvain(bibc_graph, resolution = 1)

# Get community membership
V(bibc_graph)$community <- membership(community)
V(bibc_graph)$color <- as.factor(V(bibc_graph)$community)
V(bibc_graph)$nodeSize <- degree(bibc_graph)

palette <- scico::scico(n = 17, palette = "hawaii") %>% # creating a color palette
  sample()
community_colors <- palette[as.numeric(as.factor(membership(community)))]


# Generate the node list
node_list <- data.frame(
  id = V(bibc_graph)$name,
  community = membership(community),
  color = community_colors
)

# Generate the edge list with weights
edge_list <- as_edgelist(bibc_graph) |> as_data_frame()



# Print the node and edge lists
write.csv(node_list, "node_list.csv", row.names = FALSE)
write.csv(edge_list, "edge_list.csv", row.names = FALSE)

# GET BACK SOME INFO ------------------------------------------------------

data <- rename(data, id = cited_id)
data <- data |> select(id, title, authors, cited_year)
data <-  data |> mutate(info = paste0(authors, cited_year))


node_list <- node_list |> left_join(data, by = "id") 
write.csv(node_list, "node_list.csv", row.names = FALSE)

 
node_list |> select(info) |> filter(is.na(info)) # 643

getwd()

?bibcoupling()






















library(igraph)

# Step 1: Create Initial Bipartite Graph
graph <- graph_from_data_frame(data_filtered, directed = FALSE)

# Step 2: Apply Bibliographic Coupling Projection
bibc_adj_matrix <- bibcoupling(graph) # Ensure bibcoupling() outputs a matrix
bibc_graph <- graph_from_adjacency_matrix(bibc_adj_matrix, mode = "undirected")

# Step 3: Check Nodes
print(V(bibc_graph)$name)  # Should contain only citing_id values

# Step 4: Community Detection
community <- cluster_louvain(bibc_graph)

# Step 5: Extract Nodes and Colors
V(bibc_graph)$community <- membership(community)
V(bibc_graph)$color <- as.factor(V(bibc_graph)$community)
V(bibc_graph)$nodeSize <- degree(bibc_graph)

palette <- scico::scico(n = max(membership(community)), palette = "hawaii") %>%
  sample()

community_colors <- palette[as.numeric(as.factor(membership(community)))]

# Step 6: Generate Corrected Node List
node_list <- data.frame(
  id = V(bibc_graph)$name,  # Use the projected graph
  community = membership(community),
  color = community_colors
)

# Step 7: Generate Edge List
edge_list <- as_edgelist(bibc_graph) |> as_data_frame()

write.csv(node_list, "node_list.csv", row.names = FALSE)
write.csv(edge_list, "edge_list.csv", row.names = FALSE)



#data <- rename(data, id = cited_id)
data <- data |> select(id, title, authors, cited_year)
data <-  data |> mutate(info = paste0(authors, cited_year))


node_list <- node_list |> left_join(data, by = "id")



data |> filter(citing_id == "85148415433")
df_art |> filter(citing_id == "71649083001")
df_art$citing_id <- str_extract(df_art$eid, "(?<=2-s2\\.0-)\\d+")
