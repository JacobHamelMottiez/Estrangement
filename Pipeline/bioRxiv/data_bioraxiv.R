# Fetch bioRarxiv documents 


# PACKAGES ----------------------------------------------------------------
library(rvest)
library(dplyr)
library(httr)
library(stringr)
library(bib2df)
library(RefManageR)
library(bibtex)
library(ggplot2)
library(plotly)
library(highcharter)
library(RColorBrewer)
library(jsonlite)
library(tidyverse)

dir_od <- "C:/Users/jacob/OneDrive - Université Laval/biophilo/Data/BIO_ARXIV_DATA/"



# TEST --------------------------------------------------------------------
url <- "https://api.biorxiv.org/pubs/biorxiv/2020-01-01/2021-12-31/200000"

response <- GET(
  url,
  user_agent("Mozilla/5.0"),
  add_headers(Accept = "application/json")  # Ensure JSON response
)

# Extract content as text and then parse it as JSON
content<- fromJSON(content(response, as = "text", encoding = "UTF-8"), simplifyVector = TRUE)
tibble(content$collection)
tibble(content$collection) 


# GENERALIZE --------------------------------------------------------------
url = "https://www.biorxiv.org/content/early/recent?"
bio_r_xiv_url = read_html(url)
n_pages = bio_r_xiv_url |> html_element(".pager-last a") |> html_text()
pages <- seq(from = 0, to = n_pages)


years <- seq(2013, 2024)


list_url <- list()

for(i in 1:(length(years))){
  if(years[i] == max(years)){
  print("The list is completed!")
  }else{
    url =  paste0("https://api.biorxiv.org/pubs/biorxiv/", years[i], "-01-01/", years[i+1],"-12-31")
    list_url[i] = url}
  }
  

# RETRIEVAL BY 100 ENTRIES 
retrieval <- function(url) {
  all_data <- tibble()
  dumb_condition <- seq(from = 0, to = 10000000, by = 100)  # Adjust this to your needs
  csv <- seq(1, 11)
  
  for(i in dumb_condition) {
    complete_url <- paste0(url, "/", i)
    
    response <- GET(
      complete_url,
      user_agent("Mozilla/5.0"),
      add_headers(Accept = "application/json")
    )
    
    if (http_type(response) != "application/json") {
      next  # Skip if response is not JSON
    }
    
    content <- fromJSON(content(response, as = "text", encoding = "UTF-8"), simplifyVector = TRUE)
    
    if (!"collection" %in% names(content)) {
      next  # Skip if "collection" key is missing
    }
    
    data <- tibble(content$collection)
    
    if (nrow(data) == 0) {
      break  # Stop if no more data is found
    }
    
    all_data <- bind_rows(all_data, data)
  }
  
  # Save the CSV with a unique name based on the timestamp
  unique_filename <- paste0("data_", Sys.time(), ".csv")
  unique_filename <- gsub("[: ]", "_", unique_filename)  # Clean up timestamp for file name
  write_csv(all_data, unique_filename)
  
  print(paste("CSV saved:", unique_filename, "with", nrow(all_data), "rows"))
  
  return(all_data)  # Return all the data collected
}

lapply(list_url, FUN = retrieval)


dir_od <- "C:/Users/jacob/OneDrive - Université Laval/biophilo/"

full_data = tibble()
name_year <- seq(2013, 2023)
for(i in 1:length(name_year)){
  data <- read_csv(paste0(dir_od, "data_", name_year[i], ".csv"))
  full_data = rbind(full_data, data)
}

full_data #256, 935 entries
full_data |> count(published_journal) |> arrange(desc(n))
full_data |> count(preprint_category) |> arrange(desc(n)) |> print(n = 55)



write_csv(full_data, paste0(dir_od, "all_bio_arxiv.csv"))

bio_arxiv <- read_csv(paste0(dir_od, "all_bio_arxiv.csv"))

disciplines <-  bio_arxiv |> distinct()

disciplines_cat <- disciplines |> count(preprint_category) |> arrange(desc(n)) |> print(n=55)

colors = viridis::cividis(40)
highchart() |> hc_add_series(disciplines_cat |> slice_max(n =40, order_by = n), hcaes(x = preprint_category, y = n, group = preprint_category), type = "column") |> 
  hc_colors(colors = colors) |> 
  hc_title(text = "In Which Category Does Each Paper Fall Into? (n = 137,247)",   # Title text
           align = "center",                    # Alignment (can be "left", "center", "right")
           style = list(fontSize = "70px", fontWeight = "bold", fontFamily = "Garamond")) |>  # Custom styling 
  hc_subtitle(
    text = "From BioArxiv 2013-2024 ", # Subtitle text
    align = "center",                   # Subtitle alignment
    style = list(
      fontSize = "40px",                # Font size for subtitle
      fontFamily = "Garamond",       # Custom font for subtitle
      color = "#666666",                # Subtitle color
      fontStyle = "italic"              # Italicize subtitle
    )
  )

