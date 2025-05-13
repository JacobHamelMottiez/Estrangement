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
library(tidyverse)
dir_od <- "C:/Users/jacob/OneDrive - Université Laval/biophilo/Data/"

# TEST CASE : DONE --------------------------------------------------------
x <- "https://philpapers.org/browse/developmental-biology?import_options=1&sqc=off&categorizerOn=1&cId=5272&langFilter=&onlineOnly=off&catId=5272&cn=developmental-biology&sort=cat&start=0&limit=499&new=1&filterByAreas=&publishedOnly=on&search_inside=1&showCategories=on&proOnly=on&newWindow=&hideAbstracts=off&freeOnly=off&format=bib&jlist=&ap_c1=&ap_c2=apiId=[1397950]&apiKey=[M8rRv5rbBgJaP8oi]"
response <- GET(x)
content <- rawToChar(response$content)

# Save to .bib file
writeLines(content, "developmental_biology.bib", useBytes = TRUE)

bib2df("developmental_biology.bib")



# Add a user agent and handle redirects
response <- GET(x,
                user_agent("Mozilla/5.0"),
                add_headers(Accept = "text/plain"),
                config(followlocation = TRUE))

# Check the status code
print(status_code(response))

# Look at the actual content
content <- rawToChar(response$content)
print(nchar(content))  # This will tell us if we got any content

writeLines(content, "developmental_biology.bib", useBytes = TRUE)
test <- bib2df("developmental_biology.bib")




# GENERALIZE --------------------------------------------------------------
main_url <- "https://philpapers.org/browse/philosophy-of-biology"
page <- read_html(main_url)

documents_by_subcat <- page %>%
  html_elements(".catName2+ .hint .hint") %>%
  html_text()

names_subcat <- page %>%
  html_elements(".catName2") %>%
  html_text()

names_subcat <- gsub(" ", "-", names_subcat) |> tolower()
documents_by_subcat = as.numeric(gsub(",", "", documents_by_subcat)) 



# Initialize the list
url_list <- list()

# Define the function
generate_urls <- function(total, names_subcat, increment) {
  url_list <- list() # Initialize an empty list to store URLs

  for (i in seq_along(total)) {
    # Generate sequence with increments, ensuring the final value matches `total[i]`
    sequence <- seq(0, total[i], by = increment)
    if (total[i] != tail(sequence, 1)) {
      sequence <- c(sequence, total[i])
    }

    # Generate URLs for the current subcategory
    urls_for_subcat <- sapply(1:(length(sequence) - 1), function(k) {
      paste0(
        "https://philpapers.org/browse/", names_subcat[i],
        "?import_options=1&sqc=off&categorizerOn=1&cId=5272&langFilter=&onlineOnly=off&catId=5272&cn=", names_subcat[i],
        "&sort=cat&start=", sequence[k],
        "&limit=", sequence[k + 1],
        "&new=1&filterByAreas=&publishedOnly=on&search_inside=1&showCategories=on&proOnly=on&newWindow=&hideAbstracts=off&freeOnly=off&format=bib&jlist=&ap_c1=&ap_c2="
      )
    })

    # Add URLs for the subcategory to the list
    url_list[[names_subcat[i]]] <- urls_for_subcat
  }

  return(url_list)
}

url_list <- generate_urls(documents_by_subcat, names_subcat, 500)  
print(url_list)



# Initialize an empty data frame to store the combined results
combined_bib_data <- data.frame()

# Function to fetch and process data from a single URL
bib_fct <- function(url) {
  response <- GET(
    url,
    user_agent("Mozilla/5.0"),
    add_headers(Accept = "text/plain")
  )
  
  content <- rawToChar(response$content)
  print(nchar(content))  # Log the size of the content received
  
  file_name <- str_extract(url, "(?<=/browse/)(.*?)(?=\\?)")
  print(file_name)  # Log the file name derived from the URL
  
  # Create a unique file name for saving the content
  local_file_name <- paste0(file_name, ".bib")
  
  # Write content to a local .bib file
  writeLines(content, local_file_name, useBytes = TRUE)
  
  # Attempt to read the .bib file and add it to the combined data
  try({
    bib_file <- bib2df(local_file_name)
    print(head(bib_file))  # Display the first few rows of the bib data
    
    # Combine the current bib data with the overall data frame
    assign("combined_bib_data", rbind(combined_bib_data, bib_file), envir = .GlobalEnv)
  }, silent = TRUE)
}

# Apply the function over all URLs in the generated list
process_urls <- function(url_list) {
  for (subcategory in names(url_list)) {
    message("Processing subcategory: ", subcategory)
    sapply(url_list[[subcategory]], FUN = bib_fct)
  }
}

# Example: Generating URLs and processing them
generate_and_process <- function(total, names_subcat, increment) {
  # Generate the URL list
  url_list <- generate_urls(total, names_subcat, increment)
  
  # Process the URLs to fetch and combine data
  process_urls(url_list)
}

generate_and_process(documents_by_subcat, names_subcat, 500) # I get 76 742 entries, I expected ~ 40 000. When I do a unique, I get 16, 430. Ok, there is not really 40000 unique entries some repeat over the same subdisciplines.


combined_bib_data

write_csv(combined_bib_data, paste0(dir_od, "philpapers_all_documents_classified.csv"))

unique_DOI <- combined_bib_data |> unique() |> filter(!is.na(DOI))
write_csv(unique_DOI, paste0(dir_od, "philpapers_articles_classified.csv"))

# NEXT STEP, TAKE ALL THE LINKS AND SCRAPE ABSTRACTS. MATCH ON JOURNAL, TITLE, YEAR AND POSSIBLY DOI. 


# VISUALIZE THE MOST PROLIFIC JOURNALS  -----------------------------------
combined_bib_data <- combined_bib_data  |> unique()
viz_journals <- combined_bib_data |> 
  filter(!is.na(JOURNAL)) |> 
  select(JOURNAL) |> 
  count(JOURNAL) |> 
  arrange(desc(n)) 

data = viz_journals |> slice_head(n = 10) 

plot <- ggplot(data, aes(x = JOURNAL, y = n, fill = JOURNAL)) + 
  geom_bar(stat="identity")

sum(viz_journals$n)
colors = viridis::magma(10)

highchart() |> hc_add_series(data, hcaes(x = JOURNAL, y = n, group = JOURNAL), type = "column") |> 
  hc_colors(colors = colors) |> 
  hc_title(text = "Where do Philosopher of Biology Publish Their Articles?",   # Title text
    align = "center",                    # Alignment (can be "left", "center", "right")
    style = list(fontSize = "70px", fontWeight = "bold", fontFamily = "Garamond")) |>  # Custom styling 
hc_subtitle(
  text = "From PhilPapers - Philosophy of Biology -  edited by Manolo Martínez", # Subtitle text
  align = "center",                   # Subtitle alignment
  style = list(
    fontSize = "40px",                # Font size for subtitle
    fontFamily = "Garamond",       # Custom font for subtitle
    color = "#666666",                # Subtitle color
    fontStyle = "italic"              # Italicize subtitle
  )
)





# Look at the distribution of types.  -------------------------------------



combined_bib_data |> distinct() # 10 000. 
category <-  combined_bib_data |> 
  distinct() |>
  select(CATEGORY) |> 
  count(CATEGORY) |>
  distinct()

colors = viridis::turbo(5)
highchart() |> hc_add_series(category, hcaes(x = CATEGORY, y = n, group = CATEGORY), type = "column") |> 
  hc_colors(colors = colors) |> 
  hc_title(text = "Which Documents are Fetched From Philpapers?",   # Title text
           align = "center",                    # Alignment (can be "left", "center", "right")
           style = list(fontSize = "70px", fontWeight = "bold", fontFamily = "Garamond")) |>  # Custom styling 
  hc_subtitle(
    text = "From PhilPapers - Philosophy of Biology -  edited by Manolo Martínez", # Subtitle text
    align = "center",                   # Subtitle alignment
    style = list(
      fontSize = "40px",                # Font size for subtitle
      fontFamily = "Garamond",       # Custom font for subtitle
      color = "#666666",                # Subtitle color
      fontStyle = "italic"              # Italicize subtitle
    )
  )


combined_bib_data
