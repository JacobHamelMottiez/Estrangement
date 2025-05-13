# PACKAGES
library(readr)
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
library(scales) 
library(readxl)

dir_od <- "C:/Users/jacob/OneDrive - Université Laval/biophilo/Data/BIO_ARXIV_DATA/"
all_bio_arxiv <- read_csv("Data/BIO_ARXIV_DATA/all_bio_arxiv.csv")



# DATA --------------------------------------------------------------------
bio_arxiv <- read_csv(paste0(dir_od, "all_bio_arxiv.csv"))
disciplines <-  bio_arxiv |> distinct()


disciplines_cat <- disciplines |> mutate(preprint_category = toupper(preprint_category), 
                                         published_date = as.numeric(format(published_date, "%Y")))|> 
  filter(published_date >= 2020) |>
  count(preprint_category) |>
  arrange(desc(n)) |> print(n=55) 

# Convert to proportions
disciplines_cat <- disciplines_cat %>%
  mutate(percent = n / sum(n)*100)  # Compute percentage




# VIZUALISATION -----------------------------------------------------------
ggplot(disciplines_cat, aes(x = preprint_category, y = percent, group = preprint_category)) + 
  geom_bar(stat = "identity")



colors = viridis::magma(28)
highchart() |> hc_add_series(disciplines_cat |> slice_max(n =40, order_by = n), hcaes(x = preprint_category, y = percent, group = preprint_category), type = "column") |> 
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



# JOURNALS ----------------------------------------------------------------

published_journal <- disciplines |> select(published_journal) 
published_journal$published_journal <- toupper(published_journal$published_journal)
published_journal <- published_journal |> count(published_journal) |> arrange(desc(n)) # ~ 5000 journnals. 



ISSN_median_mean <- read_csv("Data/ISSN_median_mean.csv")



all_journals_scopus <- read_excel("C:/Users/jacob/OneDrive - Université Laval/DATA/ALL_JOURNALS_SCOPUS_October_2024.xlsx")
all_journals_scopus_filtered <- all_journals_scopus |> select(ISSN, `Source Title`)


all_journals_scopus_filtered$`Source Title` <- toupper(all_journals_scopus_filtered$`Source Title`)
all_journals_scopus_filtered <- rename(all_journals_scopus_filtered, published_journal = `Source Title`)

ISSN_test <-  left_join(published_journal, all_journals_scopus_filtered, by = "published_journal")


 
ISSN_test |> filter(is.na(ISSN)) # About 2,107 journals don't match in Scopus database. 
ISSN_test |> filter(!is.na(ISSN)) |> filter(str_detect(published_journal, "PROCEEDINGS OF THE"))




test <- final_table |> select(sourcetitle) |> count(sourcetitle) |> arrange(desc(n)) |> rename(published_journal = sourcetitle)


left_join(published_journal, test, by = "published_journal") |> arrange(desc(n.y))





















