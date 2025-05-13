# FILE INFO ---------------------------------------------------------------
# Creator: Jacob Hamel-Mottiez, Louis Renaud-Desjardins
#
# Description: 
# This file contains all the articles and references
# from the journal Biology and Philosophy. The data has been fetched on 
# Albator SQL server via UQAM OST access.
#
# Date of creation: 2024-08-13
#
# Jacob Hamel-Mottiez is responsible for this file.
#
# For any comments or issues, please write at 
# jacob.hamel-mottiez.1@ulaval.ca
#---


# PACKAGES ----------------------------------------------------------------
library(tidyverse)
library(readxl)
library(maps)
library(viridis)
library(plotly)

#dir <- "/media/louis/05E2-13B8/BioWOS/data/"
dir <- "C:/Users/jacob/OneDrive - Université Laval/DATA/"


# DATA --------------------------------------------------------------------
# on doit enlever les deux dernières lignes de revues et philo_bio parce que c'est de la bouette
revues <- read_tsv(paste0(dir,"WOS_revues.rpt")) %>% slice(1:(n()-2)) # 21 296
bio_and_philo <- read_tsv(paste0(dir,"philo_of_bio_refs.rpt")) %>% # 135 546
  slice(1:(n()-2)) %>% # 135 544
  distinct() # 67 774
liste_discipline <- read_excel(paste0(dir,"Liste_discipline.xlsx")) # 144


bio_and_philo %>% distinct() %>% nrow() #  67774
test <- bio_and_philo %>% count(OST_BK,Ordre,UID,UID_Ref,Cited_Author,Year,
                        Volume,Page,Doi,Cited_Title,Cited_Work,Patent_No,
                        Assignee,Art_No,Occurence_Order) %>% 
  filter(n==1)

test$Cited_Title
bio_and_philo %>% filter(str_detect(Cited_Title, "The importance of symbiosis in philosophy of biology"))
bio_and_philo %>% filter(str_detect(Cited_Author, "Suarez, J"))



bio_and_philo %>% filter(Cited_Work == "NULL") %>% nrow() # 22

test <- bio_and_philo %>% count(Cited_Work) %>% arrange(Cited_Work)

data_revue <- bio_and_philo %>% 
  filter(Cited_Work != "NULL") %>% 
  left_join(revues, by = c("Cited_Work" = "Revue")) %>% 
  left_join(liste_discipline, by = c("Code_discipline" = "Code_Discipline")) %>% # 67 752
  filter(!is.na(Code_discipline)) # 34 080

data_abbrev <- bio_and_philo %>% 
  filter(Cited_Work != "NULL") %>% 
  left_join(revues, by = c("Cited_Work" = "Abbrev_11")) %>% 
  left_join(liste_discipline, by = c("Code_discipline" = "Code_Discipline")) %>% 
  filter(!is.na(Code_discipline)) # 8524

data <- bind_rows(data_revue, data_abbrev)

# il y a 67344 publications sans correspondance entre Cited_Work et revues...
# ... proche de 50% ... pas très convaincant


# WHAT ARE THE TOP CITED JOURNALS IN B&P? ------------------------ ---------
top_journals <- data %>% count(Code_Revue, sort = T) %>% 
  left_join(revues)


# WHAT ARE THE TOP CITED disciplines IN B&P? ---------------------------------
top_discipline <- data %>% count(Discipline, sort = T)
top_specialite <- data %>% count(Specialite, sort = T)

top_discipline_top_journaux <- data %>% 
  filter(Code_Revue %in% top_journals$Code_Revue[1:20]) %>% 
  count(Discipline, sort = T)


# WHAT IS THE DISTRIBUTION OF DOCUMENTS ?-----------------------------------
document_type <- read_tsv(paste0(dir, "p.ex.Liste_Document.rpt")) %>% 
  slice(1:(n()-2))
art_doc_type <- read_tsv(paste0(dir, "cited_citant_B&P.rpt")) %>% 
  slice(1:(n()-2))

document_type <- document_type |> mutate(Code_Document = as.character(Code_Document))
art_doc_type <- art_doc_type|> mutate(Code_Document = as.character(Code_Document))

art_doc_type_count <- art_doc_type |> count(Code_Document, sort = T) 

doc_type_n <- left_join(document_type, art_doc_type_count, by = "Code_Document") |> arrange(-n)
doc_type_n <-  doc_type_n |>  mutate(percent = n/sum(doc_type_n$n, na.rm = TRUE)*100)


# WHAT ARE THE MOST CITED COUNTRY? ----------------------------------------
adresses <- read_tsv(paste0(dir, "articles_adress_B&P.rpt"))
art_country <- adresses |> count(Pays, sort = T) |> rename(region = Pays)

# Visualisation of it. 
world_df <-  map_data("world")
world_df$region <- toupper(world_df$region)
world_df <- left_join(world_df, art_country, by = "region")

p <- ggplot(world_df, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill= n), color = "white") +
  scale_fill_viridis(option="D", begin = 0.1, end = 0.7, direction = 1) 
ggplotly(p)


# NETWORK OF CITING AND CITED ---------------------------------------------
data_network <- read_tsv(paste0(dir, "cited_citant_B&P.rpt"))



