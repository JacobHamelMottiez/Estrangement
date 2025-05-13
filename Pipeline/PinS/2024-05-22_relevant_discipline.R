#---
# FILE INFO
# Creator: Jacob Hamel-Mottiez
# Description: This file is for selecting the relevant
# discipline for an ulterior SQL query. 
#
#
# Creating date: 2024-05-22
# Last modified:file.mtime("C:/Users/jacob/Dropbox/PC/Desktop/RSTUDIO/Projects/2024-05-22_relevant_discipline.R")
# Jacob Hamel-Mottiez is responsible for this file. 
# For any comments or issues, please write at 
# jacob.hamel-mottiez.1@ulaval.ca
#---


# PACKAGES ----------------------------------------------------------------
library(readxl)
library(gender)
library(data.table)
library(dplyr)
library(ggplot2)
library(devtools)
library(remotes)
library(ggplot2)
library(plotly)
library(readr)
library(maps)
library(tidyverse)
library(ggthemes)
library(viridis)
library(quantmod)
library(readr)

# DATA LOADING AND FORMATING ----------------------------------------------
Liste_discipline <- read_excel("C:/Users/jacob/Dropbox/PC/Desktop/RSTUDIO/Projects/PINS/Copie de Liste_discipline.xlsx")

# SELECTING RELEVANT DISCIPLINES ------------------------------------------
relevant_disc <- c("Biologie", "Recherche biomédicale", "Médecine clinique", "Santé", "Sciences sociales", "Humanités")
relevant_code <- Liste_discipline |> filter(Discipline %in% relevant_disc) |> select(Discipline, Code_Discipline) 

# BIOLOGY : 1-10
# RECHERCHE BIOMÉDICALE : 11-25
# MÉDECINE CLINIQUE : 33-66
# SANTÉ : 110-117
# HUMANITÉS : 139-144


# TEST 
biology_art <- read_tsv("C:/Users/jacob/OneDrive - Université Laval/2024-07-08 old/2024-07-08_articles_BIOLOGY.rpt")
biology_art[1:10]





# START HERE  -------------------------------------------------------------

expdata <- data.table(expdata)
#references_CD1[,.N, by = .(Cited_Author, Cited_Work, UID_Ref)][order(-N)][1:10] 
expdata$OST_BK <- as.character(expdata$OST_BK)
country_tbl <- merge(expdata, adress, by = "OST_BK")
country_tbl[,N :=.N, by = Country][order(-N)]
country_tbl <- country_tbl[, .(N, Country, `Annee_Bibliographique`)]
country_tbl <- rename(country_tbl, region = Country)
country_tbl[, share := N/sum(N)*100][1:10]
country_tbl[country_tbl=="Peoples R China"] <- "China"
country_tbl$region <- toupper(country_tbl$region)


# MAP
world_df <-  map_data("world")
world_df <- data.table(world_df) 
world_df$region <- toupper(world_df$region)

world_df <- left_join(world_df, unique(country_tbl), by = "region")


p <- ggplot(world_df, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill= share), color = "white") +
  scale_fill_viridis(option="C", begin = 0.1, end = 0.7, direction = 1) 
ggplotly(p)


# By year window
min_year <- min(country_tbl$Annee_Bibliographique)
max_year <- max(country_tbl$Annee_Bibliographique)
last_year <- 2023
gap <- 20
time_window <- seq(min_year, max_year, gap)

# Loop for creating a x-year window. 
for(i in 1:(length(time_window))){
  if(time_window[i] == 2007){
    country_tbl[Annee_Bibliographique >= time_window[i] & Annee_Bibliographique < last_year, 
                start_window := 2007]}
  else{
    country_tbl[Annee_Bibliographique >= time_window[i] & Annee_Bibliographique < time_window[i+1], 
                start_window := time_window[i]]
  }
}



# Top 10 most cited country 
country_tbl[, .(N, region, share)][order(-share)][1:10]

for(i in 1:length(time_window)){
  x <- country_tbl[start_window == time_window[i] & !is.na(start_window), .N, by = .(region, start_window)][order(-N)]
  print(x[start_window == time_window[i]][1:10])
}


#unique(country_tbl$region)
#unique(world_df$region)
q <- ggplot(world_df, aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill=region), color = "white") 
ggplotly(q)


# COMMENT 
# Some countries need matching. But the main country are matched.


# CLEANING ----------------------------------------------------------------
# Use OpenRefine for it. 
