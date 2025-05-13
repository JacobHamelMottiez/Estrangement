# PACKAGES ----------------------------------------------------------------
library(dplyr)
library(refinr)
library(DT)
library(data.table)
library(readr)
library(stringi)
library(textreuse)
library(RecordLinkage)
library(viridisLite)
library(plotly)
library(dplyr)
library(stringr)

revues <- read_tsv("C:/Users/jacob/OneDrive - Université Laval/DATA/WOS_revues.rpt")
art_humanities <- read_tsv("C:/Users/jacob/OneDrive - Université Laval/DATA/HUMANITIES/2024-07-16_articles_HUMANITIES.rpt")

setDT(revues)
setDT(art_humanities)

journals <- c("BIOLOGY & PHILOSOPHY", 
              "STUDIES IN HISTORY AND PHILOSOPHY OF SCIENCE PART C-STUDIES IN HISTORY AND PHILOSOPHY OF BIOLOGICAL AND BIOMEDICAL SCIENCES",
              "HISTORY AND PHILOSOPHY OF THE LIFE SCIENCES")

rel_rev <- revues[Revue %in% journals,]
rel_rev$Code_Revue <- as.double(rel_rev$Code_Revue)
merge(rel_rev, art_humanities, by = "Code_Revue")

require(installr)

updateR()
