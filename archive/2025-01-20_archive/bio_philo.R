# FILE INFO ---------------------------------------------------------------
# Creator: Jacob Hamel-Mottiez, Louis Renaud-Desjardins
#
# Description: 
# This file contains all the articles and references
# from the journal Biology and Philosophy. The data has been fetched on 
# Albator SQL server via UQAM OST access.
#
# Date of creation: 2024-09-10
#
# Jacob Hamel-Mottiez is responsible for this file.
#
# For any comments or issues, please write at 
# jacob.hamel-mottiez.1@ulaval.ca
#---


# PACKAGES ----------------------------------------------------------------
library(tidyverse)

dir <- "C:/Users/jacob/OneDrive - Université Laval/DATA/"

# DATA --------------------------------------------------------------------
bio_and_philo <- read_tsv(paste0(dir,"bio_philo_2024-09-10.rpt")) # 24 585

prob <- problems(bio_and_philo)
# rows 18001, 22187, 24585, 24586
bio_and_philo[22188,]

bio_and_philo <- bio_and_philo %>% 
  slice(1:(n()-2)) %>% # 24 583
  distinct() # 24 583

# Nombre d'articles de la revue étudiée ----------------------------------

bio_and_philo %>% distinct(OST_BK_citant) %>% nrow() # 1554
bio_and_philo %>% distinct(code_revue_citant) # 2229, un seul code c'est parfait!

bio_and_philo %>% select(contains("citant")) %>% distinct() %>% nrow() # 16 672

test <- bio_and_philo %>% 
  select(contains("citant")) %>% 
  distinct() %>% 
  add_count(OST_BK_citant) %>% 
  arrange(desc(n))

test2 <- test %>% filter(OST_BK_citant == "104082566")
test2 %>% distinct(annee_citant)
test2 %>% distinct(code_revue_citant)
test2 %>% distinct(titre_citant)

test3 <- test2 %>% count(rev_citant, abrev_rev_citant)

bio_and_philo %>% select(contains("rev_"))

bio_and_philo %>% select(OST_BK_citant, annee_citant) %>% 
  distinct() %>% 
  count(annee_citant) %>% 
  ggplot(aes(x = annee_citant, y = n)) +
  geom_col() +
  ggtitle("Nombre d'articles par année")

# nombre d'articles cités ------------------------------------------------------------

bio_and_philo %>% count(OST_BK_cite, titre_cite) %>% 
  arrange(desc(n)) # 16225

test <- bio_and_philo %>% filter(OST_BK_cite == "NULL")
# certains article n'ont pas de références


# revues citées  ----------------------------------

bio_and_philo %>% count(abrev_rev_cite, sort = T) # 1981
bio_and_philo %>% count(rev_cite, sort = T) # 1981
bio_and_philo %>% count(rev_cite, abrev_rev_cite, sort = T)


# disciplines citées  ----------------------------------

bio_and_philo %>% count(disc_cite, sort = T) # 17
bio_and_philo %>% count(spec_cite, sort = T) # 17





