# ------------------------------
# Title: {filename}
# Author: Jacob Hamel-Mottiez
# Date: sys.Date()
# Description: 
# Contact : For any issue please write to jacob.hamel-mottiez.1@ulaval.ca. 
# ------------------------------

# PACKAGES ----------------------------------------------------------------
library(tidyverse)
library(plotly)
library(readxl)



# DATA --------------------------------------------------------------------
arts_and_refs_special_philo_bio <- read_csv("Data/pybiblio/SPECIALIZED PHILOSOPHY OF BIOLOGY/articles_and_references_special_philo_bio.csv")
arts_and_refs_bioRxiv <- read_csv("Data/pybiblio/BIOLOGY/bioRxiv/articles_and_references_bioRxiv.csv")
arts_bioRxiv_reformat <- read_csv("Data/pybiblio/BIOLOGY/bioRxiv/articles_bioRxiv_reformat.csv")
arts_bioRxiv_reformat

arts_and_refs_bioRxiv |> select(citing_journal)

# Citation delay ----------------------------------------------------------
# for all
arts_and_refs_special_philo_bio <- arts_and_refs_special_philo_bio |> mutate(delay = citing_year-cited_year)


arts_and_refs_special_philo_bio$decade <- cut(arts_and_refs_special_philo_bio$citing_year, 
                                              breaks = c(1986, 1996, 2006, 2017, 2026),  # Include up to 2024
                                              labels = c("1986-1997", "1998-2005", "2006-2013", "2014-2025"),
                                              right = FALSE)  # Left-inclusive


p1 <- ggplot(arts_and_refs_special_philo_bio |> filter(!is.na(decade)), aes(x = delay, color = decade, group = decade)) +
  stat_ecdf(geom = "step", show.legend = TRUE) +
  labs(title = "Citation Delay Philosophy of Biology",
       x = "Delay",
       y = "CDF") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(limits = c(0, 50)) 
p1

# only for biological references. 
arts_and_refs_special_philo_bio <- arts_and_refs_special_philo_bio |> mutate(delay = citing_year-cited_year)

arts_and_refs_special_philo_bio <- arts_and_refs_special_philo_bio |> filter(cited_journal %in% arts_and_refs_bioRxiv$citing_journal) # 31, 760 


arts_and_refs_special_philo_bio$decade <- cut(arts_and_refs_special_philo_bio$citing_year, 
                                              breaks = c(1986, 1996, 2006, 2017, 2026),  # Include up to 2024
                                              labels = c("1986-1997", "1998-2005", "2006-2013", "2014-2025"),
                                              right = FALSE)  # Left-inclusive


p2 <- ggplot(arts_and_refs_special_philo_bio |> filter(!is.na(decade)), aes(x = delay, color = decade, group = decade)) +
  stat_ecdf(geom = "step", show.legend = TRUE) +
  labs(title = "Citation Delay Philosophy of Biology",
       x = "Delay",
       y = "Density") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(limits = c(0, 50)) 
p2




# CITATION DELAY BY JOURNAL -----------------------------------------------
arts_and_refs_special_philo_bio

#Density
ggplot(arts_and_refs_special_philo_bio |> filter(!is.na(decade)), aes(x = delay, color = decade, group = decade)) +
  geom_density() +
  labs(title = "Citation Delay Philosophy of Biology",
       x = "Delay",
       y = "Density") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(limits = c(0, 50)) + 
  facet_wrap(~citing_journal)

ggplot(arts_and_refs_special_philo_bio |> filter(!is.na(decade)), aes(x = delay, color = decade, group = decade)) +
  geom_density() +
  labs(title = "Citation Delay Philosophy of Biology",
       x = "Delay",
       y = "Density") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(limits = c(0, 50)) 


#Density
ggplot(arts_and_refs_special_philo_bio |> filter(!is.na(decade)), aes(x = delay, color = decade, group = decade)) +
  stat_ecdf(geom = "step", show.legend = TRUE) +
  labs(title = "Citation Delay Philosophy of Biology",
       x = "Delay",
       y = "CDF") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(limits = c(0, 50)) 
  #facet_wrap(~citing_journal)



summary_data <- arts_and_refs_special_philo_bio |> 
  group_by(citing_year) |>
  summarize(mean_delay = mean(delay, na.rm = TRUE),
          median_delay = median(delay, na.rm = TRUE),
          sd_delay = sd(delay, na.rm = TRUE),
          n = n())


ggplot(summary_data, aes(x = citing_year, y = mean_delay)) +
  geom_line() +
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "Average Citation Delay Over Time",
       x = "Citing Year", y = "Mean Citation Delay")



