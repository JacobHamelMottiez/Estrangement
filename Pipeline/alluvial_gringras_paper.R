# ------------------------------
# Title: Alluvial Gingras paper. 
# Author: Jacob Hamel-Mottiez
# Date: 2025-05-27
# Description: 
# Contact : For any issue please write to jacob.hamel-mottiez.1@ulaval.ca. 
# ------------------------------

# PACKAGES ----------------------------------------------------------------
library(tidyverse)
library(plotly)
library(readxl)
library(tibble)

discipline_flow <- tibble::tibble(
  citing_discipline = "Philosophy of Science",
  cited_discipline = c(
    "Philosophy of science",
    "Philosophy",
    "Humanities (other)",
    "Social Sciences (other)",
    "Science Studies",
    "Professional Fields",
    "Economics",
    "Psychology",
    "Health",
    "Clinical Medicine",
    "Biomedical Research",
    "Biology",
    "Physics",
    "Engineering & Technology",
    "Mathematics",
    "Natural Sciences & Engineering (other)"
  ),
  percentage = c(
    30.2, 19.7, 3.8, 4.0, 4.8, 3.6, 1.5, 5.6,
    5.5, 5.1, 2.8, 4.2, 3.2, 3.1, 1.8, 1.1
  )
)
