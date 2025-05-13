for_name_philo_of_bio_journals = c(
  "BIOLOGY_&_PHILOSOPHY",
  "BIOLOGY_AND_PHILOSOPHY",
  "BIOLOGICAL_THEORY",
  "STUDIES_IN_HISTORY_AND_PHILOSOPHY_OF_SCIENCE_PART_C__STUDIES_IN_HISTORY_AND_PHILOSOPHY_OF_BIOLOGICAL_AND_BIOMEDICAL_SCIENCES",
  "HISTORY_AND_PHILOSOPHY_OF_THE_LIFE_SCIENCES")

dir <- "C:/Users/jacob/OneDrive - Université Laval/biophilo/Data/"
for(i in 1:length(for_name_philo_of_bio_journals)){
  refs <- read_csv(paste0(dir, for_name_philo_of_bio_journals[i],"_references",".csv"))
  assign(paste0(for_name_philo_of_bio_journals[i], "_refs"), refs)
}

# Merge the two variations of the journal "Biology and Philosophy" 
BIOLOGY_AND_PHILOSOPHY_ALL <- rbind(`BIOLOGY_&_PHILOSOPHY`,BIOLOGY_AND_PHILOSOPHY) |> tibble()
BIOLOGY_AND_PHILOSOPHY_ALL_refs <- rbind(`BIOLOGY_&_PHILOSOPHY_refs`,BIOLOGY_AND_PHILOSOPHY_refs) |> tibble()
write_csv(BIOLOGY_AND_PHILOSOPHY_ALL, paste0(dir, "BIOLOGY_AND_PHILOSOPHY_ALL.csv"))
write_csv(BIOLOGY_AND_PHILOSOPHY_ALL_refs, paste0(dir,"BIOLOGY_AND_PHILOSOPHY_ALL_refs.csv"))