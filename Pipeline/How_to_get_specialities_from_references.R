library(tidyverse)
library(readxl)
library(janitor)


BRITISH_JOURNAL_FOR_THE_PHILOSOPHY_OF_SCIENCE <- read_csv("Data/pybiblio/BRITISH_JOURNAL_FOR_THE_PHILOSOPHY_OF_SCIENCE.csv")
ref <- read_csv("Data/pybiblio/BRITISH_JOURNAL_FOR_THE_PHILOSOPHY_OF_SCIENCE_refs_pyblio.csv")
journaux <- read_excel("C:/Users/jacob/OneDrive - Université Laval/DATA/ALL_JOURNALS_SCOPUS_October_2024.xlsx") 






###############################################
###############################################


# nombre de journaux dans scopus
liste_j <- journaux %>% distinct(`Source Title`) # 46 378
nrow(liste_j) # 46378
# certains titres se répètent

# disciplines
liste_d <- journaux %>% 
  unite("d",matches("^\\d{4}"), na.rm = T, sep = "_") %>% 
  separate_longer_delim(d, delim = "_") %>% 
  distinct(d) # 28

# disciplines les plus importantes dans les journaux de scopus

top_d <- journaux %>% 
  unite("d",matches("^\\d{4}"), na.rm = T, sep = "_") %>% 
  separate_longer_delim(d, delim = "_") %>% 
  count(d, sort = T) 


###############################################
###############################################

# disciplines des refs

j_d <- journaux %>% 
  unite("d",matches("^\\d{4}"), na.rm = T, sep = "_") %>% 
  separate_longer_delim(d, delim = "_") %>% 
  select(`Source Title`,d) %>% 
  distinct() %>% 
  mutate(scopus = 1)

test <- ref %>% select(citing_eid, sourcetitle) %>% 
  left_join(j_d, by = join_by(sourcetitle == `Source Title`))

# nombre de journaux des refs non-reconnus dans les journaux de scopus
test %>% filter(is.na(scopus)) %>% nrow() # 42943
nrow(ref) |> 
# 86429 ref au total
# la moitié des références ne provient pas de journaux de la liste de journaux de scopus
ref_journaux_nonscopus <- test %>% filter(is.na(scopus)) %>% 
  count(sourcetitle) |> arrange(desc(n)) # there are important journal to classify that we loose. 
# j'ai l'impression que ces références sont des livres

# disciplines des autres reférences

ref_d <- test %>% filter(!is.na(scopus)) %>% 
  count(d, sort = T) |> print(n = 27)




test2 <- test |> group_by(citing_eid) |> count(d) |> arrange(desc(n))
x <- ggplot(test2, aes(x = n)) +
  geom_bar()
ggplotly(x)



test |> filter(is.na(scopus))







all_journals_scopus <- read_excel("C:/Users/jacob/OneDrive - Université Laval/DATA/ALL_JOURNALS_SCOPUS_October_2024.xlsx")


















