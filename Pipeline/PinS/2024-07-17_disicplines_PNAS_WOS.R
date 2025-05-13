# PACKAGES ----------------------------------------------------------------
library(dplyr)
library(refinr)
library(DT)
library(data.table)
library(readr)
library(readxl)

# METHOD 1  ---------------------------------------------------------------------
# Specialities in the Life Sciences and Biomedecine as listed by WoS
# https://images.webofknowledge.com/images/help/WOS/hp_research_areas_easca.html

# Load specialties list. 
List_discipline <- read_excel("C:/Users/jacob/OneDrive/Bureau/MASTER PHILOSOPHY/Liste_discipline.xlsx")
setDT(List_discipline)
List_discipline <- List_discipline[,.(Code_Discipline, EDiscipline, ESpecialite)]

# Specialities
disciplines <- c(
  "Agriculture",
  "Allergy",
  "Anatomy & Morphology",
  "Anesthesiology",
  "Anthropology",
  "Audiology & Speech-Language Pathology",
  "Behavioral Sciences",
  "Biochemistry & Molecular Biology",
  "Biodiversity & Conservation",
  "Biophysics",
  "Biotechnology & Applied Microbiology",
  "Cardiovascular System & Cardiology",
  "Cell Biology",
  "Critical Care Medicine",
  "Dentistry, Oral Surgery & Medicine",
  "Dermatology",
  "Developmental Biology",
  "Emergency Medicine",
  "Endocrinology & Metabolism",
  "Entomology",
  "Environmental Sciences & Ecology",
  "Evolutionary Biology",
  "Fisheries",
  "Food Science & Technology",
  "Forestry",
  "Gastroenterology & Hepatology",
  "General & Internal Medicine",
  "Genetics & Heredity",
  "Geriatrics & Gerontology",
  "Health Care Sciences & Services",
  "Hematology",
  "Immunology",
  "Infectious Diseases",
  "Integrative & Complementary Medicine",
  "Legal Medicine",
  "Life Sciences Biomedicine Other Topics",
  "Marine & Freshwater Biology",
  "Mathematical & Computational Biology",
  "Medical Ethics",
  "Medical Informatics",
  "Medical Laboratory Technology",
  "Microbiology",
  "Mycology",
  "Neurosciences & Neurology",
  "Nursing",
  "Nutrition & Dietetics",
  "Obstetrics & Gynecology",
  "Oncology",
  "Ophtalmology",
  "Orthopedics",
  "Otorhinolaryngology",
  "Paleontology",
  "Parasitology",
  "Pathology",
  "Pediatrics",
  "Pharmacology & Pharmacy",
  "Physiology",
  "Plant Sciences",
  "Psychiatry",
  "Public, Environmental & Occupational Health",
  "Radiology, Nuclear Medicine & Medical Imaging",
  "Rehabilitation",
  "Reproductive Biology",
  "Research & Experimental Medicine",
  "Respiratory System",
  "Rheumatology",
  "Sport Sciences",
  "Substance Abuse",
  "Surgery",
  "Toxicology",
  "Transplantation",
  "Tropical Medicine",
  "Urology & Nephrology",
  "Veterinary Sciences",
  "Virology",
  "Zoology"
)

disciplines_dt<- data.table(specialties = disciplines)
disciplines_dt <- merge(x = disciplines_dt, y = List_discipline, by.x = "specialties", by.y = "ESpecialite", all.x = TRUE)




# METHOD 2 --------------------------------------------------------------------
# Take back the specialities of PNAS for biology as Pradeu did in 
# T. (2017). Thirty years of Biology & Philosophy: philosophy of which biology? 
# Biology & Philosophy, 32(2), 149‑167. https://doi.org/10.1007/s10539-016-9558-7

# Specialities
specialties <- c(
  "Agricultural sciences", "Anthropology", "Applied biological sciences", "Biochemistry", 
  "Biophysics and computational biology", "Cell biology", "Developmental biology", 
  "Ecology", "Environmental sciences", "Evolution", "Genetics", "Immunology", 
  "Medical sciences", "Microbiology", "Neuroscience", "Pharmacology", "Physiology", 
  "Plant biology", "Population biology", "Psychological and cognitive sciences", 
  "Sustainability science", "Systems biology", "Others"
)

# Convert to a data table
specialties_dt <- data.table(Specialty = specialties)
specialties_dt$Specialty


# METHOD 3 ---------------------------------------------------------
# Take all the biological journals listed by WoS and cross each of them with their 
# respective specialities. 
# https://wosjournal.com/list-of-journals.php?id=Biology 

# Load biology journals and the all journals from WoS 
WoS_journals_all <- read_tsv("C:/Users/jacob/OneDrive - Université Laval/DATA/2024-07-18_liste_revue.rpt")
WoS_journals_bio <- read_tsv("C:/Users/jacob/OneDrive - Université Laval/DATA/journals_biology_WoS.tsv")
WoS_journals_bio <- rename(WoS_journals_bio, "Address" = "Publisher Address")
WoS_journals_bio <- rename(WoS_journals_bio, "Revue" = "Journal Title")

# Join biology journals with their respective specialties  
WoS_journals_bio <- left_join(WoS_journals_bio, WoS_journals_all, by = "Revue")
unique_code <- as.vector(unique(WoS_journals_bio$Code_discipline))

# Filter to get only the specialities in the selected biology journals.
biology_specialities <- List_discipline %>%
  filter(List_discipline$Code_Discipline %in% unique_code)

List_discipline[ESpecialite == "Zoology"]

