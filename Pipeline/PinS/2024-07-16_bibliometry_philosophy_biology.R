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

# DATA  --------------------------------------------------------------------
# Articles
art_humanities <- read_tsv("C:/Users/jacob/OneDrive - Université Laval/DATA/HUMANITIES/2024-07-16_articles_HUMANITIES.rpt")
setDT(art_humanities)
art_humanities[1:10]

# References
refs_humanities <- read_tsv("C:/Users/jacob/OneDrive - Université Laval/DATA/HUMANITIES/2024-07-12_references_HUMANITIES.rpt",skip_empty_rows = TRUE)
setDT(refs_humanities)
refs_humanities <- refs_humanities[rowSums(refs_humanities == "" | is.na(refs_humanities)) != ncol(refs_humanities)]

## Select philosophy
rel_journals <- c()
art_philosophy <- art_humanities[rel_journals,]
rm(art_humanities)

refs_philosophy <- left_join(art_philosophy[,.(UID,Code_Discipline)], refs_humanities, by = "UID")
rm(art_humanities, refs_humanities)
gc()

# MANIPULATION ------------------------------------------------------------
setDT(refs_philosophy)
refs_philosophy[UID_Ref != "NULL", N_UID_Ref := .N, by = UID_Ref]

# Cited_Author homogenization function
clean_author <- function(x) {
  x <- stri_trans_general(x, "Latin-ASCII")
  x <- tolower(x)
  x <- gsub("[^[:alnum:][:space:]]", "", x)
  return(x)
}
refs_philosophy$Cleaned_Author  <- clean_author(refs_philosophy$Cited_Author)

# Getting First name and Initials
refs_philosophy[, c("First_name", "Initials") := tstrsplit(Cleaned_Author, " ", fixed=TRUE, keep=1:2)]

# Function to process author names
process_author <- function(author) {
  parts <- strsplit(author, " ", fixed = TRUE)[[1]]
  if (length(parts) == 2) {
    return(parts)
  } else {
    return(c(NA_character_, NA_character_))
  }
}

refs_philosophy[, c("First_name", "Initials") := transpose(lapply(Cleaned_Author, process_author))]
refs_philosophy[, full_info := paste(First_name, Cited_Title, Cited_Work, Year)]

## Saving cleaned refs 
saveRDS(refs_philosophy, "refs_philosophy.RDS")


# OPENREFINE FINGERPRINT --------------------------------------------------------------
set.seed(123)  # for reproducibility
refs_subset <- refs_philosophy |>
  sample_n(nrow(refs_philosophy))  # adjust this number as needed

# Step 1: Create lists of names for each letter
alphabet <- LETTERS

# Initialize a counter
counter <- 0
total_letters <- length(alphabet)

# Steps 2 and 3: Process each group and combine results
result <- do.call(rbind, lapply(alphabet, function(letter) {
  # Subset data for the current letter
  subset_data <- refs_subset[startsWith(refs_subset$Cited_Author, letter), ]
  
  if(nrow(subset_data) > 0) {
    # Create new column
    subset_data$Modified_Title <- subset_data$Cited_Title
    subset_data$Modified_Work <- subset_data$Cited_Work
    
    # Apply refinr functions
    subset_data$Modified_Title <- key_collision_merge(subset_data$Modified_Title)
    subset_data$Modified_Title <- n_gram_merge(subset_data$Modified_Title)
    subset_data$Modified_Work <- key_collision_merge(subset_data$Modified_Work)
    subset_data$Modified_Work <- n_gram_merge(subset_data$Modified_Work)
    
    # Update and print progress
    counter <<- counter + 1
    cat(sprintf("Processed letter %s (%d/%d)\n", letter, counter, total_letters))
    
    return(subset_data)
  } else {
    # Update and print progress even if subset is empty
    counter <<- counter + 1
    cat(sprintf("No data for letter %s (%d/%d)\n", letter, counter, total_letters))
    
    return(NULL)
  }
}))

# Print summary to check the results
cat(sprintf("Original subset rows: %d\n", nrow(refs_subset)))
cat(sprintf("Result rows: %d\n", nrow(result)))

# Check if Modified_Title exists and show some comparisons
if("Modified_Title" %in% names(result)) {
  differences <- sum(result$Cited_Title != result$Modified_Title, na.rm = TRUE)
  cat(sprintf("Number of modified titles: %d\n", differences))
  
  # Show a few examples of modifications
  modified <- result[result$Cited_Title != result$Modified_Title, c("Cited_Author", "Cited_Title", "Modified_Title")]
  print(head(modified))
} else {
  cat("Modified_Title column not found in result\n")
}


# Check if Modified_Title exists and show some comparisons
if("Modified_Work" %in% names(result)) {
  differences <- sum(result$Cited_Work != result$Modified_Work, na.rm = TRUE)
  cat(sprintf("Number of modified titles: %d\n", differences))
  
  # Show a few examples of modifications
  modified <- result[result$Cited_Work != result$Modified_Work, c("Cited_Author", "Cited_Title", "Modified_Title")]
  print(head(modified))
} else {
  cat("Modified_Work column not found in result\n")
}

saveRDS(result, "refs_philosophy_OR.RDS")


refs_philosophy_OR <- result
rm(result, refs_philosophy, art_philosophy, modified)


# MANUAL CHECKING ---------------------------------------------------------
refs_philosophy_OR









































# OTHER ALGORITHM ---------------------------------------------------------
# FINDING SIMILAR FULL_INFO -----------------------------------------------
unique(refs_philosophy[Cited_Work == "NULL",.(UID_Ref, First_name, Cited_Title, Cited_Work, N_UID_Ref, full_info)]) |> nrow()
# Documents without a Cited_Work = 2307. Negligible. 

# TEXTREUSE ---------------------------------------------------------------
# https://cran.r-project.org/web/packages/textreuse/vignettes/textreuse-minhash.html

test <- unique(refs_philosophy[N_UID_Ref>5, .(First_name, full_info, Year, UID_Ref,N_UID_Ref)][order(-N_UID_Ref)])


minhash <- minhash_generator(n = 200, seed = 123)
corpus <- TextReuseCorpus(text = test$full_info,
                          tokenizer = tokenize_ngrams, 
                          n = 4, minhash_func = minhash,
                          keep_tokens = TRUE, progress = FALSE)

head(minhashes(corpus[[1]]))
length(minhashes(corpus[[1]]))


# Print information about the corpus
print(corpus)
corpus

# Use LSH to find similar documents
buckets <- lsh(corpus, bands = 50, progress = FALSE)

# Find potential matches
candidates <- lsh_candidates(buckets)


lsh <- lsh_compare(candidates, corpus, jaccard_similarity)


lsh |> filter(score >= 0.65 & score < 1)
corpus[["doc-12976"]]
corpus[["doc-8595"]]

refs_philosophy[UID_Ref == 'WOS:000288000800002', .(Cited_Author, Cited_Title, Cited_Work, Year, UID_Ref,N_UID_Ref)]
# 28 citations 

refs_philosophy[UID_Ref == "WOS:000288000800016", .(Cited_Author, Cited_Title, Cited_Work, Year, UID_Ref,N_UID_Ref)]
# 30 citations 

# same refs different UID_Ref. 
# DISTRIBUTION OF WORDS -------------------------------------------
# Function to count words
count_words <- function(text) {
  return(length(unlist(strsplit(as.character(text), "\\s+"))))
}

# Calculate word counts and add as a new column
my_data <- refs_philosophy[1:100] %>%
  mutate(word_count = sapply(full_info, count_words))

# Display the original text and its word count for each row
result <- my_data %>%
  select(full_info, word_count)

# Print the result
print(result, n = Inf)  # n = Inf will print all rows

# Summary statistics
summary_stats <- summary(my_data$word_count)
print("Summary Statistics:")
print(summary_stats)

# Calculate distribution
distribution <- table(my_data$word_count)
print("\nDistribution:")
print(distribution)

# Plot histogram
p <- ggplot(my_data, aes(x = word_count)) +
  geom_density(color = "#65ffa9", fill = "#65ffa9", alpha = 0.5, linewidth = 1) + 
  theme_minimal() +
  labs(title = "Distribution of Word Counts",
       x = "Number of Words",
       y = "Frequency")

ggplotly(p)

# Save the plot (optional)


# Calculate percentiles
percentiles <- quantile(my_data$word_count, probs = seq(0, 1, 0.1))
print("\nPercentiles:")
print(percentiles)

# Print top 10 rows with highest word counts
print("\nTop 10 rows with highest word counts:")
top_10 <- my_data %>%
  arrange(desc(word_count)) %>%
  select(full_info, word_count) %>%
  head(10)
print(top_10)
