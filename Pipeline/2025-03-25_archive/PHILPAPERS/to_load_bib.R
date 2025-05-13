bib_files <- c(
  "interlevel-relations-in-biology.bib",
  "teleology-and-function.bib",
  "environmental-philosophy.bib",
  "systematic-biology.bib",
  "philosophy-of-biology-miscellaneous.bib",
  "genetics-and-molecular-biology.bib",
  "evolutionary-biology.bib",
  "ecology-and-conservation-biology.bib",
  "developmental-biology.bib"
)

# Function to combine specific bib files
combine_biology_bibs <- function(file_list) {
  # Initialize empty list to store bibliography entries
  combined_entries <- list()
  
  # Process each file
  for (file in file_list) {
    if (file.exists(file)) {
      tryCatch({
        # Read the bib file
        bib <- ReadBib(file)
        
        # Add entries to combined list
        combined_entries <- c(combined_entries, as.list(bib))
        
        cat(sprintf("Successfully processed: %s\n", file))
      }, error = function(e) {
        warning(sprintf("Error processing %s: %s", file, e$message))
      })
    } else {
      warning(sprintf("File not found: %s", file))
    }
  }
  
  # Convert combined entries back to bibliography object
  combined_bib <- as.BibEntry(combined_entries)
  
  # Remove potential duplicates
  combined_bib <- unique(combined_bib)
  
  # Save combined bibliography to a new file
  WriteBib(combined_bib, "combined_biology_bibliography.bib")
  
  # Return the combined bibliography object
  return(combined_bib)
}

# Execute the function
tryCatch({
  cat("Starting to process biology bibliography files...\n")
  combined_result <- combine_biology_bibs(bib_files)
  cat("\nCombination complete!\n")
  cat(sprintf("Total entries in combined bibliography: %d\n", length(combined_result)))
}, error = function(e) {
  cat(sprintf("Error: %s\n", e$message))
})


library(readr)
all_papers_philpapers <- read_csv("C:/Users/jacob/OneDrive - Université Laval/biophilo/Data/PHILPAPERS_PHILO_DATA/all_papers_philpapers.csv")
View(all_papers_philpapers)


