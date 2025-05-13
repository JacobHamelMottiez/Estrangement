# PACKAGES ----------------------------------------------------------------
library(dplyr)
library(refinr)
library(DT)
library(data.table)
library(readr)
library(ggplot2)
require(patchwork)
library(ggplot2)
library(ggrepel)

# BIOLOGY AND PHILOSOPHY --------------------------------------------------
# Names of the disciplines
disciplines_BP_2003_2015 <- tibble(
  discipline = c(
    "Agricultural Sciences",
    "Anthropology",
    "Applied Biological Sciences",
    "Biophysics & Computational Biology",
    "Cell Biology",
    "Environmental Sciences",
    "Immunology & Inflammation",
    "Microbiology",
    "Pharmacology",
    "Physiology",
    "Plant Biology",
    "Population Biology",
    "Sustainability Science",
    "Systems Biology",
    "Evolution",
    "Developmental Biology",
    "Biochemistry",
    "Psychological and Cognitive Sciences",
    "Neuroscience",
    "Medical Sciences",
    "Genetics",
    "Ecology"
  ),
  percentage = c(0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 62, 4, 2, 14, 3, 2, 5, 3),
  journal = "B&P"
)



disciplines_BP_1986_2002 <- tibble(
  discipline = c(
    "Evolution",
    "Others",
    "Reflections on Biology in general",
    "Evolutionary Ethics",
    "Evolutionary Epistemology",
    "Genetics",
    "Ecology",
    "Species",
    "Taxonomy",
    "Function, Teleology, Design"
  ),
  percentage = c(25, 25, 6, 11, 12, 3, 4, 9, 3, 4),
  journal = "B&P"
)


# Combine into a data frame (optional)
BP_tb <- rbind(disciplines_BP_1986_2002, disciplines_BP_2003_2015)




# ggplot
ggplot(data = BP_tb, mapping = aes(x=discipline, y = percentage, color = discipline, fill = discipline)) + 
  geom_bar(stat = "identity") + 
  theme(
    legend.position="bottom", 
    legend.box = "horizontal",
    legend.title = element_blank(),
    axis.text.x=element_blank(), 
    axis.ticks.x=element_blank())


# PNAS --------------------------------------------------------------------
# Names of the disciplines
disciplines_PNAS <- tibble(
  discipline = c(
    "Population Biology",
    "Plant Biology",
    "Physiology",
    "Pharmacology",
    "Neuroscience",
    "Microbiology",
    "Medical Sciences",
    "Immunology & Inflammation",
    "Genetics",
    "Biochemistry",
    "Biophysics & Computational Biology",
    "Cell Biology",
    "Ecology",
    "Evolution",
    "Developmental Biology",
    "Environmental Sciences",
    "Applied Biological Sciences",
    "Anthropology",
    "Agricultural Sciences",
    "Systems Biology",
    "Psychological and Cognitive Sciences",
    "Sustainability Science"
  ),
  percentage = c(0, 4, 2, 1, 12, 7, 10, 7, 5, 12, 9, 8, 3, 5, 3, 2, 2, 2, 1, 1, 3, 1)
  )

# Associated percentages



# Combine into a data frame (optional)
PNAS_tb <- disciplines_PNAS |> mutate(journal = "PNAS")

# Merged data.table
merged_tb  <- rbind(PNAS_tb, BP_tb)

p <- ggplot(data = PNAS_tb, mapping = aes(x=discipline, y = percentage, color = discipline, fill = discipline)) + 
  geom_bar(stat = "identity") + 
  theme(
  legend.position="bottom", 
  legend.box = "horizontal",
  legend.title = element_blank(),
  axis.text.x=element_blank(), 
  axis.ticks.x=element_blank(), 
) + ylim(c(0,60))


p1 <- ggplot(data = BP_tb, mapping = aes(x=discipline, y = percentage, color = discipline, fill = discipline)) + 
  geom_bar(stat = "identity") + 
  theme(
    legend.position="bottom", 
    legend.box = "horizontal",
    legend.title = element_blank(),
    axis.text.x=element_blank(), 
    axis.ticks.x=element_blank())

p2 <- ggplot(data = merged_tb, mapping = aes(x = discipline, y = percentage, fill = discipline)) + 
    geom_bar(stat = "identity") + 
    theme_gray() +  # Apply the default ggplot2 theme
    theme(
      panel.background = element_rect(fill = "gray90", color = NA),  # Force the gray background
      panel.grid.major = element_line(color = "white"),  # Keep white gridlines
      legend.position = "bottom", 
      legend.box = "horizontal",
      legend.title = element_blank(),
      axis.text.x = element_blank(), 
      axis.ticks.x = element_blank(), 
      legend.text = element_text(size=20), 
      axis.title.y = element_text(size=25),
      axis.text.y = element_text(size = 20), 
      strip.text.x = element_text(size = 30), 
      axis.title.x = element_text(size = 25)
    ) +
    xlab("Discipline") +
    ylab("Pourcentage") +
    facet_grid(~journal)
  
  print(p2)
  


# Malaterre et al. (2020) vs Pradeu (2017) --------------------------------
TM_dt <- data.table(
  Discipline = c("A-Evolution", 
               "B-Individuality-Altruism", 
               "C-Species-Ecology", 
               "D-Genetics-Development", 
               "E-Network-entropy-information", 
               "F-Cognition-behavior", 
               "G-Socio-normative issues", 
               "H-General philosophy of science", 
               "I-Others"),
  Percentage = c(18, 8, 9, 10, 6, 10, 9, 22, 8),
  Data = c("TM")
)


p3 <- ggplot(data = TM_dt, mapping = aes(x = Discipline, y = Percentage, fill = Discipline)) + 
  geom_bar(stat = "identity") + 
  theme(
    legend.position = "bottom", 
    legend.box = "horizontal",
    legend.title = element_blank(),
    axis.text.x = element_blank(), 
    axis.ticks.x = element_blank()
  ) + ylim(c(0,60))



# Create the data frame
data <- data.frame(
  Discipline = c("A-Evolution", "B-Individuality-Altruism", "C-Species-Ecology", 
                 "D-Genetics-Development", "E-Network-entropy-information", 
                 "F-Cognition-behavior", "G-Socio-normative issues", 
                 "H-General philosophy of science", "I-Others"),
  Percentage = c(18, 8, 9, 10, 6, 10, 9, 22, 8),
  Evolution = c(rep(TRUE, 3), rep(FALSE, 6))
)


# Create the "whole corpus" plot
whole_corpus_plot <- ggplot(data, aes(x = 1, y = -Percentage, fill = Discipline, alpha = Evolution)) +
  geom_col(width = 0.1) +
  coord_flip() +
  theme_void()+
  theme(legend.position = "none") +
  scale_alpha_discrete(range = c(0.35, 0.95)) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))
# Create the vertical bar plot
vertical_plot <- ggplot(data, aes(x = Discipline, y = Percentage, fill = Discipline)) +
  geom_col() +
  theme_minimal() +
  theme(
    legend.position = "bottom", 
    legend.box = "horizontal",
    legend.title = element_blank(),
    axis.text.x = element_blank(), 
    axis.ticks.x = element_blank()
  )

  # Combine the plots using patchwork
combined_plot <- (plot_spacer() + plot_layout(widths = c(38, 62))) / 
  whole_corpus_plot /
  vertical_plot + 
  plot_layout(heights = c(1, 0.3, 3))

# Display the combined plot
print(combined_plot)



# New plot 
p3+p1
















# New idea : Jitter and labels. 
full_merged_tb <- full_join(
  disciplines_BP_2003_2015 %>% select(discipline, percentage) %>% rename(percentage_BP = percentage),
  PNAS_tb %>% select(discipline, percentage) %>% rename(percentage_PNAS = percentage),
  by = "discipline"
) %>%
  # Replace NA with 0
  replace_na(list(percentage_BP = 0, percentage_PNAS = 0))

# View the merged tibble
ggplot(data = full_merged_tb, aes(x = percentage_BP, y = percentage_PNAS, color = discipline)) +
  geom_label_repel(aes(label = discipline), 
                   position = position_jitter(width = 1, height = 1, seed = 1),
                  size = 4, 
                  max.overlaps = 10, # Reduce label clutter
                  box.padding = 0.5, 
                  force = 10) +
  geom_point(position = position_jitter(width = 1, height = 1, seed = 1)) + # Add jitter to avoid overlap
  theme_minimal() + 
  geom_abline(intercept = 0, slope = 1, size = 0.5)+
  theme(legend.position = "none")



full_merged_tb <- full_merged_tb |> mutate(ratio = percentage_BP/percentage_PNAS)

ggplot(data = full_merged_tb, aes(x = percentage_BP, y = percentage_PNAS, color = discipline)) +
  geom_label_repel(aes(label = discipline), 
                   position = position_jitter(width = 1, height = 1, seed = 1),
                   size = 5, 
                   max.overlaps = 8, # Reduce label clutter
                   box.padding = 0.5, 
                   force = 20,) +
  geom_point(size = 4,position = position_jitter(width = 1, height = 1, seed = 1)) + # Add jitter to avoid overlap
  theme_minimal() + 
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  ggplot2::scale_x_sqrt()+
  theme(legend.position = "none") 









