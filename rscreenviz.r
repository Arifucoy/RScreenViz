#Packages
library(tidyverse)
library(googlesheets4)
library(scales)
gs4_deauth() #not looking to make changes to original file, just want to read it.

#Establishing Google Sheets connection
ssid <- "1GoKS0ZXUohPGWAc2E8ClZINNNHI9Lj-2XoaVoCvBlDk"
screendf <- range_read(ssid, sheet = "1. First pass")

#Bar graph colour scheme
scheme = c("PASS" = "#00BA38",
           "IN NCBI" = "#619CFF",
           "NOT SURE" = "#FFD64C",
           "FAIL" = "#F8766d")

#First pass overview bargraph
overview <- screendf %>%
  select(outbreak_screen:full_dataset) %>%  
  gather(key = Criteria, value = Outcome) #Putting columns of interest into long format

#Sorting and variables for making graphs cleaner/nitpicking
overview$Outcome <- factor(overview$Outcome, levels = c("PASS", "IN NCBI", "NOT SURE", "FAIL"))
criteria_labels = c("outbreak_screen" = "Outbreak Screen",
                    "sequence_screen" = "Sequence Screen",
                    "mappable" = "Mappable",
                    "full_dataset" = "Full Dataset")

overviewgraph <- ggplot(overview, aes(x = Outcome, fill = Outcome)) + 
  geom_bar(width = 0.5) +
  #Changing bar fill colours to scheme above
  scale_fill_manual(values = scheme) +
  #Sorting facet wrap into order of columns on sheet
  facet_wrap(~factor(Criteria, levels = c("outbreak_screen", "sequence_screen", "mappable", "full_dataset"), labels = criteria_labels), scales = "free_x") + 
  ggtitle("Bacterial Outbreak Metadata Extraction: First Screening") +
  labs(x = "Outcome", y = "Count") +
  #Adding text of respective count and percent of the criteria
  geom_text(stat = "count", size = 18, aes(label = paste0(after_stat(count), "\n", 
                                                          percent(accuracy = 0.01, after_stat(count)/((sum(after_stat(count))/4))))), vjust = -0.15) +
  coord_cartesian(ylim = c(0, 120)) +
  theme_bw(base_size = 48) +
  theme(legend.position = "none")
ggsave("overviewfig.png", overviewgraph, width = 32, height = 24, dpi = 96, units = "in")

#Per organism graph
#Wrangle data to make data frame orientated around organisms
per_bacteria <- screendf %>% 
  select(organism, outbreak_screen:full_dataset) %>% 
  #Create new column to take all coloumns into criteria and have an overall decision on the validity of the paper per organism
  mutate(Validity = case_when(if_all(outbreak_screen:mappable, ~ . %in% c("PASS", "IN NCBI")) ~ "PASS", #If all columns have PASS
                              if_any(outbreak_screen:mappable, ~ . == "FAIL") ~ "FAIL", #if any column has FAIL
                              if_any(outbreak_screen:mappable, ~ . == "NOT SURE" & . != "FAIL") ~ "NOT SURE" #if any column has NOT SURE, as long as there isn't FAIL
  )
  ) %>% 
  #Some papers have more than one organism, separate
  separate_rows(organism, sep = "\\s*\\|\\s*") %>%  #separate base on | character, and if there are 0 or more spaces before and after the |
  filter(organism != "") %>% #one record had a | but no organism after
  droplevels() %>% 
  mutate(species_revised = if_else(table(organism)[organism] == 1, "Other", organism)) #to group species that only show up once into one group so that graph isn't long

bacteriaplot <- ggplot(per_bacteria, aes(x = reorder(species_revised, table(species_revised)[species_revised]), fill = Validity)) + #Reorder from ascending counts to down
  geom_bar(width = 0.9) + 
  scale_fill_manual(values = scheme) + #Fill colour scheme
  theme(legend.position = "none") +
  ggtitle("Bacterial Outbreak Metadata Extraction: First Screening") +
  labs(x = "Organism", y = "Count", caption = paste0("Bars are only labelled if their count is ≥ 2", "\n", "`Other` organism category represents species where n = 1.")) +
  #geom_text to label only those that have a count of >1 so that it doesn't take up too much space, and text can be bigger.
  geom_text(stat = "count", size = 12, aes(group = Validity, label = if_else(after_stat(count) > 1, paste0(after_stat(count), "\n(", percent(accuracy = 0.01, after_stat(count)/sum(after_stat(count))), ")"), "")), position = position_stack(vjust = 0.5)) +
  coord_cartesian(ylim = c(0, 15)) +
  theme_bw(base_size = 48) +
  theme(legend.position = "none",
        plot.caption = element_text(hjust = 0, size = 48)) +
  coord_flip()
ggsave("bacteriafig.png", bacteriaplot, width = 36, height = 36, dpi = 96, units = "in")
