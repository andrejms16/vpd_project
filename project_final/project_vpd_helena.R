#setwd("C:/Users/helen/data_science/vpd_r") # sets working dir

# load the necessary packages
#install.packages("readxl")
library(readxl); 
library(tidyverse); 
library(quantmod); 
library(tidyquant); 
library(geomtextpath)
library( ggplot2)
library(tidyquant)
library(dplyr)
#install.packages("showtext")
library(showtext)
font_add_google("Roboto", "Roboto")
showtext_auto()
library(ggplot2)
library(scales)
library(showtext)
#install.packages("paletteer_d") devtools::install_github("EmilHvitfeldt/paletteer")
#library(paletteer)
#library(paletteer_d)

library(patchwork)
library(countrycode)
library(ggflags)

# Load the SteppedSequential5Steps palette
#color_palette <- as.character(paletteer_d("colorBlindness::SteppedSequential5Steps"))
#color_palette
# Add Roboto font
font_add_google("Roboto", "Roboto")
showtext_auto()

df_fw <- read_excel("./datasets/df_foodwaste.xlsx", sheet = 1)
df_wd <- read_excel("./datasets/df_waste_destination.xlsx", sheet = 1)

df_wd
df_fw

df_wd_wide <- df_wd %>%
  # Select relevant columns, drop "2020" and "2021"
  select(-c('2020.0', '2021.0', waste_material_type,waste_destination_type)) %>%
  # Ensure "2022" is numeric
  #mutate('2022.0' = as.numeric('2022')) %>%
  # Reshape into wide format for wd_type
  pivot_wider(names_from = wd_type, values_from  ='2022.0') %>%
  # percentage of total waste
  mutate(perc_waste_treatment = waste_treatment/waste_treatment) %>%
  mutate(perc_recycling = recycling/waste_treatment) %>%
  mutate(perc_disposal_incineration = disposal_incineration/waste_treatment) %>%
  mutate(perc_disposal_landfill_others = disposal_landfill_others/waste_treatment) %>%
  mutate(perc_recovery_energy = recovery_energy /waste_treatment) 

# View the resulting dataset
df_wd_wide

# Dataset for raking of countries - recycling waste destination
ranking_recycling_wd <- df_wd_wide %>%
  # Drop columns
  select(c(measure, country, recycling, perc_recycling )) %>%
  # Filter for recycling and tonnes per capita
  filter(measure == "tonnes_per_capita") %>%
  # Rename column 2022
  rename(recycling_tonnes_per_capita = recycling) %>%
  # Transforming tonnes per capita to kg per capita
  mutate(recycling_kg_per_capita = as.numeric(recycling_tonnes_per_capita)*1000) %>%
  # Order by kg_per_capita_2022 in descending order
  arrange(desc(perc_recycling))

ranking_recycling_wd
# Extract top 5 rows
top_5 <- ranking_recycling_wd %>%
  slice_head(n = 5)

# Extract bottom 5 rows
bottom_5 <- ranking_recycling_wd %>%
  slice_tail(n = 5)

# Extract European Union row
eu <- ranking_recycling_wd %>%
  filter(country == "European Union")

# Combine all rows, ensuring no duplicates
rkg_recycling_wd <- bind_rows(top_5, eu, bottom_5) %>%
  distinct()

rkg_recycling_wd
# Classify countries into categories
rkg_recycling_wd <- rkg_recycling_wd %>%
  mutate(category = case_when(
    row_number() <= 5 ~ "Top 5",                 # First 5 rows as "Top 5"
    row_number() > (n() - 5) ~ "Bottom 5",       # Last 5 rows as "Bottom 5"
    country == "European Union" ~ "European Union" # "European Union" row
  ))

rkg_recycling_wd 
#<- rkg_recycling_wd + mutate(rkg_recycling_wd , countrycode = tolower(ifelse(country == "European Union","EU", countrycode(country, origin = 'country.name', destination = 'iso2c'))))


# Dataset for raking of countries - recycling waste destination
ranking_landfill_wd <- df_wd_wide %>%
  # Drop columns
  select(c(measure, country, disposal_landfill_others, perc_disposal_landfill_others )) %>%
  # Filter for recycling and tonnes per capita
  filter(measure == "tonnes_per_capita", perc_disposal_landfill_others > 0) %>%
  # Rename column 2022
  rename(landfill_tonnes_per_capita = disposal_landfill_others) %>%
  # Transforming tonnes per capita to kg per capita
  mutate(landfill_kg_per_capita = as.numeric(landfill_tonnes_per_capita)*1000) %>%
  # Order by kg_per_capita_2022 in descending order
  arrange(desc(perc_disposal_landfill_others))

ranking_landfill_wd
# Extract top 5 rows
top_5 <- ranking_landfill_wd %>%
  slice_head(n = 5)

# Extract bottom 5 rows
bottom_5 <- ranking_landfill_wd %>%
  slice_tail(n = 5)

# Extract European Union row
eu <- ranking_landfill_wd %>%
  filter(country == "European Union")

# Combine all rows, ensuring no duplicates
rkg_landfill_wd <- bind_rows(top_5, eu, bottom_5) %>%
  distinct()

rkg_landfill_wd
# Classify countries into categories
rkg_landfill_wd <- rkg_landfill_wd %>%
  mutate(category = case_when(
    row_number() <= 5 ~ "Top 5",                 # First 5 rows as "Top 5"
    row_number() > (n() - 5) ~ "Bottom 5",       # Last 5 rows as "Bottom 5"
    country == "European Union" ~ "European Union" # "European Union" row
  ))
rkg_landfill_wd

rkg_recycling_wd <- mutate(rkg_recycling_wd , countrycode = tolower(ifelse(country == "European Union","EU", countrycode(country, origin = 'country.name', destination = 'iso2c'))))

# Horizontal bar plot with conditional coloring
p_rkg_recy <- ggplot(rkg_recycling_wd, aes(x = perc_recycling, y = reorder(country, perc_recycling), fill = category)) +
  geom_col() +
  geom_text(
    aes(label = scales::percent(perc_recycling, accuracy = 1)), # Format as percentage with no decimals
    hjust = 1.1, color = "white", size = 5, fontface = "bold"
  ) +
  geom_flag(x = 0, aes(country = countrycode), size = 8) +
  labs(
    title = "Food Waste - Recycling destination",
    subtitle = "Share by Country (Top 5, Bottom 5, and European Union)"
  ) +
  scale_fill_manual(
    values = c("Top 5" = "#008C8C", "European Union" = "gray", "Bottom 5" = "#C41E3A"), # Custom colors
    breaks = c("Top 5", "European Union", "Bottom 5"), # Order in the legend
    guide = guide_legend(title = NULL)                # Remove legend title
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Roboto"),                 # Apply Roboto font globally
    axis.text.y = element_text(size = 12),                  # Keep y-axis text
    legend.position = "none",                          # Position legend at bottom right
    axis.title.x = element_blank(),                         # Remove x-axis title
    axis.text.x = element_blank(),                          # Remove x-axis text
    axis.ticks.x = element_blank(),                         # Remove x-axis ticks
    axis.title.y = element_blank(),                         # Remove y-axis title
    axis.ticks.y = element_blank(),                         # Remove y-axis ticks
    panel.grid.major.y = element_blank(),                   # Remove major y-axis gridlines
    panel.grid.minor.x = element_blank(),                   # Remove minor x-axis gridlines
    plot.title = element_text(hjust = 0, face = "bold", size = 18),  # Center and bold the title
    plot.subtitle = element_text(hjust = 0, size = 11),              # Center the subtitle
    plot.title.position = "plot"                            # Align title with the plot area
  )

# Display the plot
p_rkg_recy

rkg_landfill_wd <- mutate(rkg_landfill_wd , countrycode = tolower(ifelse(country == "European Union","EU", countrycode(country, origin = 'country.name', destination = 'iso2c'))))

# Horizontal bar plot with conditional coloring
p_rkg_landfill <- ggplot(rkg_landfill_wd, aes(x = perc_disposal_landfill_others, y = reorder(country, perc_disposal_landfill_others), fill = category)) +
  geom_col() +
  geom_text(
    aes(label = scales::percent(perc_disposal_landfill_others, accuracy = 0.1)), # Format as percentage with no decimals
    hjust = -0.1, color = "black", size = 5, fontface = "bold"
  ) +
  geom_flag(x = 0, aes(country = countrycode), size = 8) +
  labs(
    title = "Food Waste - Landfill destination",
    subtitle = "Share by Country (Top 5, Bottom 5, and European Union)"
  ) +
  scale_fill_manual(
    values = c("Top 5" = "#C41E3A", "European Union" = "gray", "Bottom 5" = "#008C8C"), # Custom colors
    breaks = c("Top 5", "European Union", "Bottom 5"), # Order in the legend
    guide = guide_legend(title = NULL)                # Remove legend title
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Roboto"),                 # Apply Roboto font globally
    axis.text.y = element_text(size = 12),                  # Keep y-axis text
    legend.position = c(0.9, 0.1),                          # Position legend at bottom right
    legend.justification = c(1, 0),                         # Justify legend position
    legend.box.background = element_rect(color = "gray"),   # Add a frame to the legend
    legend.margin = margin(4, 4, 4, 4),                     # Add spacing around the legend
    axis.title.x = element_blank(),                         # Remove x-axis title
    axis.text.x = element_blank(),                          # Remove x-axis text
    axis.ticks.x = element_blank(),                         # Remove x-axis ticks
    axis.title.y = element_blank(),                         # Remove y-axis title
    axis.ticks.y = element_blank(),                         # Remove y-axis ticks
    panel.grid.major.y = element_blank(),                   # Remove major y-axis gridlines
    panel.grid.minor.x = element_blank(),                   # Remove minor x-axis gridlines
    plot.title = element_text(hjust = 0, face = "bold", size = 18),  # Center and bold the title
    plot.subtitle = element_text(hjust = 0, size = 11),              # Center the subtitle
    plot.title.position = "plot"                            # Align title with the plot area
  )

# Display the plot
p_rkg_landfill