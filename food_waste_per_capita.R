# Define a function 'food_waste_per_capita' that takes 'data' and 'custom_theme' as arguments
food_waste_per_capita <- function(data, custom_theme) {
  
  # Load necessary libraries for data manipulation, visualization, and mapping
  library(ggplot2)     # Data visualization library
  library(sf)          # Simple Features for spatial data
  library(dplyr)       # Data manipulation library
  library(readxl)      # Reading Excel files
  library(tidyverse)   # Collection of R packages for data science
  library(rnaturalearth)    # Access to Natural Earth map data
  library(rnaturalearthdata) # Data for natural earth maps
  library(data.table)  # Data manipulation for large datasets
  library(plotly)      # Interactive plots
  library(lubridate)   # Date and time handling
  library(grid)        # Grid-based graphics
  library(shadowtext)  # Text with shadow in ggplot
  library(gghighlight) # Highlighting specific data points in ggplot
  library(ggflags)     # Flags for countries in ggplot
  library(countrycode) # Convert country names to country codes
  
  # Optional: Install ggflags package from GitHub
  # remotes::install_github('rensa/ggflags')
  
  #### Data Selection and Cleaning
  # Filter data for the metric "kg_per_capita" and year "2022"
  data <- filter(data, metric == "kg_per_capita" & year == "2022")
  
  # Remove rows with more than one missing value
  data <- data[rowSums(is.na(data)) < 2 , ]
  
  #### Ranking of Food Waste Per Capita (Second Plot)
  # Select necessary columns and sort by 'households'
  data <- data %>%
    dplyr::select(country , households) %>% arrange(desc(households))
  
  # Reorder countries based on the 'households' value
  data <- mutate(data, country = fct_reorder(country, households))
  
  # Filter top 5, bottom 5, and "European Union" for ranking
  ranking <- filter(data,
                    row_number() <= 5 |
                      row_number() >= n() - 5 | country == "European Union")
  
  # Assign ranking categories (Top 5, European Union, Last 5)
  ranking <- mutate(ranking, waste_ranking = as.factor(ifelse(
    row_number() <= 5,
    "top_5",
    ifelse(
      country == "European Union",
      "European_Union",
      ifelse(row_number() >= n() - 5, "last_5", "")
    )
  )))
  
  # Convert country names to country codes (ISO 2-letter code)
  ranking <- mutate(ranking, countrycode = tolower(ifelse(
    country == "European Union",
    "EU",
    countrycode(country, origin = 'country.name', destination = 'iso2c')
  )))
  
  #### Create the Plot
  graph <- ggplot(ranking, aes(households, country))
  
  # Create a bar plot with flags and customized color scales
  graph <- graph +
    geom_col(aes(x = households, y = country, fill = waste_ranking), width = 0.6) +  # Bar chart with color by ranking
    geom_flag(x = 18, aes(country = countrycode), size = 10) +  # Add country flags
    scale_fill_manual(
      name = "Ranking of Food Waste per Country",
      labels = c("Top 5 Countries", "European Union", "Last 5 Countries"),
      values = c(
        "top_5" = "#B54057",   # Color for top 5 countries
        "European_Union" = "gray",   # Color for European Union
        "last_5" = "#048A81"   # Color for last 5 countries
      )
    ) +
    scale_x_continuous(
      limits = c(0, 130),  # Set limits for the x-axis
      breaks = seq(0, 140, by = 10),  # Set breaks for x-axis
      expand = c(0, 0),   # Remove extra space at both ends of the axis
      position = "top"  # Position the labels on the top of the bars
    ) +
    scale_y_discrete(expand = expansion(add = c(0, 0.5))) +  # Adjust y-axis space
    custom_theme +  # Apply custom theme passed as argument
    geom_text(
      aes(label = households),  # Add text labels for household values
      hjust = 0,  # Horizontal adjustment for text
      size = 7,  # Set font size for labels
      family = "Econ Sans Cnd"   # Set font family for labels
    ) +
    geom_text(
      aes(0, y = country, label = country),  # Add text labels for country names
      hjust = 0,
      nudge_x = 0.3,  # Shift the text slightly to the right
      colour = "white",  # Set text color to white
      family = "Econ Sans Cnd",  # Set font family for country names
      face = "bold",  # Make text bold
      size = 5  # Set font size for country names
    ) +
    labs(
      title = "Food Waste Per Capita in EU",   # Main title
      subtitle = "Total in KG of Waste Food per Capita, 2022",  # Subtitle
      x = "Quantity of Food Waste per Capita (KG)",  # X-axis label
      y = "Country",  # Y-axis label
      caption = "Source: Eurostat"  # Caption
    )
  
  # Return the generated plot
  return(graph)
}




