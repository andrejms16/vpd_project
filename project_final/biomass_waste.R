treemap_by_country <- function(df, custom_theme) {
  library(tidyverse)
  library(ggplot2)
  library(dplyr)
  library(showtext)
  library(treemapify)
  library(scales)
  library(ggflags)
  library(countrycode)
  
  # Load fonts
  showtext_auto()
  font_add_google("Nunito", "nunito")
  
  # Pivot years
  df <- df %>%
    pivot_longer(cols = starts_with("q_"),
                 names_to = "year",
                 values_to = "quantity")
  
  # Feature engineering
  df$year <- gsub("q_", "", df$year)
  df$year <- as.numeric(df$year)
  df$wd_type <- df$wd_type %>%
    gsub("_", " ", .) %>%
    tools::toTitleCase()
  
  # Calculate waste destination
  waste_destination <- df %>%
    filter(measure == "tonnes") %>%
    select(wd_type, country, year, quantity)
  
  
  biomass_waste <- waste_destination %>%
    filter(year == 2022,
           country != "European Union",
           wd_type != "Waste Treatment") %>%
    select(wd_type, country, quantity) %>%
    mutate(wd_type = ifelse(wd_type == "Disposal Landfill Others", "Disposal Landfill & Others", wd_type)) %>%
    rename("disposal_method" = "wd_type")
  
  biomass_waste$country_code <- countrycode(biomass_waste$country,
                                            origin = "country.name",
                                            destination = "iso2c")
  
  
  # Ploting the treemap
  
  
  font_add_google(name = "Nunito", family = "Nunito")
  showtext_auto()
  
  min_area_threshold <- 1000
  
  # Create the treemap
  p <- ggplot(biomass_waste,
              aes(
                area = quantity,
                fill = disposal_method,
                subgroup = country,
                label = ifelse(quantity < min_area_threshold, "", paste(comma(quantity), " tonnes"))  # Solo mostrar etiquetas si el área es mayor que el umbra
              )) +
    geom_treemap() +
    geom_treemap_subgroup_border(colour = "white", size = 10) + # Bordes entre países
    geom_treemap_subgroup_border(colour = "black", size = 2) +  # Marco delgado alrededor de todos los países
    geom_treemap_subgroup_text(
      place = "centre",
      # Posicionar etiquetas en la parte superior
      alpha = 0.2,
      colour = "black",
      # grow = TRUE
      size = 50,
      # Ajustar el tamaño fijo de la etiqueta del país
      fontface = "bold"
    ) +
    geom_treemap_text(colour = "black",
                      place = "centre",
                      grow = FALSE) +
    scale_fill_manual(
      values = c(
        "Disposal Landfill & Others" = "#E0CA3C",
        # Amarillo vibrante
        "Disposal Incineration" = "#DA95A3",
        # Rojo profundo
        "Recycling" = "#048A81",
        # Verde oscuro
        "Waste Treatment" = "#A799B7",
        # Púrpura intenso
        "Recovery Energy" = "#93B7BE"           # Azul claro
      )
    ) +
    # geom_flag(aes(x = 0.5, y = 0.5, country = country_code), size = 8) +
    labs(title = "Proportion of Biomass Waste Disposal Methods",
         subtitle = "By countries, values in tonnes",
         fill = "Waste Disposal Methods") +
    theme_minimal() +
    custom_theme
  return(p)
}

treemap_by_type <- function(df, custom_theme) {
  library(tidyverse)
  library(ggplot2)
  library(dplyr)
  library(showtext)
  library(treemapify)
  library(scales)
  library(ggflags)
  library(countrycode)
  
  # Load fonts
  showtext_auto()
  font_add_google("Nunito", "nunito")
  
  # Pivot years
  df <- df %>%
    pivot_longer(cols = starts_with("q_"),
                 names_to = "year",
                 values_to = "quantity")
  
  # Feature engineering
  df$year <- gsub("q_", "", df$year)
  df$year <- as.numeric(df$year)
  df$wd_type <- df$wd_type %>%
    gsub("_", " ", .) %>%
    tools::toTitleCase()
  
  # Calculate waste destination
  waste_destination <- df %>%
    filter(measure == "tonnes") %>%
    select(wd_type, country, year, quantity)
  
  
  biomass_waste <- waste_destination %>%
    filter(year == 2022,
           country != "European Union",
           wd_type != "Waste Treatment") %>%
    select(wd_type, country, quantity) %>%
    mutate(wd_type = ifelse(wd_type == "Disposal Landfill Others", "Disposal Landfill & Others", wd_type))  %>%
    rename("disposal_method" = "wd_type")
  
  biomass_waste$country_code <- countrycode(biomass_waste$country,
                                            origin = "country.name",
                                            destination = "iso2c")
  
  font_add_google(name = "Nunito", family = "Nunito")
  showtext_auto()
  
  min_area_threshold <- 1000
  
  q <- ggplot(biomass_waste,
              aes(
                area = quantity,
                fill = disposal_method,
                subgroup = disposal_method,
                label = ifelse(quantity < min_area_threshold, "", paste(country))  # Solo mostrar etiquetas si el área es mayor que el umbra
              )) +
    geom_treemap() +
    geom_treemap_subgroup_border(colour = "white", size = 10) + # Bordes entre países
    geom_treemap_subgroup_border(colour = "black", size = 2) +  # Marco delgado alrededor de todos los países
    geom_treemap_subgroup_text(
      place = "centre",
      # Posicionar etiquetas en la parte superior
      alpha = 0.2,
      colour = "black",
      # grow = TRUE
      size = 50,
      # Ajustar el tamaño fijo de la etiqueta del país
      fontface = "bold"
    ) +
    geom_treemap_text(colour = "black",
                      place = "centre",
                      grow = FALSE) +
    scale_fill_manual(
      values = c(
        "Disposal Landfill & Others" = "#E0CA3C",
        # Amarillo vibrante
        "Disposal Incineration" = "#DA95A3",
        # Rojo profundo
        "Recycling" = "#048A81",
        # Verde oscuro
        "Waste Treatment" = "#A799B7",
        # Púrpura intenso
        "Recovery Energy" = "#93B7BE"           # Azul claro
      )
    ) +
    labs(title = "Proportion of Biomass Waste Disposal Methods", 
         subtitle = "By disposal method, values in tonnes",
         fill = "Waste Disposal Methods") +
    theme_minimal() +
    custom_theme
  
  return(q)
}
