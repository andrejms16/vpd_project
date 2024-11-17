library(readxl)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(showtext)
library(ggalluvial)
library(treemapify)
library(plotly)
library(scales)
library(ggflags)


# Loading fonts
showtext_auto()
font_add_google("Nunito", "nunito")

# Loading data
df <- read_excel("data/df_waste_destination.xlsx")


# Pivot years
df <- df %>%
  pivot_longer(
    cols = starts_with("q_"),
    names_to = "year",
    values_to = "quantity"
  )

# Fix some columns
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
  filter(year == 2022, country != "European Union", wd_type != "Waste Treatment") %>%
  select(wd_type, country, quantity)

# Treemap

# Agregar Nunito desde Google Fonts
font_add_google(name = "Nunito", family = "Nunito")

# Activar showtext
showtext_auto()

min_area_threshold <- 1000

library(treemapify); library(plotly); library(scales)

# Create the treemap
p <- ggplot(biomass_waste, aes(
  area = quantity,
  fill = wd_type,
  subgroup = country,
  label = ifelse(quantity < min_area_threshold, "", paste(comma(quantity), " tonnes"))  # Solo mostrar etiquetas si el área es mayor que el umbra
)) +
  geom_treemap() +
  geom_treemap_subgroup_border(colour = "white", size = 10) + # Bordes entre países
  geom_treemap_subgroup_border(colour = "black", size = 2) +  # Marco delgado alrededor de todos los países
  geom_treemap_subgroup_text(
    place = "centre",            # Posicionar etiquetas en la parte superior
    alpha = 0.2,
    colour = "black",
    # grow = TRUE
    size = 50,  # Ajustar el tamaño fijo de la etiqueta del país
    fontface = "bold"  
  ) +
  geom_treemap_text(
    colour = "black",
    place = "centre",
    grow = FALSE
  ) +
  scale_fill_manual(values = c(
    "Disposal Landfill Others" = "#E0CA3C",  # Amarillo vibrante
    "Disposal Incineration" = "#A63A50",     # Rojo profundo
    "Recycling" = "#048A81",                 # Verde oscuro
    "Waste Treatment" = "#A799B7",          # Púrpura intenso
    "Recovery Energy" = "#93B7BE"           # Azul claro
  )) +
  labs(title = "Proportion of Biomass Waste Disposal Methods",
       subtitle = "By countries in the EU, values in tonnes") +
  theme_minimal() +
  theme(
    plot.title = element_text(family = "Nunito", size = 40, face = "bold", hjust = 0.5), # Título
    plot.subtitle = element_text(family = "Nunito", size = 30, hjust = 0.5),
    legend.title = element_text(family = "Nunito", size = 20),             # Título de la leyenda
    legend.text = element_text(family = "Nunito", size = 18),              # Texto de la leyenda
    axis.text = element_text(family = "Nunito"),                           # Texto de los ejes
    axis.title = element_text(family = "Nunito")                           # Títulos de los ejes
  )
p
# Convertir a un gráfico interactivo
# ggplotly(p, tooltip = "label")  # Define qué mostrar en el tooltip

q <- ggplot(biomass_waste, aes(
  area = quantity,
  fill = wd_type,
  subgroup = wd_type,
  label = ifelse(quantity < min_area_threshold, "", paste(country))  # Solo mostrar etiquetas si el área es mayor que el umbra
)) +
  geom_treemap() +
  geom_treemap_subgroup_border(colour = "white", size = 10) + # Bordes entre países
  geom_treemap_subgroup_border(colour = "black", size = 2) +  # Marco delgado alrededor de todos los países
  geom_treemap_subgroup_text(
    place = "centre",            # Posicionar etiquetas en la parte superior
    alpha = 0.2,
    colour = "black",
    # grow = TRUE
    size = 50,  # Ajustar el tamaño fijo de la etiqueta del país
    fontface = "bold"  
  ) +
  geom_treemap_text(
    colour = "black",
    place = "centre",
    grow = FALSE
  ) +
  scale_fill_manual(values = c(
    "Disposal Landfill Others" = "#E0CA3C",  # Amarillo vibrante
    "Disposal Incineration" = "#A63A50",     # Rojo profundo
    "Recycling" = "#048A81",                 # Verde oscuro
    "Waste Treatment" = "#A799B7",          # Púrpura intenso
    "Recovery Energy" = "#93B7BE"           # Azul claro
  )) +
  labs(title = "Proportion of Biomass Waste Disposal Methods",
       subtitle = "By countries in the EU, values in tonnes") +
  theme_minimal() +
  theme(
    plot.title = element_text(family = "Nunito", size = 40, face = "bold", hjust = 0.5), # Título
    plot.subtitle = element_text(family = "Nunito", size = 30, hjust = 0.5),
    legend.title = element_text(family = "Nunito", size = 20),             # Título de la leyenda
    legend.text = element_text(family = "Nunito", size = 18),              # Texto de la leyenda
    axis.text = element_text(family = "Nunito"),                           # Texto de los ejes
    axis.title = element_text(family = "Nunito")                           # Títulos de los ejes
  )
q
