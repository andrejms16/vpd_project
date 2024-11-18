library(readxl); library(tidyverse); library(ggplot2); library(dplyr); library(showtext); library(ggalluvial)
# Loading fonts
showtext_auto()
font_add_google("Nunito", "nunito")


# Loading the datasets into R variables
df_1 <- read_excel("data/df_foodwaste.xlsx")
df_2 <- read_excel("data/df_waste_destination.xlsx")

# Pivot both datasets
df_1 <- df_1 %>%
  pivot_longer(
    cols = c("total", "primary_production", "manufacture", "retail", "restaurants", "households"),
    names_to = "waste_type",
    values_to = "quantity"
  )

df_2 <- df_2 %>%
  pivot_longer(
    cols = starts_with("q_"),
    names_to = "year",
    values_to = "quantity"
  )

df_2$year <- gsub("q_", "", df_2$year)
df_2$year <- as.numeric(df_2$year)

# Calculate total waste
total_waste <- df_1 %>%
  filter(waste_type == "total",
         metric == "tonne",
         country != "European Union")


# Calculate waste destination
waste_destination <- df_2 %>%
  filter(measure == "tonnes") %>%
  select(wd_type, country, year, quantity)
  
  
# Create a basic barplot
ggplot(total_waste, aes(x = quantity , y = country, fill = factor(year))) +
  geom_bar(stat = "identity", position = "dodge") +  # "identity" porque ya tenemos las cantidades
  labs(title = "Total amount of waste per year in the european union",
       x = "Country",
       y = "Total amount of waste in tonnes",
       fill = "Year") +  
  theme_minimal() +  # Estilo de gráfico minimalista
  scale_fill_viridis_d() +  # Opcional: Usar una paleta de colores agradable para el fill
  # coord_flip() +
  theme(
    plot.title = element_text(hjust = 0.5, vjust = 2, size = 30, face = "bold", family = "nunito"),
    plot.margin = margin(t = 30, r = 10, b = 10, l = 10),  # Aumentar el espacio arriba del gráfico
    axis.title = element_text(size = 16, family = "nunito"),  # Títulos de los ejes más grandes
    axis.text = element_text(size = 14, family = "nunito"),  # Valores de los ejes más grandes
    legend.title = element_text(size = 16, family = "nunito"),  # Título de la leyenda más grande
    legend.text = element_text(size = 14, family = "nunito") 
  )

# Create a Sankey diagram
sankey_test <- waste_destination %>%
  filter(year == 2020, country != "European Union", wd_type != "waste_treatment") %>%
  select(wd_type, country, quantity)


# Treemap

# Agregar Nunito desde Google Fonts
font_add_google(name = "Nunito", family = "Nunito")

# Activar showtext
showtext_auto()

min_area_threshold <- 1000

library(treemapify); library(plotly); library(scales)
biomass_waste <- sankey_test
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
    "disposal_landfill_others" = "#E0CA3C",  # Amarillo vibrante
    "disposal_incineration" = "#A63A50",     # Rojo profundo
    "recycling" = "#048A81",                 # Verde oscuro
    "waste_treatment" = "#A799B7",          # Púrpura intenso
    "recovery_energy" = "#93B7BE"           # Azul claro
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
    "disposal_landfill_others" = "#E0CA3C",  # Amarillo vibrante
    "disposal_incineration" = "#A63A50",     # Rojo profundo
    "recycling" = "#048A81",                 # Verde oscuro
    "waste_treatment" = "#A799B7",          # Púrpura intenso
    "recovery_energy" = "#93B7BE"           # Azul claro
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














# Sankey diagram
ggplot(sankey_test,
       aes(axis1 = country, axis2 = wd_type , y = quantity)) +
  geom_alluvium(aes(fill = country), width = 0.2) +
  geom_stratum(width = 0.2, fill = "grey") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  labs(title = "Diagrama de Sankey del Desperdicio por País y Categoría",
       x = "Flujo de Desperdicios",
       y = "Cantidad (toneladas)") +
  theme_minimal()





