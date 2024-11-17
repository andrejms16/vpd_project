library(readxl); library(tidyverse); library(ggplot2); library(dplyr); library(showtext); library(networkD3); library(ggalluvial)
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
  filter(year == 2020, country != "European Union") %>%
  select(wd_type, country, quantity)


ggplot(sankey_test,
       aes(axis1 = country, axis2 = wd_type , y = quantity)) +
  geom_alluvium(aes(fill = country), width = 0.2) +
  geom_stratum(width = 0.2, fill = "grey") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  labs(title = "Diagrama de Sankey del Desperdicio por País y Categoría",
       x = "Flujo de Desperdicios",
       y = "Cantidad (toneladas)") +
  theme_minimal()



nodes <- data.frame(name = c(as.character(unique(sankey_test$country)),
                      as.character(unique(sankey_test$wd_type))))
                             

links <- data.frame(
  source = match(sankey_test$country, nodes$name) - 1,
  target = match(sankey_test$wd_type, nodes$name) + length(unique(sankey_test$country)) - 1,
  value = sankey_test$quantity
)

# Create sankey diagram
sankey <- sankeyNetwork(
  Links = links, 
  Nodes = nodes, 
  Source = "source", 
  Target = "target", 
  Value = "value", 
  NodeID = "name", 
  units = "toneladas"
)

# Show diagram
sankey


