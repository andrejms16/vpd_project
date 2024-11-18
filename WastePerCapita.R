library(readxl)
library(tidyverse)
library(ggplot2)

#Load dataset
foodwaste <- read_excel("data/df_foodwaste.xlsx")


# Pivot waste origin
foodwaste <- foodwaste %>%
  pivot_longer(
    cols = c(
      "primary_production",
      "manufacture",
      "retail",
      "restaurants",
      "households",
      "total"
    ),
    names_to = "origin",
    values_to = "quantity"
  )


#Fix labels and filter rows
foodwaste <- foodwaste %>%
  mutate(origin = str_to_title(gsub("_", " ", origin))) %>%
  mutate(metric = gsub("_", " ", metric))


#Set data types
foodwaste <- foodwaste %>%
  mutate(
    country = as.factor(country),
    year = as.numeric(year),
    metric = as.factor(metric),
    origin = as.factor(origin),
    quantity = as.numeric(quantity)
  )


#Filter data of interest
foodwaste <- foodwaste %>%
  filter(
    year == 2022,
    metric == "kg per capita",
    country != "European Union",
    origin != 'Total',
    !is.na(quantity)
  )


#Calculate proportions
proportions <- foodwaste %>%
  group_by(country) %>%
  summarize(total_waste = sum(quantity, na.rm = TRUE)) %>%
  mutate(avg_consumption_per_capita = 698.5) %>%
  mutate(Waste_Proportion = total_waste / avg_consumption_per_capita) %>%
  mutate(Consumption_Proportion = 1 - Waste_Proportion)


proportions_plot = proportions %>%
  pivot_longer(
    cols = c("Waste_Proportion", "Consumption_Proportion"),
    names_to = "category",
    values_to = "values"
  )





#Generate plot
ggplot(proportions_plot, aes(
  x = values,
  y = reorder(country, -total_waste),
  fill = category
)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c(
    "Waste_Proportion" = "#B54057",
    "Consumption_Proportion" = "#048A81"
  )) +
  labs(
    title = "Proportion of food waste vs food consumption in 2022",
    subtitle = "proportion calculated on an average 608Kg food consumed per capita in the EU per year",
    x = "Proportion",
    y = "Countries",
    fill = "Categoría"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      family = "Nunito",
      size = 40,
      face = "bold",
      hjust = 0.5
    ),
    # Título
    plot.subtitle = element_text(
      family = "Nunito",
      size = 20,
      hjust = 0.5
    ),
    legend.title = element_text(family = "Nunito", size = 20),
    # Título de la leyenda
    legend.text = element_text(family = "Nunito", size = 18),
    # Texto de la leyenda
    axis.text = element_text(family = "Nunito", size = 15),
    # Texto de los ejes
    axis.title = element_text(family = "Nunito", size = 18)                           # Títulos de los ejes
  )
