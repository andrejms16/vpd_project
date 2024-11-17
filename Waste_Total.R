library(ggplot2)
library(sf)
library(dplyr)
library(readxl)
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(data.table)
library(plotly)
library( lubridate )
library(dplyr)
library(grid)
library(shadowtext)

#### Select Data Set
data = read_excel('./datasets/df_foodwaste.xlsx')
data <- filter(data,metric=="tonne" & year=="2022" )
data <- data[ rowSums( is.na( data ) ) < 2 , ]

#### First PLOT % FOOD WASTE in EU comes FROM Y
# Select Data 
 data <- data %>%
        dplyr::select( primary_production, manufacture, retail, restaurants, households )

data <- data %>%
  pivot_longer(
    cols = c("primary_production", "manufacture", "retail", "restaurants", "households"),
    names_to = "waste_type",
    values_to = "quantity"
  )

# Calculate total waste
data <- aggregate(quantity ~ waste_type, data, sum)
data <- mutate(data , quantity_MM = quantity / 1000000)
data <- mutate(data , waste_type = fct_reorder(waste_type, quantity_MM))
data <- mutate(data , perc = paste0(sprintf("%4.1f", quantity_MM / sum(quantity_MM) * 100), "%"))

graph <- ggplot(data, aes(quantity_MM, waste_type))

graph <- graph + 
  geom_col(aes(quantity_MM, waste_type), fill = "#076fa2", width = 0.6) +
  scale_x_continuous(
    limits = c(0, 75.5),
    breaks = seq(0, 80, by = 10), 
    expand = c(0, 0), # The horizontal axis does not extend to either side
    position = "top"  # Labels are located on the top
  ) +
  scale_y_discrete(expand = expansion(add = c(0, 0.5))) +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid.major.x = element_line(color = "#A8BAC4", size = 0.3),
    axis.ticks.length = unit(0, "mm"),
    #axis.title = element_blank(),
    axis.line.y.left = element_line(color = "black"),
    axis.text.y = element_blank(),
    axis.text.x = element_text(family = "Econ Sans Cnd", size = 16),
    plot.title = element_text(
      family = "Econ Sans Cnd", 
      face = "bold",
      size = 22
    ),
    plot.subtitle = element_text(
      family = "Econ Sans Cnd",
      size = 20
    ),
    axis.title.x = element_text(size = 15, face = "bold"),
    axis.title.y = element_text(size = 15, face = "italic")
  ) +
  geom_text(aes(label = perc), hjust = 0, size = 7 , colour = "#076fa2" , family = "Econ Sans Cnd") +
  geom_text(
    aes(0, y = waste_type, label = waste_type),
    hjust = 0,
    nudge_x = 0.3,
    colour = "white",
    family = "Econ Sans Cnd",
    size = 7
  ) +
  labs(
    title = "Source of Food Waste in EU",
    subtitle = "Total in Tonnes of Waste Food (MM), 2022",
    x = "Quantity of Food Waste (MM of Tonnes)",
    y = "Source of Food Waste"
  )
