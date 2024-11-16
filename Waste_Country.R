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

data = read_excel('./datasets/ProjectDataset.xlsx')

world <- ne_countries(scale = "medium", returnclass = "sf")
Europe <- world[which(world$continent == "Europe"),]

map_data <- Europe %>%
  left_join(data, by = c("name" = "Country"))

happy_plot <- ggplot(map_data) +
  geom_sf(aes(fill = as.numeric(Total_waste), text = paste(name,"-", Total_waste))) +
  geom_sf_text(aes(label = Total_waste), colour = "white", size=3) +
  coord_sf(xlim = c(-25,50), ylim = c(35,70), expand = FALSE)+
  scale_fill_gradient2(low = "grey", high = "#006400")+
  labs(title = "Total Food Waste in European Union (Kilograms per Capita - 2020)",
       fill = "Total Waste") +
  theme_minimal()

happy_plot1 <- ggplotly(happy_plot, tooltip = "text") %>% 
  plotly::style(hoverlabel = list(bgcolor = "white"))
