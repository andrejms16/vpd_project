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
install.packages("waffle")
library(waffle) # remotes::install_github("hrbrmstr/waffle")
library(rmarkdown)
install.packages("ggtext")
library(ggtext)
install.packages("waffle")
library(magick)
install.packages("magick")
library(rsvg)
install.packages("ggtextures")
library(ggtextures) #devtools::install_github("clauswilke/ggtextures")

install.packages("paletteer")
library(paletteer)

# Load the SteppedSequential5Steps palette
color_palette <- as.character(paletteer_d("colorBlindness::SteppedSequential5Steps"))
color_palette

#### Select Data Set
df_fw <- read_excel("C:/Users/helen/data_science/vpd_r/project/df_foodwaste.xlsx", sheet = 1)
data = df_fw
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
data <- mutate(data , quantity_MM = round(quantity / 1000000 , digits = 2))
data <- mutate(data , waste_type = fct_reorder(waste_type, quantity_MM))
data <- mutate(data , perc = paste0(sprintf("%4.1f", quantity_MM / sum(quantity_MM) * 100), "%"))
data <- mutate(data , percentage = quantity_MM / sum(quantity_MM))


# Waffle plot: 
my_font <- "Roboto"




# Define a shared color palette for the categories
category_colors <- c(
  "households" = color_palette[8], 
  "manufacture" = color_palette[18], 
  "primary_production" = color_palette[13], 
  "restaurants" = color_palette[23], 
  "retail" = color_palette[3]
)
data
# Update dataset with label positions
data <- data %>%
  mutate(label_x = c(4, 9, 11, 12.5, 14),  # X positions for the labels
         label_y = rep(8, n()),      # Place all labels in the 8th row (above the graph)
         label_color = category_colors[waste_type],
         label_text = paste(round(quantity_MM, 1), "MM")) # Assign colors from the palette

data
color_palette

wa_plot <- 
  ggplot(data, aes(fill = waste_type, values = quantity_MM)) +
  geom_waffle(n_rows = 7, size = 1, colour = "white", flip = FALSE, make_proportional = TRUE,
              show.legend = FALSE, height = 1, width = 1) +  # Ensure square tiles
  scale_fill_manual(values = category_colors) +  # Use shared color palette
  scale_y_continuous(breaks = NULL) +  # Remove gridlines
  scale_x_continuous(breaks = NULL) +  # Remove gridlines
  labs(title = "**54% of food waste in EU comes from Households**", 
       subtitle = "Total in Tonnes of Waste Food (MM), 2022:<br>
                  <span style='color:#A3CC51FF;'>**Production (agriculture)**</span>,
                  <span style='color:#CC5151FF;'>**Retail and commerce**</span>,
                  <span style='color:#6551CCFF;'>**Restaurants**</span>,
                  <span style='color:#51A3CCFF;'>**Industries**</span> and 
                  <span style='color:#CC8E51FF;'>**Households**</span>.") +
  theme(text = element_text(family = my_font), 
        plot.title = element_markdown(color = "#292929"), 
        plot.subtitle = element_markdown(color = "#292929", margin = margin(b = 5)),
        axis.text = element_blank(),  # Remove axis labels
        axis.ticks = element_blank(),  # Remove axis ticks
        plot.background = element_rect("#EBEBEB"),
        panel.background = element_blank(),
        axis.title = element_blank(),
        legend.position = "none") +  # Remove axis titles
  geom_text(data = data, 
            aes(x = 1, y = 0.15, label = label_text[1], colour = waste_type[1]),  # Use positions and category colors
            size = 3.5, inherit.aes = TRUE) +
  geom_text(data = data, 
            aes(x = 1, y = -0.2, label = perc[1], colour = waste_type[1]),  # Use positions and category colors
            size = 3.5, inherit.aes = TRUE) +
  geom_text(data = data, 
            aes(x = 3, y = 0.15, label = label_text[2], colour = waste_type[2]),  # Use positions and category colors
            size = 3.5, inherit.aes = TRUE) +
  geom_text(data = data, 
            aes(x = 3, y = -0.2, label = perc[2], colour = waste_type[2]),  # Use positions and category colors
            size = 3.5, inherit.aes = TRUE) +
  geom_text(data = data, 
            aes(x = 4, y = 0.15, label = label_text[3], colour = waste_type[3]),  # Use positions and category colors
            size = 3.5, inherit.aes = TRUE) +
  geom_text(data = data, 
            aes(x = 4, y = -0.2, label = perc[3], colour = waste_type[3]),  # Use positions and category colors
            size = 3.5, inherit.aes = TRUE) +
  geom_text(data = data, 
            aes(x = 6, y = 0.15, label = label_text[4], colour = waste_type[4]),  # Use positions and category colors
            size = 3.5, inherit.aes = TRUE) +
  geom_text(data = data, 
            aes(x = 6, y = -0.2, label = perc[4], colour = waste_type[4]),  # Use positions and category colors
            size = 3.5, inherit.aes = TRUE) +
  geom_text(data = data, 
            aes(x = 12, y = 0.15, label = label_text[5], colour = waste_type[5]),  # Use positions and category colors
            size = 3.5, inherit.aes = TRUE)  +
  geom_text(data = data, 
            aes(x = 12, y = -0.2, label = perc[5], colour = waste_type[5]),  # Use positions and category colors
            size = 3.5, inherit.aes = TRUE)  +
  scale_colour_manual(values = category_colors) +  # Use shared color palette for text
  coord_fixed(ratio = 1)  # Ensure squares instead of rectangles

wa_plot
