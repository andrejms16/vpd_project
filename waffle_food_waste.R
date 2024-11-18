waffle_food_waste <- function(data, custom_theme) {
  library(ggplot2)
  library(sf)
  library(dplyr)
  library(readxl)
  library(tidyverse)
  library(rnaturalearth)
  library(rnaturalearthdata)
  library(data.table)
  library(plotly)
  library(lubridate)
  library(dplyr)
  library(grid)
  library(shadowtext)
  
  library(waffle) # remotes::install_github("hrbrmstr/waffle")
  library(rmarkdown)
  
  library(ggtext)
  
  library(magick)
  
  library(rsvg)
  
  library(ggtextures) #devtools::install_github("clauswilke/ggtextures")
  
  
  library(paletteer)
  
  # Load the SteppedSequential5Steps palette
  color_palette <- c("#B54057", "#93B7BE", "#048A81", "#E0CA3C", "#A799B7")
  color_palette
  
  #### Select Data Set
  # df_fw <- read_excel("data/df_foodwaste.xlsx", sheet = 1)
  # data = df_fw
  data <- filter(data, metric == "tonne" & year == "2022")
  data <- data[rowSums(is.na(data)) < 2 , ]
  
  #### First PLOT % FOOD WASTE in EU comes FROM Y
  # Select Data
  data <- data %>%
    dplyr::select(primary_production,
                  manufacture,
                  retail,
                  restaurants,
                  households)
  
  data <- data %>%
    pivot_longer(
      cols = c(
        "primary_production",
        "manufacture",
        "retail",
        "restaurants",
        "households"
      ),
      names_to = "waste_type",
      values_to = "quantity"
    )
  
  # Calculate total waste
  data <- aggregate(quantity ~ waste_type, data, sum)
  data <- mutate(data , quantity_MM = round(quantity / 1000000 , digits = 2))
  data <- mutate(data , waste_type = fct_reorder(waste_type, quantity_MM))
  data <- mutate(data , perc = paste0(sprintf(
    "%4.1f", quantity_MM / sum(quantity_MM) * 100
  ), "%"))
  data <- mutate(data , percentage = quantity_MM / sum(quantity_MM))
  
  
  # Waffle plot:
  # my_font <- "Roboto"
  
  
  
  
  # Define a shared color palette for the categories
  category_colors <- c(
    "Households" = color_palette[1],
    "Manufacture" = color_palette[2],
    "Primary Production" = color_palette[3],
    "Restaurants" = color_palette[4],
    "Retail" = color_palette[5]
  )
  data
  # Update dataset with label positions
  data <- data %>%
    mutate(
      label_x = c(4, 20, 25, 27, 29),
      # X positions for the labels
      label_y = rep(8, n()),
      # Place all labels in the 8th row (above the graph)
      label_color = category_colors[waste_type],
      label_text = paste(round(quantity_MM, 1), "MM"),
      waste_type = str_to_title(gsub("_", " ", waste_type))
    ) # Assign colors from the palette
  
  
  
  font_size <- 6
  
  
  wa_plot <-
    ggplot(data, aes(fill = waste_type, values = quantity_MM)) +
    geom_waffle(
      n_rows = 8,
      size = 1,
      colour = "white",
      flip = FALSE,
      make_proportional = TRUE,
      show.legend = FALSE,
      height = 1,
      width = 1
    ) +  # Ensure square tiles
    scale_fill_manual(values = category_colors) +  # Use shared color palette
    scale_y_continuous(breaks = NULL) +  # Remove gridlines
    scale_x_continuous(breaks = NULL) +  # Remove gridlines
    labs(
      title = "54% of food waste in EU comes from Households",
      subtitle = "Total in Tonnes of Waste Food (MM), 2022."
    ) +
    custom_theme +  
    theme(
      axis.title.x = element_blank(),  # Elimina el título del eje X
      axis.title.y = element_blank()   # Elimina el título del eje Y
    ) +
    geom_text(
      data = data,
      aes(
        x = 1,
        y = 0.15,
        label = label_text[1],
        colour = waste_type[1]
      ),
      # Use positions and category colors
      size = font_size,
      inherit.aes = TRUE
    ) +
    geom_text(
      data = data,
      aes(
        x = 1,
        y = -0.2,
        label = perc[1],
        colour = waste_type[1]
      ),
      # Use positions and category colors
      size = font_size,
      inherit.aes = TRUE
    ) +
    geom_text(
      data = data,
      aes(
        x = 8,
        y = 0.15,
        label = label_text[2],
        colour = waste_type[2]
      ),
      # Use positions and category colors
      size = font_size,
      inherit.aes = TRUE
    ) +
    geom_text(
      data = data,
      aes(
        x = 8,
        y = -0.2,
        label = perc[2],
        colour = waste_type[2]
      ),
      # Use positions and category colors
      size = font_size,
      inherit.aes = TRUE
    ) +
    geom_text(
      data = data,
      aes(
        x = 11,
        y = 0.15,
        label = label_text[3],
        colour = waste_type[3]
      ),
      # Use positions and category colors
      size = font_size,
      inherit.aes = TRUE
    ) +
    geom_text(
      data = data,
      aes(
        x = 11,
        y = -0.2,
        label = perc[3],
        colour = waste_type[3]
      ),
      # Use positions and category colors
      size = font_size,
      inherit.aes = TRUE
    ) +
    geom_text(
      data = data,
      aes(
        x = 12,
        y = 0.15,
        label = label_text[4],
        colour = waste_type[4]
      ),
      # Use positions and category colors
      size = font_size,
      inherit.aes = TRUE
    ) +
    geom_text(
      data = data,
      aes(
        x = 12,
        y = -0.2,
        label = perc[4],
        colour = waste_type[4]
      ),
      # Use positions and category colors
      size = font_size,
      inherit.aes = TRUE
    ) +
    geom_text(
      data = data,
      aes(
        x = 13,
        y = 0.15,
        label = label_text[5],
        colour = waste_type[5]
      ),
      # Use positions and category colors
      size = font_size,
      inherit.aes = TRUE
    )  +
    geom_text(
      data = data,
      aes(
        x = 13,
        y = -0.2,
        label = perc[5],
        colour = waste_type[5]
      ),
      # Use positions and category colors
      size = font_size,
      inherit.aes = TRUE
    )  +
    scale_colour_manual(values = category_colors) +  # Use shared color palette for text
    coord_fixed(ratio = 1)  # Ensure squares instead of rectangles
  
  return(wa_plot)
}
