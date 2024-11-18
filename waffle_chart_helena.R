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
    axis.title.y = element_text(size = 15, face = "italic"),
    plot.caption =  element_text(
      family = "Econ Sans Cnd",
      size = 16
    )
  ) +
  geom_text(aes(label = perc), hjust = 0, size = 7 , colour = "#076fa2" , family = "Econ Sans Cnd") +
  geom_text(
    aes(0, y = waste_type, label = waste_type),
    hjust = 0,
    nudge_x = 0.3,
    colour = "white",
    family = "Econ Sans Cnd",
    size = 5
  ) +
  labs(
    title = "Source of Food Waste in EU",
    subtitle = "Total in Tonnes of Waste Food (MM), 2022",
    x = "Quantity of Food Waste (MM of Tonnes)",
    y = "Source of Food Waste",
    caption = "Source: Eurostat"
  )

graph

install.packages("echarts4r")
library(echarts4r)
remotes::install_github("JohnCoene/echarts4r.assets")
library(echarts4r.assets) #remotes::install_github("JohnCoene/echarts4r.assets")

style <- list(
  normal = list(opacity = 1),
  # normal
  emphasis = list(opacity = 1) # on hover
)

#E_Charts

data %>% 
  e_charts(waste_type) %>% 
  e_pictorial(quantity_MM, symbol = ea_icons("trash"), 
              symbolRepeat = TRUE, z = 0,
              itemStyle = style,
              symbolSize = c(20, 30)) %>% 
  e_theme("westeros") %>%
  e_title("Source of Food Waste in EU - Total in Tonnes of Waste Food (MM), 2022") %>% 
  e_flip_coords() %>%
  # Hide Legend
  e_legend(show = FALSE) %>%
  # Remove Gridlines
  e_x_axis(splitLine=list(show = FALSE)) %>%
  e_y_axis(splitLine=list(show = FALSE)) %>%
  # Format Label
  e_labels(fontSize = 26, fontWeight ='bold', position = "right", offset=c(10, 0))


# Waffle plot: 
my_font <- "Roboto Condensed"


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
         label_text = paste0(round(quantity_MM, 1), "MM\n(", round(percentage, 0), "%)"))  # Assign colors from the palette

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
                  <span style='color:#CC8E51FF;'>**Households**</span>,
                  <span style='color:#51A3CCFF;'>**Industries**</span>,
                  <span style='color:#A3CC51FF;'>**Production (agriculture)**</span>,
                  <span style='color:#6551CCFF;'>**Restaurants**</span> and
                  <span style='color:#CC5151FF;'>**Retail and commerce**</span>.") +
  theme(text = element_text(family = my_font), 
        plot.title = element_markdown(color = "#292929"), 
        plot.subtitle = element_markdown(color = "#292929", margin = margin(b = 20)),
        axis.text = element_blank(),  # Remove axis labels
        axis.ticks = element_blank(),  # Remove axis ticks
        plot.background = element_rect("#EBEBEB"),
        panel.background = element_blank(),
        axis.title = element_blank(),
        legend.position = "none") +  # Remove axis titles
  geom_text(data = data, 
            aes(x = 4, y = 7.8, label = label_text[1], colour = waste_type[1]),  # Use positions and category colors
            size = 3.5, inherit.aes = FALSE) +
  geom_text(data = data, 
            aes(x = 9, y = 7.8, label = label_text[2], colour = waste_type[2]),  # Use positions and category colors
            size = 3.5, inherit.aes = FALSE) +
  geom_text(data = data, 
            aes(x = 11, y = 7.8, label = label_text[3], colour = waste_type[3]),  # Use positions and category colors
            size = 3.5, inherit.aes = FALSE) +
  geom_text(data = data, 
            aes(x = 12.5, y = 7.8, label = label_text[4], colour = waste_type[4]),  # Use positions and category colors
            size = 3.5, inherit.aes = FALSE) +
  geom_text(data = data, 
            aes(x = 14, y = 7.8, label = label_text[5], colour = waste_type[5]),  # Use positions and category colors
            size = 3.5, inherit.aes = FALSE)  +
  scale_colour_manual(values = category_colors) +  # Use shared color palette for text
  coord_fixed(ratio = 1)  # Ensure squares instead of rectangles

wa_plot
warnings()
