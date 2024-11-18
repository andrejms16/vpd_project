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
library(waffle) # remotes::install_github("hrbrmstr/waffle")
library(rmarkdown)
library(ggtext)
library(magick)
library(rsvg)
library(ggtextures) #devtools::install_github("clauswilke/ggtextures")

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
    axis.title.y = element_text(size = 15, face = "italic")
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
    y = "Source of Food Waste"
  )

graph


library(echarts4r)
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

wa_plot <- 
  ggplot(data , aes(fill = waste_type, values = quantity_MM)) +
  geom_waffle(n_rows = 9, size = 0.33, colour = "white", flip = FALSE, show.legend = FALSE) +
  scale_fill_manual(limits = c("households", "manufacture", "primary_production", "restaurants", "retail"),
                    values = c("#FD8235", "#82869B", "#F7B32B", "#365181", "#F72C25")) +
  scale_y_continuous(breaks = seq(0, 9, by = 3)) +
  scale_x_continuous(breaks = seq(0, 10, by = 2)) +
  labs(title = "**Source of Food Waste in EU**", 
       subtitle = "**Total in Tonnes of Waste Food (MM), 2022:<br>
                  <span style='color:#FD8235;'>households</span>,
                  <span style='color:#82869B;'>manufacture</span>,
                  <span style='color:#F7B32B;'>primary_production</span>,
                  <span style='color:#365181;'>restaurants</span> and
                  <span style='color:#F72C25;'>retail</span>.**") +
  theme(text = element_text(family = my_font), 
        plot.title = element_markdown(color = "#292929"), 
        plot.subtitle = element_markdown(color = "#292929"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.background = element_rect("#EBEBEB"),
        panel.background = element_blank(),
        axis.title = element_text(color = "#292929", face = "bold"),
        axis.text = element_text(color = "#292929", face = "bold")) 
wa_plot
