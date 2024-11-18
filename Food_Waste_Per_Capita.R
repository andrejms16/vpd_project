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
library(gghighlight)
library(ggflags)
library(countrycode)
#remotes::install_github('rensa/ggflags')

x <- c("canada", "antarctica")
countrycode("canada", origin = 'country.name', destination = 'iso3c')

#### Select Data Set
data = read_excel('./datasets/df_foodwaste.xlsx')
data <- filter(data,metric=="kg_per_capita" & year=="2022")
data <- data[ rowSums( is.na( data ) ) < 2 , ]

#### Second PLOT Ranking of Food Waste Per Capita
# Select Data 
data <- data %>%
  dplyr::select( country , households ) %>% arrange(desc(households))

# Calculate total waste
data <- mutate(data, country = fct_reorder(country, households))

ranking <- filter(data, row_number() <= 5 | row_number() >= n() - 5 | country == "European Union" )
ranking <- mutate(ranking, waste_ranking = as.factor( ifelse(row_number() <= 5, "top_5", ifelse(country == "European Union", "European_Union", ifelse(row_number() >= n() - 5, "last_5","")))))
ranking <- mutate(ranking, countrycode = tolower(ifelse(country == "European Union","EU", countrycode(country, origin = 'country.name', destination = 'iso2c'))))

graph <- ggplot(ranking, aes(households, country))

graph <- graph + 
  geom_col(aes(x = households, y = country, fill = waste_ranking), width = 0.6) +
  geom_flag(x = 18, aes(country = countrycode), size = 10) +
  scale_fill_manual(name="Ranking of Food Waste per Country", 
                    labels = c("Top 5 Countries", "European Union", "Last 5 Counties"), 
                    values = c("top_5"="#C41E3A", "European_Union"="gray", "last_5"="#076fa2")) + 
  scale_x_continuous(
    limits = c(0, 130),
    breaks = seq(0, 140, by = 10), 
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
  geom_text(aes(label = households), hjust = 0, size = 7  , family = "Econ Sans Cnd") +
  geom_text(
    aes(0, y = country, label = country),
    hjust = 0,
    nudge_x = 0.3,
    colour = "white",
    family = "Econ Sans Cnd",
    face = "bold",
    size = 5
  ) +
  labs(
    title = "Food Waste Per Capita in EU",
    subtitle = "Total in KG of Waste Food per Capita, 2022",
    x = "Quantity of Food Waste per Capita (KG)",
    y = "Country",
    caption = "Source: Eurostat"
  )

graph
