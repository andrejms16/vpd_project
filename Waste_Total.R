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

#### Select Data Set
data = read_excel('./datasets/df_foodwaste.xlsx')
data <- filter(data,metric=="tonne" & year=="2022" )



#### First PLOT % FOOD WASTE in EU comes FROM Y
# Select Data 
data <- data %>%
        dplyr::select( primary_production, manufacture, retail, restaurants, households )

data <- data[ rowSums( is.na( data ) ) < 2 , ]

#Sum Data
total <- colSums (data, na.rm = TRUE, dims = 1)

#Building The Plot
graph <- barplot(total, col=rgb(0.2,0.4,0.6,0.6) )
graph <- graph + labs ( y = "Quantity of food wasting in 2022" , x = "Sources of food wasting" )