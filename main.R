  library(readxl)
  library(shiny)
  source("food_waste_per_capita.R")
  source("biomass_waste.R")
  source("source_food_waste.R")
  source("waffle_food_waste.R")
  # source("recycling.R")
  
  
  custom_theme <- theme(
    panel.background = element_rect(fill = "white"),
    panel.grid.major.x = element_line(color = "#A8BAC4", size = 0.3),
    axis.ticks.length = unit(0, "mm"),
    axis.line.y.left = element_line(color = "black"),
    axis.text.y = element_blank(),
    # Titles
    plot.title = element_text(
      family = "Nunito",
      size = 35,
      face = "bold",
      hjust = 0.5
    ),
    # Subtitles
    plot.subtitle = element_text(
      family = "Nunito",
      size = 25,
      hjust = 0.5
    ),
    # Legend title
    legend.title = element_text(family = "Nunito", size = 20),
    # Legend text
    legend.text = element_text(family = "Nunito", size = 18),
    # Axis text
    axis.text = element_text(family = "Nunito", size = 15),
    # Axis title
    axis.title = element_text(family = "Nunito", size = 15),
    legend.key.size = unit(1, "cm"),  # Agrandar los cuadros de la leyenda
    axis.text.x = element_text(size = 14, family = "Nunito", color = "black"),  # Configurar texto del eje X
    # axis.text.y = element_text(size = 14, family = "Nunito", color = "black")   # Configurar texto del eje Y
  )
  
  
  # Draw plots
  waste_data <- read_excel("data/df_foodwaste.xlsx")
  disposal_data <- read_excel("data/df_waste_destination.xlsx")
  # disposal_data_2 <- read_excel("data/df_waste_destination_2.xlsx")
  
  plot1 <- food_waste_per_capita(waste_data, custom_theme)
  plot1
  plot3 <- treemap_by_country(disposal_data, custom_theme)
  plot3
  plot2 <- total_food_waste(waste_data, custom_theme)
  plot2
  plot4 <- treemap_by_type(disposal_data, custom_theme)
  plot4
  plot5 <- waffle_food_waste(waste_data, custom_theme)
  plot5
  # plot6 <- recycling(disposal_data_2, custom_theme)
  # plot6
  
  # Interfaz de usuario
library(shiny)

# Define la interfaz de usuario
ui <- navbarPage(
  title = "Menu",
  # Agrega la fuente Nunito desde Google Fonts
  header = tags$head(
    tags$h1("Food waste and allocation analysis - European Union - 2022", align = "center"),
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Nunito:wght@300;400;700&display=swap"),
    tags$style(HTML("
      body {
        font-family: 'Nunito', sans-serif;
      }
      h1, h2, h3, h4, h5, h6 {
        font-weight: 700;
      }
      .navbar-brand {
        font-size: 24px;
        font-weight: 700;
      }
    "))
  ),
  # Página 1
  tabPanel("Waste Generation", 
           fluidPage(
             h4("General rankings on food waste generation"),
             fluidRow(
               column(6, plotOutput("plot1a", height = "800px", width = "1000px")),  # Primer gráfico
               column(6, plotOutput("plot1b", height = "800px", width = "1000px"))   # Segundo gráfico
             )
           )),
  
  # Página 2
  tabPanel("Biomass Allocation", 
           fluidPage(
             h4("Biomass Allocation in the UE"),
             fluidRow(
               column(6, plotOutput("plot2a", height = "800px", width = "1000px")),  # Primer gráfico
               column(6, plotOutput("plot2b", height = "800px", width = "1000px"))   # Segundo gráfico
             )
           )),
  
  # Página 3
  tabPanel("Waste Disposal", 
           fluidPage(
             h4("Biomass Disposal in the UE"),
             fluidRow(
               column(6, plotOutput("plot3a", height = "800px", width = "1000px")),  # Primer gráfico
               column(6, plotOutput("plot3b", height = "800px", width = "1000px"))   # Segundo gráfico
             )
           ))
  
)

# Define el servidor
server <- function(input, output) {
  # Gráficos de la Página 1
  output$plot1a <- renderPlot({
    plot1  # Primer gráfico
  })
  
  output$plot1b <- renderPlot({
    plot2  # Segundo gráfico
  })
  
  # Gráficos de la Página 2
  output$plot2a <- renderPlot({
    plot3  # Primer gráfico
  })
  
  output$plot2b <- renderPlot({
    plot4  # Segundo gráfico
  })
  
  # Gráficos de la Página 3
  output$plot3a <- renderPlot({
    plot5  # Primer gráfico
  })

}


# Ejecuta la aplicación
shinyApp(ui = ui, server = server)
  
  


