## app.R ##
library(shinydashboard)

require(tidyverse)
require(readr)

# Carregando base de dados
houses <- read_rds("DADOS/houses.rds")
houses_cat <- read_rds("DADOS/houses_cat.rds")

# Definindo variáveis para Slider Range da Área Construída
A_max <- max(houses$Area_Construida)
A_min <- min(houses$Area_Construida)
range1 <- ((A_max - A_min)/10) * 4
range2 <- ((A_max - A_min)/10) * 6

# Definindo variáveis para Slider Range do Preço
P_max <- max(houses$Preco)
P_min <- min(houses$Preco)
rangeP1 <- ((P_max - P_min)/10) * 4
rangeP2 <- ((P_max - P_min)/10) * 6

ui <- dashboardPage(
  dashboardHeader(title = "Projeto Analytics APP"),
  dashboardSidebar(),
  dashboardBody(
    # Dados do controle Slider Range para o plot 1
    fluidRow(
      box(plotOutput("plot1", height = 250)),
      
      box(
        title = "Range de Área",
        sliderInput("slider1", "Área do Imóvel entre:",
                    min=A_min,
                    max=A_max,
                    value=c(range1, range2),
                    step=(A_max - A_min)/10)
      )
    ),
    # Dados do controle Select Box para o plot 2
    fluidRow(
      box(plotOutput("plot2", height = 250)),
      
      box(
        title = "Controle2",
        selectInput("select", label="Avaliação do Imóvel:",
                    choices=list("1" = 1, "2" = 2, "3" = 3, "4" = 4, "5" = 5,
                                 "6" = 6, "7" = 7, "8" = 8, "9" = 9, "10" = 10,
                                 "11" = 11, "12" = 12, "13" = 13),
                    selected = 1)
      )
    ),
    # Dados do controle Slide Range para o plot 3
    fluidRow(
      box(plotOutput("plot3", height = 250)),
      
      box(
        title = "Range de Preço",
        sliderInput("slider2", "Preço do Imóvel entre:",
                    min=P_min,
                    max=P_max,
                    value=c(rangeP1, rangeP2),
                    step=(P_max - P_min)/100)
      )
    )
  )
)

server <- function(input, output) {

  # Boxplot para visualizar a Variação do Preço x Área Construída do Imóvel
  output$plot1 <- renderPlot({
    AC_min <- min(input$slider1)
    AC_max <- max(input$slider1)
    ggplot(subset(houses, Area_Construida >= AC_min & Area_Construida <= AC_max),
           mapping = aes(x=Area_Construida, y=Preco,
                         fill = (Area_Construida))) +
      geom_boxplot(show.legend=F) +
      labs(x="Área Construída", y="Preço - Dólar ($)",
           title= "Variação do Preço x Área Construída do Imóvel") +
      scale_y_continuous(limits=c(min(houses_cat$Preco),
                                  max(houses_cat$Preco))) +
      theme(text = element_text(size=9))
  })
  
  # Boxplot para visualizar a Variação do Preço x Avaliação do Imóvel
  output$plot2 <- renderPlot({
    ggplot(subset(houses_cat, Avaliacao == input$select),
           mapping = aes(x=Avaliacao, y=Preco,
                         fill = (Avaliacao))) +
      geom_boxplot(show.legend=F) +
      labs(x="Avaliação do Imóvel", y="Preço - Dólar ($)",
           title= "Variação do Preço x Avaliação do Imóvel") +
      scale_y_continuous(limits=c(min(houses_cat$Preco),
                                  max(houses_cat$Preco))) +
      theme(text = element_text(size=9))
  })
  
  # Mapa para visualizar a relação Preço x Localização dos Imóveis
  output$plot3 <- renderPlot({
    PC_min <- min(input$slider2)
    PC_max <- max(input$slider2)
    range_houses <- subset(houses, Preco >= PC_min & Preco <= PC_max)
    qmplot(Longitude, Latitude, data=range_houses, maptype = "toner-lite",
           color = I("red"), main="Localização dos Imóveis",
           xlab = "Longitude", ylab = "Latitude")
  })
}

shinyApp(ui, server)
