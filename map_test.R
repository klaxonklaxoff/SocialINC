
source("1_example_data.R")

ui <-
  fluidPage(
    titlePanel("Social Inclusion Data Visualization Tool"),
    tabsetPanel(
      type = "pills",
      tabPanel(
        "Groups designated as Visible Minorities",
        fluid = TRUE,
        mainPanel(plotlyOutput("map", )
        )
      )
    )
  )

server <- function(input, output, session) {
  
  #'NOTE [created this smaller dataset to test map feasibility]
  test <- 
    educationDT %>% 
    filter(Year == "2016",
           Sex == "Females",
           Age == "15 to 64 years",
           Language == "English only",
           Immigration == "Immigrants",
           VisMin == "Black",
           Indicator == "Population with no certificate, diploma or degree")
  test <- test[c(1:8), ]
  
  output$map <- 
    renderPlotly(ggplotly({
      ggplot(test) +
        geom_sf(aes(fill = Value, geometry = geometry)) +
        ggtitle("Labour Force Status by Groups Designated as Visible Minorities")
    }))

}

# All together now ----
shinyApp(ui, server)