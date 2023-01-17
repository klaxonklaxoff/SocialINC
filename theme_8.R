# # source(file = "reference_sources.R", encoding = "UTF-8")
# # Install packages ----
# package_list <- c(
#   "arrow",
#   "scales",
#   "plotly",
#   "shiny",
#   "shinyWidgets"
# )
# new_packages <-
#   package_list[!(package_list %in% installed.packages()[, "Package"])]
# if (length(new_packages))
#   install.packages(new_packages)
# 
# rm(package_list, new_packages)

# Libraries -----
library(arrow)
library(scales)
library(plotly)
library(shiny)
library(shinyWidgets)
library(readxl)


# Reading the data file------
educationDT <- read_parquet(file = "./_final_data/educationDT.parquet")

# Colorblind friendly colors ----
pal <- c("#117F98","#3F875C","#D81159","#FBAE16","#ffb6db","920000",
         "#490092","#006ddb","#b66dff","#6db6ff","#b6dbff",
         "#920000","#924900","#db6d00","##ffb6db","#ffb6db")
# User interface // create layout ----
ui <-
  fluidPage(
   sidebarLayout(
          sidebarPanel(
            width = 3,
            uiOutput(NS(id=NULL,'indicator_control')),
            uiOutput(NS(id=NULL,'year_control')),
            uiOutput(NS(id=NULL,'gender_control')),
            uiOutput(NS(id=NULL,'vismin_control')),
            uiOutput(NS(id=NULL,'age_control')),
            uiOutput(NS(id=NULL,'language_control')),
            uiOutput(NS(id=NULL,'immigration_control')),
            uiOutput(NS(id=NULL,'geo_control')),
          ), #sidebarpanel
   
        # Main panel -----
         mainPanel(
           fillRow(
             plotlyOutput(NS(id =NULL,"plot_vm_education_1"),
                          height = "550px"),
                           
            
             plotlyOutput(NS(id =NULL,"plot_vm_education_2"),
                          height = "550px"),
                          
                             
             plotlyOutput(NS(id =NULL,"plot_vm_education_3"),
                          height = "550px"),
                          
                             
             plotlyOutput(NS(id =NULL,"plot_vm_education_4"),
                             height = "550px"),
                       
             plotlyOutput(NS(id =NULL,"plot_vm_education_5"),
                             height = "550px"),
                          
             plotlyOutput(NS(id =NULL,"plot_vm_education_6"),
                          height = "550px"),
                          
            # #### 8. Education and training skills ----
            # ##### 8.1. Population with no certificate, diploma or degree ----
            # conditionalPanel(
            #   condition = "input.indicator_1 == 'Population with no certificate, diploma or degree'",
            #   # h3(HTML("<b>tr(edu_indicator)</b>")),
            #   br(),
            #   br(),
            #   plotlyOutput(NS(id =NULL,"plot_vm_education_1",
            #                inline = TRUE)),
            #   br(),
            #   # h4(source_census_nhs_census),
            #   # h4(note)
            # ),
            # 
            # ##### 8.2. Population with high school diploma or equivalency certificate ----
            # conditionalPanel(
            #   condition = "input.indicator_1 == 'Population with high school diploma or equivalency certificate'",
            #   # h3(HTML("<b>tr(edu_indicator)</b>")),
            #   br(),
            #   br(),
            #   plotlyOutput("plot_vm_education_2",
            #                inline = TRUE),
            #   br(),
            #   # h4(source_census_nhs_census),
            #   # h4(note)
            # ),
            # ##### 8.3. Population with postsecondary certificate or diploma below bachelor level ----
            # conditionalPanel(
            #   condition = "input.indicator_1 == 'Population with postsecondary certificate or diploma below bachelor level'",
            #   # h3(HTML("<b>tr(edu_indicator)</b>")),
            #   br(),
            #   br(),
            #   plotlyOutput("plot_vm_education_3",
            #                inline = TRUE),
            #   br(),
            #   # h4(source_census_nhs_census),
            #   # h4(note)
            # ),
            # ##### 8.4. Population with university certificate or diploma above bachelor level ----
            # conditionalPanel(
            #   condition = "input.indicator_1 == 'Population with university certificate or diploma above bachelor level'",
            #   # h3(HTML("<b>tr(edu_indicator)</b>")),
            #   br(),
            #   br(),
            #   plotlyOutput("plot_vm_education_4",
            #                inline = TRUE),
            #   br(),
            #   # h4(source_census_nhs_census),
            #   # h4(note)
            # ),
            # ##### 8.5. Population with bachelor's degree ----
            # conditionalPanel(
            #   condition = "input.indicator_1 == 'Population with bachelor\\'s degree'",
            #   # h3(HTML("<b>tr(edu_indicator)</b>")),
            #   br(),
            #   br(),
            #   plotlyOutput("plot_vm_education_5",
            #                inline = TRUE),
            #   br(),
            #   # h4(source_census_nhs_census),
            #   # h4(note)
            # ),
            # ##### 8.6. Population with university certificate, diploma or degree above bachelor level ----
            # conditionalPanel(
            #   condition = "input.indicator_1 == 'Population with university certificate, diploma or degree above bachelor level'",
            #   # h3(HTML("<b>tr(edu_indicator)</b>")),
            #   br(),
            #   br(),
            #   plotlyOutput("plot_vm_education_6",
            #                inline = TRUE),
            #   br(),
            #   # h4(source_census_nhs_census),
            #   # h4(note)
            # ),
           
        ))
      
    ))
    
# Server // determines the data underlying the UI ----
education_server <- function(id =NULL,language) {
  
server <- function(input, output, session) {

  dictionary <- read_excel('dictionary.R/theme8_ready.xlsx') %>%
    split(.$key)
  
  # uses a reactiveVal language.
  tr <- function(key) {
    dictionary[[key]][[language()]]
  }
 
  # extract keys
  vismin <- reactive(tr("edu_vismin"))
  age <-reactive(tr("edu_age"))
 indicator <- reactive(tr("edu_indicator"))
  geo <- reactive(tr("edu_geo"))
  lang <- reactive(tr("edu_lang"))
  imm_gen <- reactive(tr("edu_imm_gen"))
  sex <- reactive(tr("edu_sex"))
  theme <- reactive(tr("edu_theme"))
  
  ## 1. Indicator ----
  output$indicator_control <- renderUI({
    req(input$Indicator)
    selectizeInput(
      inputId = NS(id=NULL,"indicator_1"),
      label = tr("edu_indicator"),
      choices = setNames(1:6, tr("edu_indicator"))
    )
  })
  #### 2.8. Education, training and skills ----
  #'NOTE [educationDT]
  conditionalPanel(
    condition =
      "input.indicator_1 == 'tr(edu_indicator)'",
   
    ##### Visible Minority ----
    #'NOTE [this is the focal variable for this tab]
    output$vismin_control <- renderUI({
      req(input$VisMin)
      pickerInput(
        inputId = NS(id=NULL,"education_vismin"), # name this for the server
        label = tr("choose_edu_vismin"), # label of filter
        choices = setNames(1:15, tr("edu_vismin")), # create drop-down list option
        multiple = TRUE, # multi-select
        selected = 1,
        options = list(
          `actions-box` = TRUE,
          `deselect-all-text` = tr("dsAll_lbl"),
          `select-all-text` = tr("sAll_lbl"),
        ))
    }),
    ##### Age group  ----
    output$age_control <- renderUI({
      req(input$Age)
      selectizeInput(
        inputId = NS(id=NULL,"education_age"),
        label = tr("choose_edu_age"),
        choices = setNames(1:9, tr("edu_age")),
        selected = 3
      )
    }),
    ##### Gender ----
    output$sex_control <- renderUI({
      req(input$Sex)
      selectizeInput(
        inputId = NS(id=NULL,"education_sex"),
        label = tr("choose_edu_sex"),
        choices = setNames(1:3, tr("edu_sex")),
        selected = 3
      )
    }),
    ##### Immigrant and generation status ----
    output$immigration_control <- renderUI({
      req(input$Immigration)
      selectizeInput(
        inputId = NS(id=NULL,"education_immigration"),
        label = tr("choose_edu_imm_gen"),
        choices = setNames(1:9, tr("edu_imm_gen")),
        selected = 3
      )
    }),
    #### Language ----
    output$language_control <- renderUI({
      req(input$Language)
      selectizeInput(
        inputId = NS(id=NULL,"education_language"),
        label = tr("choose_edu_lang"),
        choices = setNames(1:5, tr("edu_lang")),
        selected = 1
      )
    }),
    ##### Geography ----
    output$geo_control <- renderUI({
      req(input$Geography)
      selectizeInput(
        inputId = NS(id=NULL,"education_geography"),
        label = tr("choose_edu_geo"),
        choices = setNames(1:27, tr("edu_geo")),
        selected = 3
      )
    }),
        ###### Year ----
      output$year_control <- renderUI({
      pickerInput(
          inputId = NS(id=NULL,"education_year"), # name this for the server
          label = tr("choose_edu_year"), # label of filter
          choices = sort(unique(educationDT$Year), decreasing = TRUE), # create drop-down list option
          selected = sort(unique(educationDT$Year), decreasing = TRUE)[1],
          multiple = TRUE) # multi-select

    })
  )

  # Education training and skills ----
  func_plot_1 <- function(df, filter_var = NULL)
  {
    if (df == "educationDT") {
     
      filtered_data <-
        reactive({
          educationDT %>%
            filter(
              Indicator == input$filter_var,
              VisMin %in% input$education_vismin,
              Year %in% input$education_year,
              Language %in% input$education_language,
              Geography == input$education_geography,
              Immigration == input$education_immigration,
              Age == input$education_age,
              Sex == input$education_sex
            )
        })
    }
    renderPlotly(ggplotly({
      ggplot(filtered_data()) +
        geom_bar(
          stat = "identity",
          width = 0.6,
          position = position_dodge(width = 0.9),
          aes(
            x = VisMin,
            y = Value,
            colour = Year,
            fill = Year,
            text = paste0(
              tr(visible_lbl),
              VisMin,
              "<br>",
              tr(percent_lbl),
              format(Value, big.mark = ","),
              "<br>",
              tr(year_lbl),
              Year
            )
          )
        ) +
        theme_minimal() +
        theme(legend.position = "left",axis.text.x = element_text(angle = 45,vjust = 0.5,hjust = 1),axis.title.x=element_blank()) +
        scale_fill_manual(values = pal) +
        scale_color_manual(values = pal) +
        scale_y_continuous(labels = comma) +
        labs(
          y = tr(percent_lbl),
          colour = tr(year_lbl),
          fill = tr(year_lbl),
        )
    }, 
    tooltip = "text"
    ))
  }
  
  
  ### Plots ----
  
  #### 8. Education training and skills ----
  ##### 8.1. Population with no certificate, diploma or degree ----
  output$plot_vm_education_1 <-
    func_plot_1(df = "educationDT",
                filter_var = unique(as.character(educationDT$Indicator)[1]))
  ##### 8.2. Population with high school diploma or equivalency certificate ----
  output$plot_vm_education_2 <-
    func_plot_1(df = "educationDT",
                filter_var = unique(as.character(educationDT$Indicator)[2]))
  ##### 8.3.  Population with postsecondary certificate or diploma below bachelor level ----
  output$plot_vm_education_3 <-
    func_plot_1(df = "educationDT",
                filter_var = unique(as.character(educationDT$Indicator)[3]))
  ##### 8.4.  Population with university certificate or diploma above bachelor level ----
  output$plot_vm_education_4 <-
    func_plot_1(df = "educationDT",
                filter_var = unique(as.character(educationDT$Indicator)[4]))
  ##### 8.5.  Population with bachelor's degree----
  output$plot_vm_education_5 <-
    func_plot_1(df = "educationDT",
                filter_var = unique(as.character(educationDT$Indicator)[5]))
  ##### 8.6.  Population with university certificate or diploma or degree at bachelor level or above ----
  output$plot_vm_education_6 <-
    func_plot_1(df = "educationDT",
                filter_var = unique(as.character(educationDT$Indicator)[6]))
}
}

# All together now ----
#shinyApp(ui, server)

##### test -----
education_demo <- function() {
  ui <- fluidPage(

    )
  server <- function(input, output, session) {
    education_server("fr")
  }
  shinyApp(ui, server)
}
education_demo()
