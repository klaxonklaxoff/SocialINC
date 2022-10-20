source(file = "2_example_ui.R", encoding = "UTF-8")

# Server // determines the data underlying the UI ----
server <- function(input, output, session) {
  ## Themes and Definitions of Indicators ----
  ### Table 1 ----
  #'NOTE [Makes is so the table reacts to the user's selection]
  choose_def <-
    reactive({
      req(input$theme_0)
      template %>%
        filter(Theme %in% input$theme_0)
    })

  #'NOTE [Table is adjusted depending on the theme the user selects]
  output$def_table <-
    renderDataTable({
      choose_def() %>%
        select(-Theme) # Removed theme column because it's not needed in the table
    })

  ## Tab 1: Groups Designated as Visible Minorities ----
  ### Filters ----
  #'NOTE [This makes it so the theme filter affects the indicator filter]
  observe(
    updateSelectizeInput(
      session = session,
      inputId = "indicator_1",
      #'NOTE [Not showing the following indicators until they are ready // and "Hate crime" because there's no breakdown by vismin]
      choices = as.character(unique(template$Indicator[template$Theme == input$theme_1 &
                                                         !template$Indicator %in% c(

                                                           "Workers in specialized middle management occupations",

                                                           "Percent of the population living in a dwelling owned by one member of the household",
                                                           "Percent of the population living in core need household",
                                                           "Percent of the population living in suitable housing",
                                                           "Percent of the population living in an affordable housing",

                                                           "Knowledge of official languages, English only",
                                                           "Knowledge of official languages, French only",
                                                           "Knowledge of official languages, English and French",
                                                           "Knowledge of official languages, neither English nor French",
                                                           "Received a formal training paid by the employer in the past 12 months",
                                                           "Received an informal on-the-job training (from co-workers or supervisors) in the past 12 months",

                                                           "Average total household income, adjusted for the number of persons",
                                                           "Percent of the population living in poverty (low-income MBM)",
                                                           "Percent of the population living in low income situation (before-tax)",
                                                           "Percent of the population living in low income situation (after-tax)",
                                                           "Percent of the population reporting difficulty in meeting financial needs of their household",
                                                           "Percent of the population reporting ease in meeting financial needs of their household",

                                                           "Population living alone",
                                                           "Percent of the population with a personal close-ties network of 10 or more people",
                                                           "Feeling close to 5 relatives or more",
                                                           "Feeling close to 5 friends or more",
                                                           "Having no other friends or acquaintances",
                                                           "Having 1 to 19 other friends or acquaintances",
                                                           "Having 20 or more other friends or acquaintances",
                                                           "Having ethnically diverse networks of friends and acquaintances"
                                                         )]))
    )
  )

  #### Functions for Visible Minority----
  func_plot_1 <- function(df, filter_var = NULL)
  {
    if (df == "rateDT") {
      # Participation in the Labour Market (general) ----
      filtered_data <-
        reactive({
          rateDT %>%
            filter(
              Indicator == filter_var,
              VisMin %in% input$lm_vismin,
              Degree == input$lm_degree,
              Year %in% input$lm_year,
              Geography == input$lm_geography,
              Immigration == input$lm_immigration,
              Age == input$lm_age,
              Sex == input$lm_sex
            )
        })
    } else if (df == "representationDT_lm") {
      # Participation in the Labour Market (representationDT) ----
      filtered_data <-
        reactive({
          representationDT %>%
            filter(
              Indicator == filter_var,
              VisMin %in% input$lm_rep_vismin,
              Year  %in% input$lm_rep_year,
              Degree == input$lm_rep_degree,
              Geography == input$lm_rep_geography,
              Immigration == input$lm_rep_immigration,
              Age == input$lm_rep_age,
              Sex == input$lm_rep_sex
            )
        })
    } else if (df == "OverQualDT") {
      # Participation in the Labour Market (OverQualDT) ----
      filtered_data <-
        reactive({
          OverQualDT %>%
            filter(
              VisMin %in% input$lm_over_vismin,
              Year %in% input$lm_over_year,
              Location == input$lm_over_location,
              Degree == input$lm_over_degree,
              Geography == input$lm_over_geography,
              Immigration == input$lm_over_immigration,
              Age == input$lm_over_age,
              Sex == input$lm_over_sex,
              Language == input$lm_over_language
            )
        })
    } else if (df == "youthDT") {
      # Participation in the Labour Market (youthDT) ----
      filtered_data <-
        reactive({
          youthDT %>%
            filter(
              VisMin %in% input$lm_youth_vismin,
              Year  %in% input$lm_youth_year,
              Geography == input$lm_youth_geography,
              Immigration == input$lm_youth_immigration,
              Age == input$lm_youth_age,
              Sex == input$lm_youth_sex,
              Language == input$lm_youth_language
            )
        })
    } else if (df == "civicDT") {
      # Civic engagement and political participation (civicDT) ----
      filtered_data <-
        reactive({
          civicDT %>%
            filter(
              Indicator == filter_var,
              VisMin %in% input$civic_vismin,
              Geography == input$civic_geography,
              Confidence == input$civic_conf_interval,
              char_type == input$civic_sociodem,
              (Characteristic == input$civic_age |
                 Characteristic == input$civic_sex |
                 Characteristic == input$civic_immigration |
                 Characteristic == input$civic_generation |
                 Characteristic == input$civic_language |
                 Characteristic == input$civic_education
              )
            )
        })
    } else if (df == "civicDT2") {
      # Civic engagement and political participation (civicDT2) ----
      filtered_data <-
        reactive({
          civicDT2 %>%
            filter(
              Indicator == filter_var,
              VisMin %in% input$civic2_vismin,
              Geography == input$civic2_geography,
              Confidence == input$civic2_conf_interval,
              char_type == input$civic2_sociodem,
              (Characteristic == input$civic2_age |
                 Characteristic == input$civic2_sex |
                 Characteristic == input$civic2_immigration |
                 Characteristic == input$civic2_generation |
                 Characteristic == input$civic2_language |
                 Characteristic == input$civic2_education
              )
            )
        })
    } else if (df == "representationDT") {
      # Representation in decision-making positions ----
      filtered_data <-
        reactive({
          representationDT %>%
            filter(
              Indicator == filter_var,
              VisMin %in% input$rep_vismin,
              Year %in% input$rep_year,
              Degree == input$rep_degree,
              Geography == input$rep_geography,
              Immigration == input$rep_immigration,
              Sex == input$rep_sex,
              Age == input$rep_age
            )
        })
    } else if (df == "educationDT") {
      # Education training and skills ----
      filtered_data <-
        reactive({
          educationDT %>%
            filter(
              Indicator == filter_var,
              VisMin %in% input$education_vismin,
              Year %in% input$education_year,
              Language == input$education_language,
              Geography == input$education_geography,
              Immigration == input$education_immigration,
              Age == input$education_age,
              Sex == input$education_sex
            )
        })
    }else if (df == "belongingDT") {
      # Social connections and personal networks ----
      filtered_data <-
        reactive({
          belongingDT %>%
            filter(
              Indicator == filter_var,
              VisMin %in% input$belonging_vismin,
              Geography == input$belonging_geography,
              Confidence == input$belonging_conf_interval,
              char_type == input$belonging_sociodem,
              (Characteristic == input$belonging_age |
                 Characteristic == input$public_income_social_gender |
                 Characteristic == input$belonging_immigration |
                 Characteristic == input$belonging_generation |
                 Characteristic == input$belonging_language |
                 Characteristic == input$belonging_education
              )
            )
        })
    }
    else if (df == "basicDT") {
      # Basic needs and housing ----
      filtered_data <-
        reactive({
          basicDT %>%
            filter(
              Indicator == filter_var,
              VisMin %in% input$basic_vismin,
              Year  %in% input$basic_year,
              Geography == input$basic_geography,
              Confidence == input$basic_conf_interval,
              char_type == input$basic_sociodem,
              (Characteristic == input$basic_age |
                 Characteristic == input$basic_sex |
                 Characteristic == input$basic_immigration
              )
            )
        })
    } else if (df == "basicDT_health") {
      # Health and well being ----
      filtered_data <-
        reactive({
          basicDT %>%
            filter(
              Indicator == filter_var,
              VisMin %in% input$health_vismin,
              Year  %in% input$health_year,
              Geography == input$health_geography,
              Confidence == input$health_conf_interval,
              char_type == input$health_sociodem,
              (Characteristic == input$health_age |
                 Characteristic == input$health_sex |
                 Characteristic == input$health_immigration
              )
            )
        })
    }
    else if (df == "incomeDT") {
      # Income and wealth (incomeDT) ----
      filtered_data <-
        reactive({
          incomeDT %>%
            filter(
              Indicator == filter_var,
              VisMin %in% input$lm_income_vismin,
              Year  %in% input$lm_income_year,
              Degree == input$lm_income_degree,
              Geography == input$lm_income_geography,
              Immigration == input$lm_income_immigration,
              Age == input$lm_income_age,
              Sex == input$lm_income_sex
            )
        })
    }
    else if (df == "confidenceDT") {
    # Public services and institutions -----
      filtered_data <-
        reactive({
          confidenceDT %>%
            filter(
              Indicator == filter_var,
              VisMin %in% input$public_vismin,
              Geography == input$public_geography,
              Confidence == input$public_conf_interval,
              char_type == input$public_sociodem,
              (Characteristic == input$public_age |
                 Characteristic == input$public_sex |
                 Characteristic == input$public_immigration |
                 Characteristic == input$public_generation |
                 Characteristic == input$public_language |
                 Characteristic == input$public_education
              )
            )
        })
    }

    renderPlotly(ggplotly({
      ggplot(filtered_data(),
      aes(
        x = VisMin,
        y = Value,
        #colour = VisMin,
        fill = Year,
        label = Value
        )) +
        geom_col(
          width = 0.4, 
          position = position_dodge(width = 0.6)
          # aes(
          #    x = VisMin,
          #    y = Value,
          #    colour = VisMin,
          #    fill = VisMin,
          #    text = paste0(
          #      "Year: ",
          #      Year
          #    )
          #  )
          )+ 
      theme(legend.position= "right",
              axis.text.x = element_text(angle = 45,vjust = 0.5,hjust = 1)) +                                                       
        labs(
          x = "Visible Minority group(s)",
          y = "Percent",
          colour = "Visible Minority group(s)",
          fill = "Year"
        ) +
        scale_y_continuous(labels = comma) +
       geom_text(
              aes(
                  label = round(Value,1)),
                  color = "black",
                  # vjust = -1.5,
                  # hjust = -0.5,
                  size  = 2,
                  position = position_dodge(width = 0.9)
              )

      }, 
    tooltip = "text"))
  }

  #'NOTE [This chart doesn't follow the same x-axis as the other charts]
  func_plot_discrimination <- function(filter_var){
    # Discrimination and victimization ----
    filtered_data <-
      reactive({
        discriminationDT %>%
          filter(
            ind == filter_var,
            VisMin %in% input$discrimination_vismin,
            Geography == input$discrimination_geography,
            Confidence == input$discrimination_conf_interval,
            char_type == input$discrimination_sociodem,
            (Characteristic == input$discrimination_age |
               Characteristic == input$discrimination_sex |
               Characteristic == input$discrimination_immigration |
               Characteristic == input$discrimination_generation |
               Characteristic == input$discrimination_language |
               Characteristic == input$discrimination_education
            )
          )
      })

    renderPlotly(ggplotly({
      ggplot(filtered_data()) +
        geom_bar(
          stat = "identity",
          width = 0.4,
          position = position_dodge(width = 0.5),
          aes(
            x = before_since,
            y = Value,
            colour = VisMin,
            fill = VisMin,
            text = paste0(
              "Visible Minority group(s): ",
              VisMin,
              "<br>",
              "Value: ",
              format(Value, big.mark = ","),
              "<br>",
              "Reference Period in Relation to the covid-19 Pandemic: ",
              before_since
            )
          )
        ) +
        theme_minimal() +
        scale_y_continuous(labels = comma) +
        labs(
          x = "Reference Period in Relation to the covid-19 Pandemic",
          y = "Value",
          colour = "Visible Minority group(s)",
          fill = "Visible Minority group(s)"
        )
    }, tooltip = "text"))
  }
  
  func_plot_hate <- function(filter_var){
    # Hate crime ----
    filtered_data <-
      reactive({
        polData %>%
          filter(
            #Indicator == filter_var,
            Year %in% input$hate_year,
            Geography == input$hate_geography,
            motivation_type == input$hate_motivation,
            (motivation_type == input$hate_race |
               motivation_type == input$hate_police 
               
               
            )
          )
      })
    
    renderPlotly(ggplotly({
      ggplot(filtered_data()) +
        geom_line(
          stat = "identity",
          position = "identity",
          na.rm = FALSE,
          orientation = NA,
          show.legend = TRUE, 
          #position = position_dodge(width = 0.5),
          aes(
            x = Year,
            y = Percent,
            colour = Motivation,
            fill = Motivation
            # text = paste0(
            #   "Year: ",
            #   Year,
            #   "<br>",
            #   "Value: ",
            #   format(Value, big.mark = ","),
            #   "<br>",
            #   "Year: ",
            #   Year 
        )) +
        theme_minimal() +
        scale_y_continuous(labels = comma) +
        labs(
          x = "Year",
          y = "Number",
          colour = "Motivation",
          fill = "Motivation"
        )
    },
    ))
    
    
  # renderPlotly(ggplotly({
  #   # Require filtered_lineData
  #   req(func_plot_hate())
  #   # Create the base graph
  #   lp <- ggplot(filtered_data(), x = Year)
  #   # Add each series one-by-one as new traces
  #   for (i in 3:length(colnames(func_plot_hate()))) {
  #     lp <- lp %>%
  #       add_trace(x = filtered_data()$Year, y = filtered_data()[[i]],
  #                 type = "scatter", mode = "lines+markers",
  #                 name = colnames(filtered_data())[i])
  #   }
  #   # Note hovermode = "x unified" is not working as it is supposed to
  #   # Best work-around was used in xaxis with spike layout
  #   lp <- lp %>%
  #     layout(title = "Police Reported Hate Crime Time Series Analysis",
  #            hovermode = "Police Reported Hate Crime Time Series Analysis",
  #            xaxis = list(title = "Choose a year",
  #                         showspikes = TRUE,
  #                         spikecolor = "black",
  #                         spikethickness = 2,
  #                         spikemode  = 'toaxis+across',
  #                         spikesnap = 'data',
  #                         showline=TRUE),
  #            yaxis = list(title = "Number")
  #     )
  #   lp
  # }))
  }
  ### Plots ----
  #### 1. Participation in the Labour Market ----
  ##### 1.1. Working-age population in the labour force (participation rate) ----
  output$plot_vm_lm_1 <-
    func_plot_1(df = "rateDT",
                filter_var = unique(as.character(rateDT$Indicator))[1])

  ##### 1.2. Working-age population in employment (employment rate) ----
  output$plot_vm_lm_2 <-
    func_plot_1(df = "rateDT",
                filter_var = unique(as.character(rateDT$Indicator))[2])

  ##### 1.3. Working-age population in employment (unemployment rate) ----
  output$plot_vm_lm_3 <-
    func_plot_1(df = "rateDT",
                filter_var = unique(as.character(rateDT$Indicator))[3])

  ##### 1.4. Workers working mainly full-time weeks in the previous year (Population in full-time employment) ----
  output$plot_vm_lm_4 <-
    func_plot_1(df = "rateDT",
                filter_var = unique(as.character(rateDT$Indicator))[4])

  ##### 1.5. Self-employed workers in the labour force (unincorporated) ----
  output$plot_vm_lm_5 <-
    func_plot_1(df = "representationDT_lm",
                filter_var = unique(as.character(representationDT$Indicator))[4])

  ##### 1.6. Overqualified workers with a university degree ----
  output$plot_vm_lm_6 <-
    func_plot_1(df = "OverQualDT")

  ##### 1.7. Youth not in employment, education or training (NEET) ----
  output$plot_vm_lm_7 <-
    func_plot_1(df = "youthDT")
  #### 2. Civic engagement and political participation ----
  ##### 2.1. Percent of the population members of at least one civic group or organization ----
  output$plot_vm_civic_1 <-
    func_plot_1(df = "civicDT",
                filter_var = unique(as.character(civicDT$Indicator))[1])

  ##### 2.2. Percent of the population members in a sports or recreational organization ----
  output$plot_vm_civic_2 <-
    func_plot_1(df = "civicDT",
                filter_var = unique(as.character(civicDT$Indicator))[2])

  ##### 2.3. Percent of the population members in a cultural, educational or hobby organization ----
  output$plot_vm_civic_3 <-
    func_plot_1(df = "civicDT",
                filter_var = unique(as.character(civicDT$Indicator))[3])

  ##### 2.4. Percent of the population members in union or professional association ----
  output$plot_vm_civic_4 <-
    func_plot_1(df = "civicDT",
                filter_var = unique(as.character(civicDT$Indicator))[4])

  ##### 2.5. Percent of the population members in a political party or group ----
  output$plot_vm_civic_5 <-
    func_plot_1(df = "civicDT",
                filter_var = unique(as.character(civicDT$Indicator))[5])

  ##### 2.6. Percent of the population members in a religious-affiliated group ----
  output$plot_vm_civic_6 <-
    func_plot_1(df = "civicDT",
                filter_var = unique(as.character(civicDT$Indicator))[6])

  ##### 2.7. Percent of the population members in a school group, neighbourhood, civic or community association ----
  output$plot_vm_civic_7 <-
    func_plot_1(df = "civicDT",
                filter_var = unique(as.character(civicDT$Indicator))[7])

  ##### 2.8. Percent of the population members in a humanitarian or charitable organization or service club ----
  output$plot_vm_civic_8 <-
    func_plot_1(df = "civicDT",
                filter_var = unique(as.character(civicDT$Indicator))[8])

  ##### 2.9. Percent of the population members in a seniors' group ----
  output$plot_vm_civic_9 <-
    func_plot_1(df = "civicDT",
                filter_var = unique(as.character(civicDT$Indicator))[9])

  ##### 2.10. Percent of the population members in a youth organization ----
  output$plot_vm_civic_10 <-
    func_plot_1(df = "civicDT",
                filter_var = unique(as.character(civicDT$Indicator))[10])

  ##### 2.11. Percent of the population members in an immigrant or ethnic association or club ----
  output$plot_vm_civic_11 <-
    func_plot_1(df = "civicDT",
                filter_var = unique(as.character(civicDT$Indicator))[11])

  ##### 2.12. Percent of the population members in an environmental group ----
  output$plot_vm_civic_12 <-
    func_plot_1(df = "civicDT",
                filter_var = unique(as.character(civicDT$Indicator))[12])

  ##### 2.13. Percent of the population engaged in political activities ----
  output$plot_vm_civic_13 <-
    func_plot_1(df = "civicDT",
                filter_var = unique(as.character(civicDT$Indicator))[13])

  ##### 2.14. Percent of the population voting in the last federal election ----
  output$plot_vm_civic_14 <-
    func_plot_1(df = "civicDT2",
                filter_var = unique(as.character(civicDT2$Indicator))[1])

  ##### 2.15. Percent of the population voting in the last provincial election ----
  output$plot_vm_civic_15 <-
    func_plot_1(df = "civicDT2",
                filter_var = unique(as.character(civicDT2$Indicator))[2])

  ##### 2.16. Percent of the population voting in the last municipal election ----
  output$plot_vm_civic_16 <-
    func_plot_1(df = "civicDT2",
                filter_var = unique(as.character(civicDT2$Indicator))[3])

  #### 3. Representation in decision-making positions ----
  ##### 3.1. Percent of workers in all management occupations ----
  output$plot_vm_rep_1 <-
    func_plot_1(df = "representationDT",
                filter_var = unique(as.character(representationDT$Indicator))[1])

  ##### 3.2. Percent of workers in senior management occupations ----
  output$plot_vm_rep_2 <-
    func_plot_1(df = "representationDT",
                filter_var = unique(as.character(representationDT$Indicator))[2])

  #'NOTE [Not sure what's happening with this middle manager section]
  ##### 3.3. Percent of workers in specialized middle management occupations ----
  #### 3.4. Percent of workers in other middle management occupations ----
  output$plot_vm_rep_4 <-
    func_plot_1(df = "representationDT",
              filter_var = unique(as.character(representationDT$Indicator))[4])

  #### 4. Basic needs and housing ----
  ##### 4.1. Percent of the population living in a dwelling owned by one member of the household ----
  # output$plot_vm_basic_1 <-
  #   func_plot_1(df = "basicDT",
  #             filter_var = unique(basicDT$Indicator)[])

  ##### 4.2. Percent of the population living in core need household ----
  # output$plot_vm_basic_2 <-
  #   func_plot_1(df = "basicDT",
  #             filter_var = unique(basicDT$Indicator)[])

  ##### 4.3. Percent of the population living in suitable housing ----
  # output$plot_vm_basic_3 <-
  #   func_plot_1(df = "basicDT",
  #             filter_var = unique(basicDT$Indicator)[])

  ##### 4.4. Percent of the population living in an affordable housing ----
  # output$plot_vm_basic_4 <-
  #   func_plot_1(df = "basicDT",
  #             filter_var = unique(basicDT$Indicator)[])

  ##### 4.5. Percent of the population living in a food-secure household ----
  output$plot_vm_basic_5 <-
    func_plot_1(df = "basicDT",
                filter_var = unique(as.character(basicDT$Indicator))[14])

  ##### 4.6. Percent of the population living in a household with marginal food security ----
  output$plot_vm_basic_6 <-
    func_plot_1(df = "basicDT",
                filter_var = unique(as.character(basicDT$Indicator))[15])

  ##### 4.7. Percent of the population living in a food-insecure household, moderate or severe ----
  output$plot_vm_basic_7 <-
    func_plot_1(df = "basicDT",
                filter_var = unique(as.character(basicDT$Indicator))[16])

  ##### 4.8. Percent of the population living in a household with moderate food insecurity ----
  output$plot_vm_basic_8 <-
    func_plot_1(df = "basicDT",
                filter_var = unique(as.character(basicDT$Indicator))[17])

  ##### 4.9. Percent of the population living in a household with severe food insecurity ----
  output$plot_vm_basic_9 <-
    func_plot_1(df = "basicDT",
                filter_var = unique(as.character(basicDT$Indicator))[18])

  #### 5. Local community ----
  #'NOTE [TBD]

  #### 6. Health and wellbeing ----
  ##### 6.1. Percent of the population reporting very good or excellent general health ----
  output$plot_vm_health_1 <-
    func_plot_1(df = "basicDT_health",
                filter_var = unique(as.character(basicDT$Indicator))[1])

  ##### 6.2. Percent of the population reporting fair or poor general health ----
  output$plot_vm_health_2 <-
    func_plot_1(df = "basicDT_health",
                filter_var = unique(as.character(basicDT$Indicator))[2])

  ##### 6.3. Percent of the population reporting very good or excellent mental health ----
  output$plot_vm_health_3 <-
    func_plot_1(df = "basicDT_health",
                filter_var = unique(as.character(basicDT$Indicator))[3])

  ##### 6.4. Percent of the population reporting fair or poor mental health ----
  output$plot_vm_health_4 <-
    func_plot_1(df = "basicDT_health",
                filter_var = unique(as.character(basicDT$Indicator))[4])

  ##### 6.5. Percent of the population reporting their life stressful ----
  output$plot_vm_health_5 <-
    func_plot_1(df = "basicDT_health",
                filter_var = unique(as.character(basicDT$Indicator))[5])

  ##### 6.6. Percent of the population reporting life satisfaction, satisfied or very satisfied ----
  output$plot_vm_health_6 <-
    func_plot_1(df = "basicDT_health",
                filter_var = unique(as.character(basicDT$Indicator))[6])

  # ##### 6.7. Percent of the population reporting having a regular healthcare providers ----
  output$plot_vm_health_7 <-
    func_plot_1(df = "basicDT_health",
                filter_var = unique(as.character(basicDT$Indicator)[7]))
  ##### 6.8. Percent of the population reporting no need for mental health care ----
  output$plot_vm_health_8 <-
    func_plot_1(df = "basicDT_health",
                filter_var = unique(as.character(basicDT$Indicator)[8]))
  ##### 6.9. Percent of the population reporting all needs met for mental health care ----
  output$plot_vm_health_9 <-
    func_plot_1(df = "basicDT_health",
                filter_var = unique(as.character(basicDT$Indicator)[9]))
  ##### 6.10. Percent of the population reporting needs partially met for mental health care ----
  output$plot_vm_health_10 <-
    func_plot_1(df = "basicDT_health",
                filter_var = unique(as.character(basicDT$Indicator)[10]))
  ##### 6.11. Percent of the population reporting needs partially met or needs not met for mental health care ----
  output$plot_vm_health_11 <-
    func_plot_1(df = "basicDT_health",
                filter_var = unique(as.character(basicDT$Indicator)[11]))
  ##### 6.12. Percent of the population reporting needs not met for mental health cares ----
  output$plot_vm_health_12 <-
    func_plot_1(df = "basicDT_health",
                filter_var = unique(as.character(basicDT$Indicator)[12]))
  ##### 6.13. Percent of the population reporting unmet health care needs ----
  output$plot_vm_health_13 <-
    func_plot_1(df = "basicDT_health",
                filter_var = unique(as.character(basicDT$Indicator)[13]))

  #### 7. Public services and institutions ----
  #'NOTE [The indicators weren't in the same order as the indicators for confidenceDT]
  ##### 7.1. Population expressing confidence in Federal Parliament ----
  output$plot_vm_public_1 <-
    func_plot_1(df = "confidenceDT",
                filter_var = unique(as.character(confidenceDT$Indicator))[4])

  ##### 7.2. Population expressing Confidence in the Canadian media ----
  output$plot_vm_public_2 <-
    func_plot_1(df = "confidenceDT",
                filter_var = unique(as.character(confidenceDT$Indicator))[8])

  ##### 7.3. Population expressing confidence in the school system ----
  output$plot_vm_public_3 <-
    func_plot_1(df = "confidenceDT",
                filter_var = unique(as.character(confidenceDT$Indicator))[3])

  ##### 7.4. Population expressing confidence in the justice system, courts ----
  output$plot_vm_public_4 <-
    func_plot_1(df = "confidenceDT",
                filter_var = unique(as.character(confidenceDT$Indicator))[2])

  ##### 7.5. Population expressing confidence in the police ----
  output$plot_vm_public_5 <-
    func_plot_1(df = "confidenceDT",
                filter_var = unique(as.character(confidenceDT$Indicator))[1])

  ##### 7.6. Population expressing confidence in major corporations ----
  output$plot_vm_public_6 <-
    func_plot_1(df = "confidenceDT",
                filter_var = unique(as.character(confidenceDT$Indicator))[6])

  ##### 7.7. Population expressing confidence in merchants and business people ----
  output$plot_vm_public_7 <-
    func_plot_1(df = "confidenceDT",
                filter_var = unique(as.character(confidenceDT$Indicator))[7])

  ##### 7.8. Population expressing confidence in banks ----
  output$plot_vm_public_8 <-
    func_plot_1(df = "confidenceDT",
                filter_var = unique(as.character(confidenceDT$Indicator))[5])

  #### 11. Income and wealth ----
  #'NOTE [TBD]

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

  #### 9. Social connections and personnal networks ----
  ##### 9.1. Percent of the population living alone ----
  # output$plot_vm_social_1 <-
  #   func_plot_1(df = "belongingDT",
  #             filter_var = unique(belongingDT$Indicator)[])

  ##### 9.2. Median size of a personal local network with close ties ----
  # output$plot_vm_social_2 <-
  #   func_plot_1(df = "belongingDT",
  #             filter_var = unique(belongingDT$Indicator)[])

  ##### 9.3. Average size of a local personal network with close ties ----
  # output$plot_vm_social_3 <-
  #   func_plot_1(df = "belongingDT",
  #             filter_var = unique(belongingDT$Indicator)[])

  ##### 9.4. Percent of the population with a personal close-ties network of 10 or more people ----
  # output$plot_vm_social_4 <-
  #   func_plot_1(df = "belongingDT",
  #             filter_var = unique(belongingDT$Indicator)[])

  ##### 9.5. Percent of the population with a personal close-ties network of 5 or more relatives ----
  # output$plot_vm_social_5 <-
  #   func_plot_1(df = "belongingDT",
  #             filter_var = unique(belongingDT$Indicator)[])

  ##### 9.6. Percent of the population with a personal close-ties network of 5 or more friends ----
  # output$plot_vm_social_6 <-
  #   func_plot_1(df = "belongingDT",
  #             filter_var = unique(belongingDT$Indicator)[])

  ##### 9.7. Percent of the population with no personal network with weak ties ----
  # output$plot_vm_social_7<-
  #   func_plot_1(df = "belongingDT",
  #             filter_var = unique(belongingDT$Indicator)[])

  ##### 9.8. Percent of the population with a personal weak-ties network of 1 to 19 people ----
  # output$plot_vm_social_8 <-
  #   func_plot_1(df = "belongingDT",
  #             filter_var = unique(belongingDT$Indicator)[])

  ##### 9.9. Percent of the population with a personal weak-ties network of 20 or more people ----
  # output$plot_vm_social_9 <-
  #   func_plot_1(df = "belongingDT",
  #             filter_var = unique(belongingDT$Indicator)[])

  ##### 9.10. Percent of the population with a personal ethnically-diverse network ----
  # output$plot_vm_social_10 <-
  #   func_plot_1(df = "belongingDT",
  #             filter_var = unique(belongingDT$Indicator)[])
  #### 9.11. Population reporting that most people can be trusted ----
  output$plot_vm_social_11 <-
    func_plot_1(df = "belongingDT",
              filter_var = unique(belongingDT$Indicator)[1])
  #### 9.12. Population reporting strong sense of belonging to their local community ----
  output$plot_vm_social_12 <-
    func_plot_1(df = "belongingDT",
              filter_var = unique(belongingDT$Indicator)[2])
  #### 9.13. Population reporting strong sense of belonging to their town or city ----
  output$plot_vm_social_13 <-
    func_plot_1(df = "belongingDT",
              filter_var = unique(belongingDT$Indicator)[3])
  #### 9.14. Population reporting strong sense of belonging to their province ----
  output$plot_vm_social_14 <-
    func_plot_1(df = "belongingDT",
              filter_var = unique(belongingDT$Indicator)[4])
  #### 9.15. Population reporting strong sense of belonging to Canada ----
  output$plot_vm_social_15 <-
    func_plot_1(df = "belongingDT",
              filter_var = unique(belongingDT$Indicator)[5])

  #### 10. Discrimination and victimization ----
  ##### 10.1. Experience(s) of discrimination ----
  output$plot_vm_discrimination_1 <-
    func_plot_discrimination(filter_var = unique(as.character(discriminationDT$ind))[1])

  # ##### 10.2. Experience(s) of discrimination based on ethnicity or culture ----
  output$plot_vm_discrimination_2 <-
    func_plot_discrimination(filter_var = unique(as.character(discriminationDT$ind))[2])

  # ##### 10.3. Experience(s) of discrimination based on race or colour ----
  output$plot_vm_discrimination_3 <-
    func_plot_discrimination(filter_var = unique(as.character(discriminationDT$ind))[3])

  # ##### 10.4. Experience(s) of discrimination based on religion ----
  output$plot_vm_discrimination_4 <-
    func_plot_discrimination(filter_var = unique(as.character(discriminationDT$ind))[4])

  # ##### 10.5. Experience(s) of discrimination based on language ----
  output$plot_vm_discrimination_5 <-
    func_plot_discrimination(filter_var = unique(as.character(discriminationDT$ind))[5])

  # ##### 10.6. Discrimination at work or when applying for a job or promotion ----
  output$plot_vm_discrimination_6 <-
    func_plot_discrimination(filter_var = unique(as.character(discriminationDT$ind))[6])

  # ##### 10.7. Discrimination when dealing with the police ----
  output$plot_vm_discrimination_7<-
    func_plot_discrimination(filter_var = unique(as.character(discriminationDT$ind))[7])

  # ##### 10.8. Discrimination when in a store, bank or restaurant ----
  output$plot_vm_discrimination_8 <-
    func_plot_discrimination(filter_var = unique(as.character(discriminationDT$ind))[8])

  # ##### 10.9. Discrimination when attending school or classes ----
  output$plot_vm_discrimination_9 <-
    func_plot_discrimination(filter_var = unique(as.character(discriminationDT$ind))[9])
  ##### 11.1. Average employment income of the population ----
  output$plot_vm_inc_1 <-
    func_plot_1(df = "incomeDT",
                filter_var = unique(as.character(incomeDT$Indicator))[1])
  ##### 11.2. Average weekly wage of paid employees ----
  output$plot_vm_inc_2 <-
    func_plot_1(df = "incomeDT",
                filter_var = unique(as.character(incomeDT$Indicator))[2])
  # ##### 10.9. Hate crime ----
  output$plot_vm_hate_crime <-
    func_plot_hate(filter_var = unique(as.character(polData$ind))[1])

  #'NOTE [Here is where you should add the next tab // use what's in Tab 1 as a reference -- you might need to reconfigure the function to the breakdown that's relevant to your new tab]
  ## Tab 2: Geography ------
  ### Filters - Provinces and Regions----
  #'NOTE [This makes it so the theme filter affects the indicator filter]
  observe(
    updateSelectizeInput(
      session = session,
      inputId = "indicator_2",
      #'NOTE [Not showing the following indicators until they are ready // and "Hate crime" because there's no breakdown by vismin]
      choices = as.character(unique(template$Indicator[template$Theme == input$theme_2 &
                                                         !template$Indicator %in% c(
                                                           "Workers in specialized middle management occupations",
                                                           
                                                           "Percent of the population living in a dwelling owned by one member of the household",
                                                           "Percent of the population living in core need household",
                                                           "Percent of the population living in suitable housing",
                                                           "Percent of the population living in an affordable housing",
                                                           
                                                           "Knowledge of official languages, English only",
                                                           "Knowledge of official languages, French only",
                                                           "Knowledge of official languages, English and French",
                                                           "Knowledge of official languages, neither English nor French",
                                                           "Received a formal training paid by the employer in the past 12 months",
                                                           "Received an informal on-the-job training (from co-workers or supervisors) in the past 12 months",
                                                           
                                                           
                                                           "Average total household income, adjusted for the number of persons",
                                                           "Percent of the population living in poverty (low-income MBM)",
                                                           "Percent of the population living in low income situation (before-tax)",
                                                           "Percent of the population living in low income situation (after-tax)",
                                                           "Percent of the population reporting difficulty in meeting financial needs of their household",
                                                           "Percent of the population reporting ease in meeting financial needs of their household",
                                                           
                                                           "Population living alone",
                                                           "Percent of the population with a personal close-ties network of 10 or more people",
                                                           "Feeling close to 5 relatives or more",
                                                           "Feeling close to 5 friends or more",
                                                           "Having no other friends or acquaintances",
                                                           "Having 1 to 19 other friends or acquaintances",
                                                           "Having 20 or more other friends or acquaintances",
                                                           "Having ethnically diverse networks of friends and acquaintances"
                                                         )]))
    )
  )
  
#'   #### Functions for Visible Minority----
#'   func_plot_3 <- function(df, filter_var = NULL)
#'   {
#'     if (df == "rateDT_geo") {
#'       # Participation in the Labour Market (general) - Geo ----
#'       filtered_data <-
#'         reactive({
#'           rateDT %>%
#'             filter(
#'               Indicator == filter_var,
#'               VisMin == input$lm_vismin_geo,
#'               Degree == input$lm_degree_geo,
#'               Year %in% input$lm_year_geo,
#'               Geography %in% input$lm_geography_geo,
#'               Immigration == input$lm_immigration_geo,
#'               Age == input$lm_age_geo,
#'               Sex == input$lm_sex_geo
#'             )
#'         })
#'     
#'     }else if (df == "representationDT_geo") {
#'       # Participation in the Labour Market (representationDT) - Geo----
#'       filtered_data <-
#'         reactive({
#'           representationDT %>%
#'             filter(
#'               Indicator == filter_var,
#'               VisMin == input$lm_rep_vismin_geo,
#'               Year  %in% input$lm_rep_year_geo,
#'               Degree == input$lm_rep_degree_geo,
#'               Geography %in%input$lm_rep_geography_geo,
#'               Immigration == input$lm_rep_immigration_geo,
#'               Age == input$lm_rep_age_geo,
#'               Sex == input$lm_rep_sex_geo
#'             )
#'         })
#'     } else if (df == "OverQualDT_geo") {
#'       # Participation in the Labour Market (OverQualDT)- Geo----
#'       filtered_data <-
#'         reactive({
#'           OverQualDT %>%
#'             filter(
#'               VisMin == input$lm_over_vismin_geo,
#'               Year %in% input$lm_over_year_geo,
#'               Location == input$lm_over_location_geo,
#'               Degree == input$lm_over_degree_geo,
#'               Geography %in%  input$lm_over_geography_geo,
#'               Immigration == input$lm_over_immigration_geo,
#'               Age == input$lm_over_age_geo,
#'               Sex == input$lm_over_sex_geo,
#'               Language == input$lm_over_language_geo
#'             )
#'         })
#'     } else if (df == "youthDT_geo") {
#'       # Participation in the Labour Market (youthDT)- Geo ----
#'       filtered_data <-
#'         reactive({
#'           youthDT %>%
#'             filter(
#'               VisMin == input$lm_youth_vismin_geo,
#'               Year  %in% input$lm_youth_year_geo,
#'               Geography %in%  input$lm_youth_geography_geo,
#'               Immigration == input$lm_youth_immigration_geo,
#'               Age == input$lm_youth_age_geo,
#'               Sex == input$lm_youth_sex_geo,
#'               Language == input$lm_youth_language_geo
#'             )
#'         })
#'     } else if (df == "incomeDT_geo") {
#'       # Participation in the Labour Market (incomeDT)- Geo----
#'       filtered_data <-
#'         reactive({
#'           incomeDT %>%
#'             filter(
#'               Indicator == filter_var,
#'               VisMin ==  input$lm_income_vismin_geo,
#'               Year  %in% input$lm_income_year_geo,
#'               Degree == input$lm_income_degree_geo,
#'               Geography %in% input$lm_income_geography_geo,
#'               Immigration == input$lm_income_immigration_geo,
#'               Age == input$lm_income_age_geo,
#'               Sex == input$lm_income_sex_geo
#'             )
#'         })
#'     }
#'     else if (df == "civicDT_geo") {
#'       # Civic engagement and political participation (civicDT) - Geo ----
#'       filtered_data <-
#'         reactive({
#'           civicDT %>%
#'             filter(
#'               Indicator == filter_var,
#'               VisMin == input$civic_vismin_geo,
#'               Geography %in% input$civic_geography_geo,
#'               Confidence == input$civic_conf_interval_geo,
#'               char_type == input$civic_sociodem_geo,
#'               (Characteristic == input$civic_age_geo |
#'                  Characteristic == input$civic_sex_geo |
#'                  Characteristic == input$civic_immigration_geo |
#'                  Characteristic == input$civic_generation_geo |
#'                  Characteristic == input$civic_language_geo |
#'                  Characteristic == input$civic_education_geo
#'               )
#'             )
#'         })
#'     } else if (df == "civicDT2_geo") {
#'       # Civic engagement and political participation (civicDT2)- Geo ----
#'       filtered_data <-
#'         reactive({
#'           civicDT2 %>%
#'             filter(
#'               Indicator == filter_var,
#'               VisMin == input$civic2_vismin_geo,
#'               Geography %in% input$civic2_geography_geo,
#'               Confidence == input$civic2_conf_interval_geo,
#'               char_type == input$civic2_sociodem_geo,
#'               (Characteristic == input$civic2_age_geo |
#'                  Characteristic == input$civic2_sex_geo |
#'                  Characteristic == input$civic2_immigration_geo |
#'                  Characteristic == input$civic2_generation_geo |
#'                  Characteristic == input$civic2_language_geo |
#'                  Characteristic == input$civic2_education_geo
#'               )
#'             )
#'         })
#'     }
#'     else if (df == "representationDT_geo2") {
#'       # Representation in decision-making positions - Geo----
#'       filtered_data <-
#'         reactive({
#'           representationDT %>%
#'             filter(
#'               Indicator == filter_var,
#'               VisMin ==  input$rep_vismin_geo,
#'               Year %in% input$rep_year_geo,
#'               Degree == input$rep_degree_geo,
#'               Geography %in%input$rep_geography_geo,
#'               Immigration == input$rep_immigration_geo,
#'               Sex == input$rep_sex_geo,
#'               Age == input$rep_age_geo
#'             )
#'         })
#'     }
#'     else if (df == "educationDT_geo") {
#'       # Education training and skills - Geo----
#'       filtered_data <-
#'         reactive({
#'           educationDT %>%
#'             filter(
#'               Indicator == filter_var,
#'               VisMin == input$education_vismin_geo,
#'               Year %in% input$education_year_geo,
#'               Language == input$education_language_geo,
#'               Geography %in% input$education_geography_geo,
#'               Immigration == input$education_immigration_geo,
#'               Age == input$education_age_geo,
#'               Sex == input$education_sex_geo
#'             )
#'         })
#'     }
#'     else if (df == "belongingDT_geo") {
#'       # Social connections and personal networks - Geo----
#'       filtered_data <-
#'         reactive({
#'           belongingDT %>%
#'             filter(
#'               Indicator == filter_var,
#'               VisMin == input$belonging_vismin_geo,
#'               Geography %in% input$belonging_geography_geo,
#'               Confidence == input$belonging_conf_interval_geo,
#'               char_type == input$belonging_sociodem_geo,
#'               (Characteristic == input$belonging_age_geo |
#'                  Characteristic == input$public_income_social_gender_geo |
#'                  Characteristic == input$belonging_immigration_geo |
#'                  Characteristic == input$belonging_generation_geo |
#'                  Characteristic == input$belonging_language_geo |
#'                  Characteristic == input$belonging_education_geo
#'               )
#'             )
#'         })
#'     }
#'     else if (df == "basicDT_geo") {
#'       # Basic needs and housing - Geo ----
#'       filtered_data <-
#'         reactive({
#'           healthDT %>%
#'             filter(
#'               Indicator == filter_var,
#'               Year %in%  input$basic_year_geo,
#'               Geography  %in% input$basic_geography_geo,
#'               Confidence == input$basic_conf_interval_geo,
#'               char_type == input$basic_sociodem_geo,
#'               (Characteristic == input$basic_vismin_geo |
#'                  Characteristic == input$basic_sex_geo |
#'                  Characteristic == input$basic_immigration_geo
#'               )
#'             )
#'         })
#'     }
#'     else if (df == "healthDT_geo") {
#'       # Health and well being - Geo----
#'       filtered_data <-
#'         reactive({
#'           healthDT %>%
#'             filter(
#'               Indicator == filter_var,
#'               Year  %in% input$health_year_geo,
#'               Geography %in% input$health_geography_geo,
#'               Confidence == input$health_conf_interval_geo,
#'               char_type == input$health_sociodem_geo,
#'               (Characteristic == input$health_vismin_geo|
#'                  Characteristic == input$health_sex_geo |
#'                  Characteristic == input$health_immigration_geo
#'               )
#'             )
#'         })
#'     }
#'     else if (df == "confidenceDT_geo") {
#'       # Public services and institutions - Geo----
#'       filtered_data <-
#'         reactive({
#'           confidenceDT %>%
#'             filter(
#'               Indicator == filter_var,
#'               VisMin == input$public_vismin_geo,
#'               Geography %in% input$public_geography_geo,
#'               Confidence == input$public_conf_interval_geo,
#'               char_type == input$public_sociodem_geo,
#'               (Characteristic == input$public_age_geo |
#'                  Characteristic == input$public_sex_geo |
#'                  Characteristic == input$public_immigration_geo |
#'                  Characteristic == input$public_generation_geo |
#'                  Characteristic == input$public_language_geo |
#'                  Characteristic == input$public_education_geo
#'               )
#'             )
#'         })
#'     }
#'     # fig <- ggplotly(
#'     #   canada_shapefile %>%
#'     #     st_join(.,canada_shapefile, by = Geography) %>%
#'     #     ggplot() +
#'     #     geom_sf(
#'     #       aes(
#'     #       fill = Geography)) +
#'     #     ggtitle("Labour Force Status by Groups Designated as Visible Minorities")
#'     # )
#'     # fig
#' 
#'     renderPlotly(ggplotly({
#'       ggplot(filtered_data()) +
#'         geom_bar(
#'           stat = "identity",
#'           width = 0.5,
#'           position = position_dodge(width = 0.4),
#'           aes(
#'             x = Geography,
#'             y = Value,
#'             colour = Geography,
#'             fill = Geography,
#'             text = paste0(
#'               "Geography: ",
#'               Geography,
#'               "<br>",
#'               "Value: ",
#'               format(Value, big.mark = ","),
#'               "<br>",
#'               "Geography: ",
#'               Year
#'             )
#'           )
#'         ) + coord_flip()+
#'         theme_minimal() +
#'         scale_y_continuous(labels = comma) +
#'         labs(
#'           x = "Geography",
#'           y = "Value",
#'           colour = "Geography",
#'           fill = "Geography"
#'         )
#'     }, tooltip = "text"))
#'    }
#'   
#'   #'NOTE [This chart doesn't follow the same x-axis as the other charts]
#'   func_plot3_discrimination <- function(filter_var){
#'     # Discrimination and victimization - Geo----
#'     filtered_data <-
#'       reactive({
#'         discriminationDT %>%
#'           filter(
#'             ind == filter_var,
#'             VisMin ==  input$discrimination_vismin_geo,
#'             Geography %in% input$discrimination_geography_geo,
#'             Confidence == input$discrimination_conf_interval_geo,
#'             char_type == input$discrimination_sociodem_geo,
#'             (Characteristic == input$discrimination_age_geo |
#'                Characteristic == input$discrimination_sex_geo |
#'                Characteristic == input$discrimination_immigration_geo |
#'                Characteristic == input$discrimination_generation_geo |
#'                Characteristic == input$discrimination_language_geo |
#'                Characteristic == input$discrimination_education_geo
#'             )
#'           )
#'       })
#' 
#'     renderPlotly(ggplotly({
#'       ggplot(filtered_data()) +
#'         geom_bar(
#'           stat = "identity",
#'           width = 0.4,
#'           position = position_dodge(width = 0.5),
#'           aes(
#'             x = before_since,
#'             y = Value,
#'             colour = Geography,
#'             fill = Geography,
#'             text = paste0(
#'               "Geography:",
#'               Geography,
#'               "<br>",
#'               "Value: ",
#'               format(Value, big.mark = ","),
#'               "<br>",
#'               "Reference Period in Relation to the covid-19 Pandemic: ",
#'               before_since
#'             )
#'           )
#'         ) +
#'         theme_minimal() +
#'         scale_y_continuous(labels = comma) +
#'         labs(
#'           x = "Reference Period in Relation to the covid-19 Pandemic",
#'           y = "Value",
#'           colour = "Geography",
#'           fill = "Geography"
#'         )
#'     }, tooltip = "text"))
#'   }
#'   ### Plots Provinces and Regions ----
#'   #### 1. Participation in the Labour Market ----
#'   ##### 1.1. Working-age population in the labour force (participation rate) ----
#' output$plot_geo_lm_1 <- 
#'     func_plot_3(df = "rateDT_geo",
#'                 filter_var = unique(as.character(rateDT$Indicator))[1])
#'   
#'   ##### 1.2. Working-age population in employment (employment rate) ----
#'   output$plot_geo_lm_2 <-
#'     func_plot_3(df = "rateDT_geo",
#'                 filter_var = unique(as.character(rateDT$Indicator))[2])
#' 
#'   ##### 1.3. Working-age population in employment (unemployment rate) ----
#'   output$plot_geo_lm_3 <-
#'     func_plot_3(df = "rateDT_geo",
#'                 filter_var = unique(as.character(rateDT$Indicator))[3])
#' 
#'   ##### 1.4. Workers working mainly full-time weeks in the previous year (Population in full-time employment) ----
#'   output$plot_geo_lm_4 <-
#'     func_plot_3(df = "rateDT_geo",
#'                 filter_var = unique(as.character(rateDT$Indicator))[4])
#'   ##### 1.5. Self-employed workers in the labour force (unincorporated) ----
#'   output$plot_geo_lm_5 <-
#'     func_plot_3(df = "representationDT_geo",
#'                 filter_var = unique(as.character(representationDT$Indicator))[4])
#'   
#'   ##### 1.6. Overqualified workers with a university degree ----
#'   output$plot_geo_lm_6 <-
#'     func_plot_3(df = "OverQualDT_geo")
#'   
#'   ##### 1.7. Youth not in employment, education or training (NEET) ----
#'   output$plot_geo_lm_7 <-
#'     func_plot_3(df = "youthDT_geo")
#'   #### 2. Civic engagement and political participation ----
#'   ##### 2.1. Percent of the population members of at least one civic group or organization ----
#'   output$plot_geo_civic_1 <-
#'     func_plot_3(df = "civicDT_geo",
#'                 filter_var = unique(as.character(civicDT$Indicator))[1])
#'   
#'   ##### 2.2. Percent of the population members in a sports or recreational organization ----
#'   output$plot_geo_civic_2 <-
#'     func_plot_3(df = "civicDT_geo",
#'                 filter_var = unique(as.character(civicDT$Indicator))[2])
#'   
#'   ##### 2.3. Percent of the population members in a cultural, educational or hobby organization ----
#'   output$plot_geo_civic_3 <-
#'     func_plot_3(df = "civicDT_geo",
#'                 filter_var = unique(as.character(civicDT$Indicator))[3])
#'   
#'   ##### 2.4. Percent of the population members in union or professional association ----
#'   output$plot_geo_civic_4 <-
#'     func_plot_3(df = "civicDT_geo",
#'                 filter_var = unique(as.character(civicDT$Indicator))[4])
#'   
#'   ##### 2.5. Percent of the population members in a political party or group ----
#'   output$plot_geo_civic_5 <-
#'     func_plot_3(df = "civicDT_geo",
#'                 filter_var = unique(as.character(civicDT$Indicator))[5])
#'   
#'   ##### 2.6. Percent of the population members in a religious-affiliated group ----
#'   output$plot_geo_civic_6 <-
#'     func_plot_3(df = "civicDT_geo",
#'                 filter_var = unique(as.character(civicDT$Indicator))[6])
#'   
#'   ##### 2.7. Percent of the population members in a school group, neighbourhood, civic or community association ----
#'   output$plot_geo_civic_7 <-
#'     func_plot_3(df = "civicDT_geo",
#'                 filter_var = unique(as.character(civicDT$Indicator))[7])
#'   
#'   ##### 2.8. Percent of the population members in a humanitarian or charitable organization or service club ----
#'   output$plot_geo_civic_8 <-
#'     func_plot_3(df = "civicDT_geo",
#'                 filter_var = unique(as.character(civicDT$Indicator))[8])
#'   
#'   ##### 2.9. Percent of the population members in a seniors' group ----
#'   output$plot_geo_civic_9 <-
#'     func_plot_3(df = "civicDT_geo",
#'                 filter_var = unique(as.character(civicDT$Indicator))[9])
#'   
#'   ##### 2.10. Percent of the population members in a youth organization ----
#'   output$plot_geo_civic_10 <-
#'     func_plot_3(df = "civicDT_geo",
#'                 filter_var = unique(as.character(civicDT$Indicator))[10])
#'   
#'   ##### 2.11. Percent of the population members in an immigrant or ethnic association or club ----
#'   output$plot_geo_civic_11 <-
#'     func_plot_3(df = "civicDT_geo",
#'                 filter_var = unique(as.character(civicDT$Indicator))[11])
#'   
#'   ##### 2.12. Percent of the population members in an environmental group ----
#'   output$plot_geo_civic_12 <-
#'     func_plot_3(df = "civicDT_geo",
#'                 filter_var = unique(as.character(civicDT$Indicator))[12])
#'   
#'   ##### 2.13. Percent of the population engaged in political activities ----
#'   output$plot_geo_civic_13 <-
#'     func_plot_3(df = "civicDT_geo",
#'                 filter_var = unique(as.character(civicDT$Indicator))[13])
#'   
#'   ##### 2.14. Percent of the population voting in the last federal election ----
#'   output$plot_geo_civic_14 <-
#'     func_plot_3(df = "civicDT2_geo",
#'                 filter_var = unique(as.character(civicDT2$Indicator))[1])
#'   
#'   ##### 2.15. Percent of the population voting in the last provincial election ----
#'   output$plot_geo_civic_15 <-
#'     func_plot_3(df = "civicDT2_geo",
#'                 filter_var = unique(as.character(civicDT2$Indicator))[2])
#'   
#'   ##### 2.16. Percent of the population voting in the last municipal election ----
#'   output$plot_geo_civic_16 <-
#'     func_plot_3(df = "civicDT2_geo",
#'                 filter_var = unique(as.character(civicDT2$Indicator))[3])
#'   #### 3. Representation in decision-making positions ----
#'   ##### 3.1. Percent of workers in all management occupations ----
#'   output$plot_geo_rep_1 <-
#'     func_plot_3(df = "representationDT_geo2",
#'                 filter_var = unique(as.character(representationDT$Indicator))[1])
#'   
#'   ##### 3.2. Percent of workers in senior management occupations ----
#'   output$plot_geo_rep_2 <-
#'     func_plot_3(df = "representationDT_geo2",
#'                 filter_var = unique(as.character(representationDT$Indicator))[2])
#'   
#'   #'NOTE [Not sure what's happening with this middle manager section]
#'   ##### 3.3. Percent of workers in specialized middle management occupations ----
#'   #### 3.4. Percent of workers in other middle management occupations ----
#'   output$plot_geo_rep_4 <-
#'     func_plot_3(df = "representationDT_geo2",
#'                 filter_var = unique(as.character(representationDT$Indicator))[4])
#'   
#'   #### 4. Basic needs and housing ----
#'   ##### 4.1. Percent of the population living in a dwelling owned by one member of the household ----
#'   # output$plot_geo_basic_1 <-
#'   #   func_plot_3(df = "basicDT_geo",
#'   #             filter_var = unique(healthDT$Indicator)[])
#'   
#'   ##### 4.2. Percent of the population living in core need household ----
#'   # output$plot_geo_basic_2 <-
#'   #   func_plot_3(df = "basicDT_geo",
#'   #             filter_var = unique(healthDT$Indicator)[])
#'   
#'   ##### 4.3. Percent of the population living in suitable housing ----
#'   # output$plot_geo_basic_3 <-
#'   #   func_plot_3(df = "basicDT_geo",
#'   #             filter_var = unique(healthDT$Indicator)[])
#'   
#'   ##### 4.4. Percent of the population living in an affordable housing ----
#'   # output$plot_geo_basic_4 <-
#'   #   func_plot_3(df = "basicDT_geo",
#'   #             filter_var = unique(healthDT$Indicator)[])
#'   
#'   ##### 4.5. Percent of the population living in a food-secure household ----
#'   output$plot_geo_basic_5 <-
#'     func_plot_3(df = "basicDT_geo",
#'                 filter_var = unique(as.character(healthDT$Indicator))[14])
#'   
#'   ##### 4.6. Percent of the population living in a household with marginal food security ----
#'   output$plot_geo_basic_6 <-
#'     func_plot_3(df = "basicDT_geo",
#'                 filter_var = unique(as.character(healthDT$Indicator))[15])
#'   
#'   ##### 4.7. Percent of the population living in a food-insecure household, moderate or severe ----
#'   output$plot_geo_basic_7 <-
#'     func_plot_3(df = "basicDT_geo",
#'                 filter_var = unique(as.character(healthDT$Indicator))[16])
#'   
#'   ##### 4.8. Percent of the population living in a household with moderate food insecurity ----
#'   output$plot_geo_basic_8 <-
#'     func_plot_3(df = "basicDT_geo",
#'                 filter_var = unique(as.character(healthDT$Indicator))[17])
#'   
#'   ##### 4.9. Percent of the population living in a household with severe food insecurity ----
#'   output$plot_geo_basic_9 <-
#'     func_plot_3(df = "basicDT_geo",
#'                 filter_var = unique(as.character(healthDT$Indicator))[18])
#'   #### 5. Local community ----
#'   #'NOTE [TBD]
#'   
#'   #### 6. Health and wellbeing ----
#'   ##### 6.1. Percent of the population reporting very good or excellent general health ----
#'   output$plot_geo_health_1 <-
#'     func_plot_3(df = "healthDT_geo",
#'                 filter_var = unique(as.character(healthDT$Indicator))[1])
#'   
#'   ##### 6.2. Percent of the population reporting fair or poor general health ----
#'   output$plot_geo_health_2 <-
#'     func_plot_3(df = "healthDT_geo",
#'                 filter_var = unique(as.character(healthDT$Indicator))[2])
#'   
#'   ##### 6.3. Percent of the population reporting very good or excellent mental health ----
#'   output$plot_geo_health_3 <-
#'     func_plot_3(df = "healthDT_geo",
#'                 filter_var = unique(as.character(healthDT$Indicator))[3])
#'   
#'   ##### 6.4. Percent of the population reporting fair or poor mental health ----
#'   output$plot_geo_health_4 <-
#'     func_plot_3(df = "healthDT_geo",
#'                 filter_var = unique(as.character(healthDT$Indicator))[4])
#'   
#'   ##### 6.5. Percent of the population reporting their life stressful ----
#'   output$plot_geo_health_5 <-
#'     func_plot_3(df = "healthDT_geo",
#'                 filter_var = unique(as.character(healthDT$Indicator))[5])
#'   
#'   ##### 6.6. Percent of the population reporting life satisfaction, satisfied or very satisfied ----
#'   output$plot_geo_health_6 <-
#'     func_plot_3(df = "healthDT_geo",
#'                 filter_var = unique(as.character(healthDT$Indicator))[6])
#'   
#'   # ##### 6.7. Percent of the population reporting having a regular healthcare providers ----
#'   output$plot_geo_health_7 <-
#'     func_plot_3(df = "healthDT_geo",
#'                 filter_var = unique(as.character(healthDT$Indicator)[7]))
#'   ##### 6.8. Percent of the population reporting no need for mental health care ----
#'   output$plot_geo_health_8 <-
#'     func_plot_3(df = "healthDT_geo",
#'                 filter_var = unique(as.character(healthDT$Indicator)[8]))
#'   ##### 6.9. Percent of the population reporting all needs met for mental health care ----
#'   output$plot_geo_health_9 <-
#'     func_plot_3(df = "healthDT_geo",
#'                 filter_var = unique(as.character(healthDT$Indicator)[9]))
#'   ##### 6.10. Percent of the population reporting needs partially met for mental health care ----
#'   output$plot_geo_health_10 <-
#'     func_plot_3(df = "healthDT_geo",
#'                 filter_var = unique(as.character(healthDT$Indicator)[10]))
#'   ##### 6.11. Percent of the population reporting needs partially met or needs not met for mental health care ----
#'   output$plot_geo_health_11 <-
#'     func_plot_3(df = "healthDT_geo",
#'                 filter_var = unique(as.character(healthDT$Indicator)[11]))
#'   ##### 6.12. Percent of the population reporting needs not met for mental health cares ----
#'   output$plot_geo_health_12 <-
#'     func_plot_3(df = "healthDT_geo",
#'                 filter_var = unique(as.character(healthDT$Indicator)[12]))
#'   ##### 6.13. Percent of the population reporting unmet health care needs ----
#'   output$plot_geo_health_13 <-
#'     func_plot_3(df = "healthDT_geo",
#'                 filter_var = unique(as.character(healthDT$Indicator)[13]))
#'   
#'   #### 7. Public services and institutions ----
#'   #'NOTE [The indicators weren't in the same order as the indicators for confidenceDT]
#'   ##### 7.1. Population expressing confidence in Federal Parliament ----
#'   output$plot_geo_public_1 <-
#'     func_plot_3(df = "confidenceDT_geo",
#'                 filter_var = unique(as.character(confidenceDT$Indicator))[4])
#'   
#'   ##### 7.2. Population expressing Confidence in the Canadian media ----
#'   output$plot_geo_public_2 <-
#'     func_plot_3(df = "confidenceDT_geo",
#'                 filter_var = unique(as.character(confidenceDT$Indicator))[8])
#'   
#'   ##### 7.3. Population expressing confidence in the school system ----
#'   output$plot_geo_public_3 <-
#'     func_plot_3(df = "confidenceDT_geo",
#'                 filter_var = unique(as.character(confidenceDT$Indicator))[3])
#'   
#'   ##### 7.4. Population expressing confidence in the justice system, courts ----
#'   output$plot_geo_public_4 <-
#'     func_plot_3(df = "confidenceDT_geo",
#'                 filter_var = unique(as.character(confidenceDT$Indicator))[2])
#'   
#'   ##### 7.5. Population expressing confidence in the police ----
#'   output$plot_geo_public_5 <-
#'     func_plot_3(df = "confidenceDT_geo",
#'                 filter_var = unique(as.character(confidenceDT$Indicator))[1])
#'   
#'   ##### 7.6. Population expressing confidence in major corporations ----
#'   output$plot_geo_public_6 <-
#'     func_plot_3(df = "confidenceDT_geo",
#'                 filter_var = unique(as.character(confidenceDT$Indicator))[6])
#'   
#'   ##### 7.7. Population expressing confidence in merchants and business people ----
#'   output$plot_geo_public_7 <-
#'     func_plot_3(df = "confidenceDT_geo",
#'                 filter_var = unique(as.character(confidenceDT$Indicator))[7])
#'   
#'   ##### 7.8. Population expressing confidence in banks ----
#'   output$plot_geo_public_8 <-
#'     func_plot_3(df = "confidenceDT_geo",
#'                 filter_var = unique(as.character(confidenceDT$Indicator))[5])
#'   
#'   #### 11. Income and wealth ----
#'   #'NOTE [TBD]
#'   
#'   #### 8. Education training and skills ----
#'   ##### 8.1. Population with no certificate, diploma or degree ----
#'   output$plot_geo_education_1 <-
#'     func_plot_3(df = "educationDT_geo",
#'                 filter_var = unique(as.character(educationDT$Indicator)[1]))
#'   ##### 8.2. Population with high school diploma or equivalency certificate ----
#'   output$plot_geo_education_2 <-
#'     func_plot_3(df = "educationDT_geo",
#'                 filter_var = unique(as.character(educationDT$Indicator)[2]))
#'   ##### 8.3.  Population with postsecondary certificate or diploma below bachelor level ----
#'   output$plot_geo_education_3 <-
#'     func_plot_3(df = "educationDT_geo",
#'                 filter_var = unique(as.character(educationDT$Indicator)[3]))
#'   ##### 8.4.  Population with university certificate or diploma above bachelor level ----
#'   output$plot_geo_education_4 <-
#'     func_plot_3(df = "educationDT_geo",
#'                 filter_var = unique(as.character(educationDT$Indicator)[4]))
#'   ##### 8.5.  Population with bachelor's degree----
#'   output$plot_geo_education_5 <-
#'     func_plot_3(df = "educationDT_geo",
#'                 filter_var = unique(as.character(educationDT$Indicator)[5]))
#'   ##### 8.6.  Population with university certificate or diploma or degree at bachelor level or above ----
#'   output$plot_geo_education_6 <-
#'     func_plot_3(df = "educationDT_geo",
#'                 filter_var = unique(as.character(educationDT$Indicator)[6]))
#'   #### 5. Local community ----
#'   #'NOTE [TBD]
#'   #### 9. Social connections and personnal networks ----
#'   ##### 9.1. Percent of the population living alone ----
#'   # output$plot_geo_social_1 <-
#'   #   func_plot_3(df = "belongingDT_geo",
#'   #             filter_var = unique(belongingDT$Indicator)[])
#'   
#'   ##### 9.2. Median size of a personal local network with close ties ----
#'   # output$plot_geo_social_2 <-
#'   #   func_plot_3(df = "belongingDT_geo",
#'   #             filter_var = unique(belongingDT$Indicator)[])
#'   
#'   ##### 9.3. Average size of a local personal network with close ties ----
#'   # output$plot_geo_social_3 <-
#'   #   func_plot_3(df = "belongingDT_geo",
#'   #             filter_var = unique(belongingDT$Indicator)[])
#'   
#'   ##### 9.4. Percent of the population with a personal close-ties network of 10 or more people ----
#'   # output$plot_geo_social_4 <-
#'   #   func_plot_3(df = "belongingDT_geo",
#'   #             filter_var = unique(belongingDT$Indicator)[])
#'   
#'   ##### 9.5. Percent of the population with a personal close-ties network of 5 or more relatives ----
#'   # output$plot_geo_social_5 <-
#'   #   func_plot_3(df = "belongingDT_geo",
#'   #             filter_var = unique(belongingDT$Indicator)[])
#'   
#'   ##### 9.6. Percent of the population with a personal close-ties network of 5 or more friends ----
#'   # output$plot_geo_social_6 <-
#'   #   func_plot_3(df = "belongingDT_geo",
#'   #             filter_var = unique(belongingDT$Indicator)[])
#'   
#'   ##### 9.7. Percent of the population with no personal network with weak ties ----
#'   # output$plot_geo_social_7<-
#'   #   func_plot_3(df = "belongingDT_geo",
#'   #             filter_var = unique(belongingDT$Indicator)[])
#'   
#'   ##### 9.8. Percent of the population with a personal weak-ties network of 1 to 19 people ----
#'   # output$plot_geo_social_8 <-
#'   #   func_plot_3(df = "belongingDT_geo",
#'   #             filter_var = unique(belongingDT$Indicator)[])
#'   
#'   ##### 9.9. Percent of the population with a personal weak-ties network of 20 or more people ----
#'   # output$plot_geo_social_9 <-
#'   #   func_plot_3(df = "belongingDT_geo",
#'   #             filter_var = unique(belongingDT$Indicator)[])
#'   
#'   ##### 9.10. Percent of the population with a personal ethnically-diverse network ----
#'   # output$plot_geo_social_10 <-
#'   #   func_plot_3(df = "belongingDT_geo",
#'   #             filter_var = unique(belongingDT$Indicator)[])
#'   #### 9.11. Population reporting that most people can be trusted ----
#'   output$plot_geo_social_11 <-
#'     func_plot_3(df = "belongingDT_geo",
#'                 filter_var = unique(belongingDT$Indicator)[1])
#'   #### 9.12. Population reporting strong sense of belonging to their local community ----
#'   output$plot_geo_social_12 <-
#'     func_plot_3(df = "belongingDT_geo",
#'                 filter_var = unique(belongingDT$Indicator)[2])
#'   #### 9.13. Population reporting strong sense of belonging to their town or city ----
#'   output$plot_geo_social_13 <-
#'     func_plot_3(df = "belongingDT_geo",
#'                 filter_var = unique(belongingDT$Indicator)[3])
#'   #### 9.14. Population reporting strong sense of belonging to their province ----
#'   output$plot_geo_social_14 <-
#'     func_plot_3(df = "belongingDT_geo",
#'                 filter_var = unique(belongingDT$Indicator)[4])
#'   #### 9.15. Population reporting strong sense of belonging to Canada ----
#'   output$plot_geo_social_15 <-
#'     func_plot_3(df = "belongingDT_geo",
#'                 filter_var = unique(belongingDT$Indicator)[5])
#'   
#'   #### 10. Discrimination and victimization ----
#'   ##### 10.1. Experience(s) of discrimination ----
#'   output$plot_geo_discrimination_1 <-
#'     func_plot3_discrimination(filter_var = unique(as.character(discriminationDT$ind))[1])
#'   
#'   # ##### 10.2. Experience(s) of discrimination based on ethnicity or culture ----
#'   output$plot_geo_discrimination_2 <-
#'     func_plot3_discrimination(filter_var = unique(as.character(discriminationDT$ind))[2])
#'   
#'   # ##### 10.3. Experience(s) of discrimination based on race or colour ----
#'   output$plot_geo_discrimination_3 <-
#'     func_plot3_discrimination(filter_var = unique(as.character(discriminationDT$ind))[3])
#'   
#'   # ##### 10.4. Experience(s) of discrimination based on religion ----
#'   output$plot_geo_discrimination_4 <-
#'     func_plot3_discrimination(filter_var = unique(as.character(discriminationDT$ind))[4])
#'   
#'   # ##### 10.5. Experience(s) of discrimination based on language ----
#'   output$plot_geo_discrimination_5 <-
#'     func_plot3_discrimination(filter_var = unique(as.character(discriminationDT$ind))[5])
#'   
#'   # ##### 10.6. Discrimination at work or when applying for a job or promotion ----
#'   output$plot_geo_discrimination_6 <-
#'     func_plot3_discrimination(filter_var = unique(as.character(discriminationDT$ind))[6])
#'   
#'   # ##### 10.7. Discrimination when dealing with the police ----
#'   output$plot_geo_discrimination_7<-
#'     func_plot3_discrimination(filter_var = unique(as.character(discriminationDT$ind))[7])
#'   
#'   # ##### 10.8. Discrimination when in a store, bank or restaurant ----
#'   output$plot_geo_discrimination_8 <-
#'     func_plot3_discrimination(filter_var = unique(as.character(discriminationDT$ind))[8])
#'   
#'   # ##### 10.9. Discrimination when attending school or classes ----
#'   output$plot_geo_discrimination_9 <-
#'     func_plot3_discrimination(filter_var = unique(as.character(discriminationDT$ind))[9])
#'   ##### 11.1. Average employment income of the population ----
#'   output$plot_geo_inc_1 <-
#'     func_plot_3(df = "incomeDT_geo",
#'                 filter_var = unique(as.character(incomeDT$Indicator))[1])
#'   ##### 11.2. Average weekly wage of paid employees ----
#'   output$plot_geo_inc_2 <-
#'     func_plot_3(df = "incomeDT_geo",
#'                 filter_var = unique(as.character(incomeDT$Indicator))[2])
#'   
#'   ### Filters ----
#'   #'NOTE [This makes it so the theme filter affects the indicator filter]
#'   observe(
#'     updateSelectizeInput(
#'       session = session,
#'       inputId = "indicator_3",
#'       #'NOTE [Not showing the following indicators until they are ready // and "Hate crime" because there's no breakdown by vismin]
#'       choices = as.character(unique(template$Indicator[template$Theme == input$theme_3 &
#'                                                          !template$Indicator %in% c(
#'                                                            "Currently employed population considering their job related to their education",
#'                                                            "Paid employees considering their current job good for career advancement",
#'                                                            "Paid employees receiving at least one employment benefit in their current job",
#'                                                            "Paid employees having pension plan in their current job",
#'                                                            "Paid employees having paid sick leave in their current job",
#'                                                            "Paid employees having paid vacation leave in their current job",
#'                                                            "Paid employees having disability insurance in their current job",
#'                                                            "Paid employees having supplemental medical care in their current job",
#'                                                            "Paid employees having worker's compensation in their current job",
#'                                                            "Paid employees having maternity, paternity or lay-off benefits in their current job",
#'                                                            "Paid employees covered by union contract or collective agreement in their current job",
#'                                                            "Paid employees receiving formal training in their current job",
#'                                                            "Paid employees receiving informal training in their current job",
#' 
#'                                                            "Percent of workers in specialized middle management occupations",
#' 
#'                                                            "Percent of the population living in a dwelling owned by one member of the household",
#'                                                            "Percent of the population living in core need household",
#'                                                            "Percent of the population living in suitable housing",
#'                                                            "Percent of the population living in an affordable housing",
#' 
#'                                                            "Knowledge of official languages, English only",
#'                                                            "Knowledge of official languages, French only",
#'                                                            "Knowledge of official languages, English and French",
#'                                                            "Knowledge of official languages, neither English nor French",
#'                                                            "Received a formal training paid by the employer in the past 12 months",
#'                                                            "Received an informal on-the-job training (from co-workers or supervisors) in the past 12 months",
#' 
#' 
#'                                                            "Average total household income, adjusted for the number of persons",
#'                                                            "Percent of the population living in poverty (low-income MBM)",
#'                                                            "Percent of the population living in low income situation (before-tax)",
#'                                                            "Percent of the population living in low income situation (after-tax)",
#'                                                            "Percent of the population reporting difficulty in meeting financial needs of their household",
#'                                                            "Percent of the population reporting ease in meeting financial needs of their household",
#' 
#'                                                            "Percent of the population living alone",
#'                                                            "Median size of a personal local network with close ties",
#'                                                            "Average size of a local personal network with close ties",
#'                                                            "Percent of the population with a personal close-ties network of 10 or more people",
#'                                                            "Percent of the population with a personal close-ties network of 5 or more relatives",
#'                                                            "Percent of the population with a personal close-ties network of 5 or more friends",
#'                                                            "Percent of the population with no personal network with weak ties",
#'                                                            "Percent of the population with a personal weak-ties network of 1 to 19 people",
#'                                                            "Percent of the population with a personal weak-ties network of 20 or more people",
#'                                                            "Percent of the population with a personal ethnically-diverse network",
#' 
#'                                                            "Hate Crime"
#'                                                          )]))
#'     )
#'   )
#' 
#'   #### Functions for Geography ----
#'   func_plot_2 <- function(df, filter_var = NULL)
#'   {
#'      if (df == "rateDT_cma") {
#'       # Participation in the Labour Market (general) - Geo(CMAs) ----
#'       filtered_data <-
#'         reactive({
#'           rateDT %>%
#'             filter(
#'               Indicator == filter_var,
#'               VisMin == input$lm_vismin_cma,
#'               Degree == input$lm_degree_cma,
#'               Year %in% input$lm_year_cma,
#'               Geography %in%  input$lm_geography_cma,
#'               Immigration == input$lm_immigration_cma,
#'               Age == input$lm_age_cma,
#'               Sex == input$lm_sex_cma
#'             )
#'         })
#'     }else if (df == "representationDT_cma") {
#'       # Participation in the Labour Market (representationDT) - CMAs----
#'       filtered_data <-
#'         reactive({
#'           representationDT %>%
#'             filter(
#'               Indicator == filter_var,
#'               VisMin == input$lm_rep_vismin_cma,
#'               Year  %in% input$lm_rep_year_cma,
#'               Degree == input$lm_rep_degree_cma,
#'               Geography %in%input$lm_rep_geography_cma,
#'               Immigration == input$lm_rep_immigration_cma,
#'               Age == input$lm_rep_age_cma,
#'               Sex == input$lm_rep_sex_cma
#'             )
#'         })
#'     } else if (df == "OverQualDT_cma") {
#'       # Participation in the Labour Market (OverQualDT)- CMAs----
#'       filtered_data <-
#'         reactive({
#'           OverQualDT %>%
#'             filter(
#'               VisMin == input$lm_over_vismin_cma,
#'               Year %in% input$lm_over_year_cma,
#'               Location == input$lm_over_location_cma,
#'               Degree == input$lm_over_degree_cma,
#'               Geography %in%  input$lm_over_geography_cma,
#'               Immigration == input$lm_over_immigration_cma,
#'               Age == input$lm_over_age_cma,
#'               Sex == input$lm_over_sex_cma,
#'               Language == input$lm_over_language_cma
#'             )
#'         })
#'     } else if (df == "youthDT_cma") {
#'       # Participation in the Labour Market (youthDT)- CMAs ----
#'       filtered_data <-
#'         reactive({
#'           youthDT %>%
#'             filter(
#'               VisMin == input$lm_youth_vismin_cma,
#'               Year  %in% input$lm_youth_year_cma,
#'               Geography %in%  input$lm_youth_geography_cma,
#'               Immigration == input$lm_youth_immigration_cma,
#'               Age == input$lm_youth_age_cma,
#'               Sex == input$lm_youth_sex_cma,
#'               Language == input$lm_youth_language_cma
#'             )
#'         })
#'     } else if (df == "civicDT_cma") {      
#'       # Civic engagement and political participation (civicDT) - CMAs ----
#'       filtered_data <-
#'         reactive({
#'           civicDT %>%
#'             filter(
#'               Indicator == filter_var,
#'               VisMin == input$civic_vismin_cma,
#'               Geography %in% input$civic_geography_cma,
#'               Confidence == input$civic_conf_interval_cma,
#'               char_type == input$civic_sociodem_cma,
#'               (Characteristic == input$civic_age_cma |
#'                  Characteristic == input$civic_sex_cma |
#'                  Characteristic == input$civic_immigration_cma |
#'                  Characteristic == input$civic_generation_cma |
#'                  Characteristic == input$civic_language_cma |
#'                  Characteristic == input$civic_education_cma
#'               )
#'             )
#'         })
#'     } else if (df == "civicDT2_cma") {
#'       # Civic engagement and political participation (civicDT2)- CMAs ----
#'       filtered_data <-
#'         reactive({
#'           civicDT2 %>%
#'             filter(
#'               Indicator == filter_var,
#'               VisMin == input$civic2_vismin_cma,
#'               Geography %in% input$civic2_geography_cma,
#'               Confidence == input$civic2_conf_interval_cma,
#'               char_type == input$civic2_sociodem_cma,
#'               (Characteristic == input$civic2_age_cma |
#'                  Characteristic == input$civic2_sex_cma |
#'                  Characteristic == input$civic2_immigration_cma |
#'                  Characteristic == input$civic2_generation_cma |
#'                  Characteristic == input$civic2_language_cma |
#'                  Characteristic == input$civic2_education_cma
#'               )
#'             )
#'         })
#'     }
#'      else if (df == "representationDT_cma2") {
#'       # Representation in decision-making positions ----
#'       filtered_data <-
#'         reactive({
#'           representationDT %>%
#'             filter(
#'               Indicator == filter_var,
#'               VisMin ==  input$rep_vismin_cma,
#'               Year %in% input$rep_year_cma,
#'               Degree == input$rep_degree_cma,
#'               Geography %in%input$rep_geography_cma,
#'               Immigration == input$rep_immigration_cma,
#'               Sex == input$rep_sex_cma,
#'               Age == input$rep_age_cma
#'             )
#'         }) 
#'       }
#'     else if (df == "educationDT_cma") {
#'       # Education training and skills ----
#'       filtered_data <-
#'         reactive({
#'           educationDT %>%
#'             filter(
#'               Indicator == filter_var,
#'               VisMin == input$education_vismin_cma,
#'               Year %in% input$education_year_cma,
#'               Language == input$education_language_cma,
#'               Geography %in% input$education_geography_cma,
#'               Immigration == input$education_immigration_cma,
#'               Age == input$education_age_cma,
#'               Sex == input$education_sex_cma
#'             )
#'         })
#'     }
#'     else if (df == "belongingDT_cma") {
#'       # Social connections and personal networks ----
#'       filtered_data <-
#'         reactive({
#'           belongingDT %>%
#'             filter(
#'               Indicator == filter_var,
#'               VisMin == input$belonging_vismin_cma,
#'               Geography %in% input$belonging_geography_cma,
#'               Confidence == input$belonging_conf_interval_cma,
#'               char_type == input$belonging_sociodem_cma,
#'               (Characteristic == input$belonging_age_cma |
#'                  Characteristic == input$public_income_social_gender_cma |
#'                  Characteristic == input$belonging_immigration_cma |
#'                  Characteristic == input$belonging_generation_cma |
#'                  Characteristic == input$belonging_language_cma |
#'                  Characteristic == input$belonging_education_cma
#'               )
#'             )
#'         })
#'     }
#'     else if (df == "basicDT_cma") {
#'       # Basic needs and housing  ----
#'       filtered_data <-
#'         reactive({
#'           healthDT %>%
#'             filter(
#'               Indicator == filter_var,
#'               #VisMin == input$basic_vismin_cma,
#'               Year %in%  input$basic_year_cma,
#'               Geography  %in% input$basic_geography_cma,
#'               Confidence == input$basic_conf_interval_cma,
#'               char_type == input$basic_sociodem_cma,
#'               (Characteristic == input$basic_vismin_cma |
#'                  Characteristic == input$basic_sex_cma |
#'                  Characteristic == input$basic_immigration_cma
#'               )
#'             )
#'         })
#'     }
#'      else if (df == "healthDT_cma") {
#'       # Health and well being ----
#'       filtered_data <-
#'         reactive({
#'           healthDT %>%
#'             filter(
#'               Indicator == filter_var,
#'               Year  %in% input$health_year_cma,
#'               Geography %in% input$health_geography_cma,
#'               Confidence == input$health_conf_interval_cma,
#'               char_type == input$health_sociodem_cma,
#'               (Characteristic  == input$health_vismin_cma|
#'                  Characteristic == input$health_sex_cma |
#'                  Characteristic == input$health_immigration_cma
#'               )
#'             )
#'         })
#'     }
#'     else if (df == "confidenceDT_cma") {
#'       # Public services and institutions ----
#'       filtered_data <-
#'         reactive({
#'           confidenceDT %>%
#'             filter(
#'               Indicator == filter_var,
#'               VisMin == input$public_vismin_cma,
#'               Geography %in% input$public_geography_cma,
#'               Confidence == input$public_conf_interval_cma,
#'               char_type == input$public_sociodem_cma,
#'               (Characteristic == input$public_age_cma |
#'                  Characteristic == input$public_sex_cma |
#'                  Characteristic == input$public_immigration_cma |
#'                  Characteristic == input$public_generation_cma |
#'                  Characteristic == input$public_language_cma |
#'                  Characteristic == input$public_education_cma
#'               )
#'             )
#'         })
#'     }
#'     else if (df == "incomeDT_cma") {
#'       # Income and wealth (incomeDT)- CMAs----
#'       filtered_data <-
#'         reactive({
#'           incomeDT %>%
#'             filter(
#'               Indicator == filter_var,
#'               VisMin ==  input$lm_income_vismin_cma,
#'               Year  %in% input$lm_income_year_cma,
#'               Degree == input$lm_income_degree_cma,
#'               Geography %in% input$lm_income_geography_cma,
#'               Immigration == input$lm_income_immigration_cma,
#'               Age == input$lm_income_age_cma,
#'               Sex == input$lm_income_sex_cma
#'             )
#'         })
#'     }
#' 
#'     renderPlotly(ggplotly({
#'       ggplot(filtered_data()) +
#'         geom_bar(
#'           stat = "identity",
#'           width = 0.4,
#'           #height = 0.5,
#'           position = position_dodge(width = 0.5),
#'           aes(
#'             x = Year,
#'             y = Value,
#'             colour = Geography,
#'             fill = Geography,
#'             text = paste0(
#'               "Geography: ",
#'               Geography,
#'               "<br>",
#'               "Value: ",
#'               format(Value, big.mark = ","),
#'               "<br>",
#'               "Year: ",
#'               Year
#'             )
#'           )
#'         ) + coord_flip()+
#'         theme_minimal() +
#'         scale_y_continuous(labels = comma) +
#'         labs(
#'           x = "Year",
#'           y = "Value",
#'           # orientation = "h",
#'           colour = "Geography",
#'           fill = "Geography"
#'         )
#'     }, tooltip = "text"))
#'   }
#' 
#'   #'NOTE [This chart doesn't follow the same x-axis as the other charts]
#'   func_plot2_discrimination <- function(filter_var){
#'     # Discrimination and victimization ----
#'     filtered_data <-
#'       reactive({
#'         discriminationDT %>%
#'           filter(
#'             ind == filter_var,
#'             VisMin ==  input$discrimination_vismin_cma,
#'             Geography %in% input$discrimination_geography_cma,
#'             Confidence == input$discrimination_conf_interval_cma,
#'             char_type == input$discrimination_sociodem_cma,
#'             (Characteristic == input$discrimination_age_cma |
#'                Characteristic == input$discrimination_sex_cma |
#'                Characteristic == input$discrimination_immigration_cma |
#'                Characteristic == input$discrimination_generation_cma |
#'                Characteristic == input$discrimination_language_cma |
#'                Characteristic == input$discrimination_education_cma
#'             )
#'           )
#'       })
#' 
#'     renderPlotly(ggplotly({
#'       ggplot(filtered_data()) +
#'         geom_bar(
#'           stat = "identity",
#'           width = 0.4,
#'           position = position_dodge(width = 0.5),
#'           aes(
#'             x = before_since,
#'             y = Value,
#'             colour = Geography,
#'             fill = Geography,
#'             text = paste0(
#'               "Geography:",
#'               Geography,
#'               "<br>",
#'               "Value: ",
#'               format(Value, big.mark = ","),
#'               "<br>",
#'               "Reference Period in Relation to the covid-19 Pandemic: ",
#'               before_since
#'             )
#'           )
#'         ) +
#'         theme_minimal() +
#'         scale_y_continuous(labels = comma) +
#'         labs(
#'           x = "Reference Period in Relation to the covid-19 Pandemic",
#'           y = "Value",
#'           colour = "Geography)",
#'           fill = "Geography"
#'         )
#'     }, tooltip = "text"))
#'     }
#'   ### Plots CMAs----
#'   #### 1. Participation in the Labour Market ----
#'   ##### 1.1. Working-age population in the labour force (participation rate) ----
#'   output$plot_cma_lm_1 <-
#'     func_plot_2(df = "rateDT_cma",
#'                 filter_var = unique(as.character(rateDT$Indicator))[1])
#' 
#'   ##### 1.2. Working-age population in employment (employment rate) ----
#'   output$plot_cma_lm_2 <-
#'     func_plot_2(df = "rateDT_cma",
#'                 filter_var = unique(as.character(rateDT$Indicator))[2])
#' 
#'   ##### 1.3. Working-age population in employment (unemployment rate) ----
#'   output$plot_cma_lm_3 <-
#'     func_plot_2(df = "rateDT_cma",
#'                 filter_var = unique(as.character(rateDT$Indicator))[3])
#' 
#'   ##### 1.4. Workers working mainly full-time weeks in the previous year (Population in full-time employment) ----
#'   output$plot_cma_lm_4 <-
#'     func_plot_2(df = "rateDT_cma",
#'                 filter_var = unique(as.character(rateDT$Indicator))[4])
#'   ##### 1.5. Self-employed workers in the labour force (unincorporated) ----
#'   output$plot_cma_lm_5 <-
#'     func_plot_2(df = "representationDT_cma",
#'                 filter_var = unique(as.character(representationDT$Indicator))[4])
#'   
#'   ##### 1.6. Overqualified workers with a university degree ----
#'   output$plot_cma_lm_6 <-
#'     func_plot_2(df = "OverQualDT_cma")
#'   
#'   ##### 1.7. Youth not in employment, education or training (NEET) ----
#'   output$plot_cma_lm_7 <-
#'     func_plot_2(df = "youthDT_cma")
#'   #### 2. Civic engagement and political participation ----
#'   ##### 2.1. Percent of the population members of at least one civic group or organization ----
#'   output$plot_cma_civic_1 <-
#'     func_plot_2(df = "civicDT_cma",
#'                 filter_var = unique(as.character(civicDT$Indicator))[1])
#'   
#'   ##### 2.2. Percent of the population members in a sports or recreational organization ----
#'   output$plot_cma_civic_2 <-
#'     func_plot_2(df = "civicDT_cma",
#'                 filter_var = unique(as.character(civicDT$Indicator))[2])
#'   
#'   ##### 2.3. Percent of the population members in a cultural, educational or hobby organization ----
#'   output$plot_cma_civic_3 <-
#'     func_plot_2(df = "civicDT_cma",
#'                 filter_var = unique(as.character(civicDT$Indicator))[3])
#'   
#'   ##### 2.4. Percent of the population members in union or professional association ----
#'   output$plot_cma_civic_4 <-
#'     func_plot_2(df = "civicDT_cma",
#'                 filter_var = unique(as.character(civicDT$Indicator))[4])
#'   
#'   ##### 2.5. Percent of the population members in a political party or group ----
#'   output$plot_cma_civic_5 <-
#'     func_plot_2(df = "civicDT_cma",
#'                 filter_var = unique(as.character(civicDT$Indicator))[5])
#'   
#'   ##### 2.6. Percent of the population members in a religious-affiliated group ----
#'   output$plot_cma_civic_6 <-
#'     func_plot_2(df = "civicDT_cma",
#'                 filter_var = unique(as.character(civicDT$Indicator))[6])
#'   
#'   ##### 2.7. Percent of the population members in a school group, neighbourhood, civic or community association ----
#'   output$plot_cma_civic_7 <-
#'     func_plot_2(df = "civicDT_cma",
#'                 filter_var = unique(as.character(civicDT$Indicator))[7])
#'   
#'   ##### 2.8. Percent of the population members in a humanitarian or charitable organization or service club ----
#'   output$plot_cma_civic_8 <-
#'     func_plot_2(df = "civicDT_cma",
#'                 filter_var = unique(as.character(civicDT$Indicator))[8])
#'   
#'   ##### 2.9. Percent of the population members in a seniors' group ----
#'   output$plot_cma_civic_9 <-
#'     func_plot_2(df = "civicDT_cma",
#'                 filter_var = unique(as.character(civicDT$Indicator))[9])
#'   
#'   ##### 2.10. Percent of the population members in a youth organization ----
#'   output$plot_cma_civic_10 <-
#'     func_plot_2(df = "civicDT_cma",
#'                 filter_var = unique(as.character(civicDT$Indicator))[10])
#'   
#'   ##### 2.11. Percent of the population members in an immigrant or ethnic association or club ----
#'   output$plot_cma_civic_11 <-
#'     func_plot_2(df = "civicDT_cma",
#'                 filter_var = unique(as.character(civicDT$Indicator))[11])
#'   
#'   ##### 2.12. Percent of the population members in an environmental group ----
#'   output$plot_cma_civic_12 <-
#'     func_plot_2(df = "civicDT_cma",
#'                 filter_var = unique(as.character(civicDT$Indicator))[12])
#'   
#'   ##### 2.13. Percent of the population engaged in political activities ----
#'   output$plot_cma_civic_13 <-
#'     func_plot_2(df = "civicDT_cma",
#'                 filter_var = unique(as.character(civicDT$Indicator))[13])
#'   
#'   ##### 2.14. Percent of the population voting in the last federal election ----
#'   output$plot_cma_civic_14 <-
#'     func_plot_2(df = "civicDT2_cma",
#'                 filter_var = unique(as.character(civicDT2$Indicator))[1])
#'   
#'   ##### 2.15. Percent of the population voting in the last provincial election ----
#'   output$plot_cma_civic_15 <-
#'     func_plot_2(df = "civicDT2_cma",
#'                 filter_var = unique(as.character(civicDT2$Indicator))[2])
#'   
#'   ##### 2.16. Percent of the population voting in the last municipal election ----
#'   output$plot_cma_civic_16 <-
#'     func_plot_2(df = "civicDT2_cma",
#'                 filter_var = unique(as.character(civicDT2$Indicator))[3])
#'   #### 3. Representation in decision-making positions ----
#'   ##### 3.1. Percent of workers in all management occupations ----
#'   output$plot_cma_rep_1 <-
#'     func_plot_2(df = "representationDT_cma2",
#'                 filter_var = unique(as.character(representationDT$Indicator))[1])
#'   
#'   ##### 3.2. Percent of workers in senior management occupations ----
#'   output$plot_cma_rep_2 <-
#'     func_plot_2(df = "representationDT_cma2",
#'                 filter_var = unique(as.character(representationDT$Indicator))[2])
#'   
#'   #'NOTE [Not sure what's happening with this middle manager section]
#'   ##### 3.3. Percent of workers in specialized middle management occupations ----
#'   #### 3.4. Percent of workers in other middle management occupations ----
#'   output$plot_cma_rep_4 <-
#'     func_plot_2(df = "representationDT_cma2",
#'                 filter_var = unique(as.character(representationDT$Indicator))[4])
#'   
#'   #### 4. Basic needs and housing ----
#'   ##### 4.1. Percent of the population living in a dwelling owned by one member of the household ----
#'   # output$plot_cma_basic_1 <-
#'   #   func_plot_2(df = "basicDT_cma",
#'   #             filter_var = unique(healthDT$Indicator)[])
#'   
#'   ##### 4.2. Percent of the population living in core need household ----
#'   # output$plot_cma_basic_2 <-
#'   #   func_plot_2(df = "basicDT_cma",
#'   #             filter_var = unique(healthDT$Indicator)[])
#'   
#'   ##### 4.3. Percent of the population living in suitable housing ----
#'   # output$plot_cma_basic_3 <-
#'   #   func_plot_2(df = "basicDT_cma",
#'   #             filter_var = unique(healthDT$Indicator)[])
#'   
#'   ##### 4.4. Percent of the population living in an affordable housing ----
#'   # output$plot_cma_basic_4 <-
#'   #   func_plot_2(df = "basicDT_cma",
#'   #             filter_var = unique(healthDT$Indicator)[])
#'   
#'   ##### 4.5. Percent of the population living in a food-secure household ----
#'   output$plot_cma_basic_5 <-
#'     func_plot_2(df = "basicDT_cma",
#'                 filter_var = unique(as.character(healthDT$Indicator))[14])
#'   
#'   ##### 4.6. Percent of the population living in a household with marginal food security ----
#'   output$plot_cma_basic_6 <-
#'     func_plot_2(df = "basicDT_cma",
#'                 filter_var = unique(as.character(healthDT$Indicator))[15])
#'   
#'   ##### 4.7. Percent of the population living in a food-insecure household, moderate or severe ----
#'   output$plot_cma_basic_7 <-
#'     func_plot_2(df = "basicDT_cma",
#'                 filter_var = unique(as.character(healthDT$Indicator))[16])
#'   
#'   ##### 4.8. Percent of the population living in a household with moderate food insecurity ----
#'   output$plot_cma_basic_8 <-
#'     func_plot_2(df = "basicDT_cma",
#'                 filter_var = unique(as.character(healthDT$Indicator))[17])
#'   
#'   ##### 4.9. Percent of the population living in a household with severe food insecurity ----
#'   output$plot_cma_basic_9 <-
#'     func_plot_2(df = "basicDT_cma",
#'                 filter_var = unique(as.character(healthDT$Indicator))[18])
#'   #### 5. Local community ----
#'   #'NOTE [TBD]
#'   
#'   #### 6. Health and wellbeing ----
#'   ##### 6.1. Percent of the population reporting very good or excellent general health ----
#'   output$plot_cma_health_1 <-
#'     func_plot_2(df = "healthDT_cma",
#'                 filter_var = unique(as.character(healthDT$Indicator))[1])
#'   
#'   ##### 6.2. Percent of the population reporting fair or poor general health ----
#'   output$plot_cma_health_2 <-
#'     func_plot_2(df = "healthDT_cma",
#'                 filter_var = unique(as.character(healthDT$Indicator))[2])
#'   
#'   ##### 6.3. Percent of the population reporting very good or excellent mental health ----
#'   output$plot_cma_health_3 <-
#'     func_plot_2(df = "healthDT_cma",
#'                 filter_var = unique(as.character(healthDT$Indicator))[3])
#'   
#'   ##### 6.4. Percent of the population reporting fair or poor mental health ----
#'   output$plot_cma_health_4 <-
#'     func_plot_2(df = "healthDT_cma",
#'                 filter_var = unique(as.character(healthDT$Indicator))[4])
#'   
#'   ##### 6.5. Percent of the population reporting their life stressful ----
#'   output$plot_cma_health_5 <-
#'     func_plot_2(df = "healthDT_cma",
#'                 filter_var = unique(as.character(healthDT$Indicator))[5])
#'   
#'   ##### 6.6. Percent of the population reporting life satisfaction, satisfied or very satisfied ----
#'   output$plot_cma_health_6 <-
#'     func_plot_2(df = "healthDT_cma",
#'                 filter_var = unique(as.character(healthDT$Indicator))[6])
#'   
#'   # ##### 6.7. Percent of the population reporting having a regular healthcare providers ----
#'   output$plot_cma_health_7 <-
#'     func_plot_2(df = "healthDT_cma",
#'                 filter_var = unique(as.character(healthDT$Indicator)[7]))
#'   ##### 6.8. Percent of the population reporting no need for mental health care ----
#'   output$plot_cma_health_8 <-
#'     func_plot_2(df = "healthDT_cma",
#'                 filter_var = unique(as.character(healthDT$Indicator)[8]))
#'   ##### 6.9. Percent of the population reporting all needs met for mental health care ----
#'   output$plot_cma_health_9 <-
#'     func_plot_2(df = "healthDT_cma",
#'                 filter_var = unique(as.character(healthDT$Indicator)[9]))
#'   ##### 6.10. Percent of the population reporting needs partially met for mental health care ----
#'   output$plot_cma_health_10 <-
#'     func_plot_2(df = "healthDT_cma",
#'                 filter_var = unique(as.character(healthDT$Indicator)[10]))
#'   ##### 6.11. Percent of the population reporting needs partially met or needs not met for mental health care ----
#'   output$plot_cma_health_11 <-
#'     func_plot_2(df = "healthDT_cma",
#'                 filter_var = unique(as.character(healthDT$Indicator)[11]))
#'   ##### 6.12. Percent of the population reporting needs not met for mental health cares ----
#'   output$plot_cma_health_12 <-
#'     func_plot_2(df = "healthDT_cma",
#'                 filter_var = unique(as.character(healthDT$Indicator)[12]))
#'   ##### 6.13. Percent of the population reporting unmet health care needs ----
#'   output$plot_cma_health_13 <-
#'     func_plot_2(df = "healthDT_cma",
#'                 filter_var = unique(as.character(healthDT$Indicator)[13]))
#'   
#'   #### 7. Public services and institutions ----
#'   #'NOTE [The indicators weren't in the same order as the indicators for confidenceDT]
#'   ##### 7.1. Population expressing confidence in Federal Parliament ----
#'   output$plot_cma_public_1 <-
#'     func_plot_2(df = "confidenceDT_cma",
#'                 filter_var = unique(as.character(confidenceDT$Indicator))[4])
#'   
#'   ##### 7.2. Population expressing Confidence in the Canadian media ----
#'   output$plot_cma_public_2 <-
#'     func_plot_2(df = "confidenceDT_cma",
#'                 filter_var = unique(as.character(confidenceDT$Indicator))[8])
#'   
#'   ##### 7.3. Population expressing confidence in the school system ----
#'   output$plot_cma_public_3 <-
#'     func_plot_2(df = "confidenceDT_cma",
#'                 filter_var = unique(as.character(confidenceDT$Indicator))[3])
#'   
#'   ##### 7.4. Population expressing confidence in the justice system, courts ----
#'   output$plot_cma_public_4 <-
#'     func_plot_2(df = "confidenceDT_cma",
#'                 filter_var = unique(as.character(confidenceDT$Indicator))[2])
#'   
#'   ##### 7.5. Population expressing confidence in the police ----
#'   output$plot_cma_public_5 <-
#'     func_plot_2(df = "confidenceDT_cma",
#'                 filter_var = unique(as.character(confidenceDT$Indicator))[1])
#'   
#'   ##### 7.6. Population expressing confidence in major corporations ----
#'   output$plot_cma_public_6 <-
#'     func_plot_2(df = "confidenceDT_cma",
#'                 filter_var = unique(as.character(confidenceDT$Indicator))[6])
#'   
#'   ##### 7.7. Population expressing confidence in merchants and business people ----
#'   output$plot_cma_public_7 <-
#'     func_plot_2(df = "confidenceDT_cma",
#'                 filter_var = unique(as.character(confidenceDT$Indicator))[7])
#'   
#'   ##### 7.8. Population expressing confidence in banks ----
#'   output$plot_cma_public_8 <-
#'     func_plot_2(df = "confidenceDT_cma",
#'                 filter_var = unique(as.character(confidenceDT$Indicator))[5])
#'   
#'   #### 11. Income and wealth ----
#'   #'NOTE [TBD]
#'   
#'   #### 8. Education training and skills ----
#'   ##### 8.1. Population with no certificate, diploma or degree ----
#'   output$plot_cma_education_1 <-
#'     func_plot_2(df = "educationDT_cma",
#'                 filter_var = unique(as.character(educationDT$Indicator)[1]))
#'   ##### 8.2. Population with high school diploma or equivalency certificate ----
#'   output$plot_cma_education_2 <-
#'     func_plot_2(df = "educationDT_cma",
#'                 filter_var = unique(as.character(educationDT$Indicator)[2]))
#'   ##### 8.3.  Population with postsecondary certificate or diploma below bachelor level ----
#'   output$plot_cma_education_3 <-
#'     func_plot_2(df = "educationDT_cma",
#'                 filter_var = unique(as.character(educationDT$Indicator)[3]))
#'   ##### 8.4.  Population with university certificate or diploma above bachelor level ----
#'   output$plot_cma_education_4 <-
#'     func_plot_2(df = "educationDT_cma",
#'                 filter_var = unique(as.character(educationDT$Indicator)[4]))
#'   ##### 8.5.  Population with bachelor's degree----
#'   output$plot_cma_education_5 <-
#'     func_plot_2(df = "educationDT_cma",
#'                 filter_var = unique(as.character(educationDT$Indicator)[5]))
#'   ##### 8.6.  Population with university certificate or diploma or degree at bachelor level or above ----
#'   output$plot_cma_education_6 <-
#'     func_plot_2(df = "educationDT_cma",
#'                 filter_var = unique(as.character(educationDT$Indicator)[6]))
#'   #### 5. Local community ----
#'   #'NOTE [TBD]
#'   #### 9. Social connections and personnal networks ----
#'   ##### 9.1. Percent of the population living alone ----
#'   # output$plot_cma_social_1 <-
#'   #   func_plot_2(df = "belongingDT_cma",
#'   #             filter_var = unique(belongingDT$Indicator)[])
#'   
#'   ##### 9.2. Median size of a personal local network with close ties ----
#'   # output$plot_cma_social_2 <-
#'   #   func_plot_2(df = "belongingDT_cma",
#'   #             filter_var = unique(belongingDT$Indicator)[])
#'   
#'   ##### 9.3. Average size of a local personal network with close ties ----
#'   # output$plot_cma_social_3 <-
#'   #   func_plot_2(df = "belongingDT_cma",
#'   #             filter_var = unique(belongingDT$Indicator)[])
#'   
#'   ##### 9.4. Percent of the population with a personal close-ties network of 10 or more people ----
#'   # output$plot_cma_social_4 <-
#'   #   func_plot_2(df = "belongingDT_cma",
#'   #             filter_var = unique(belongingDT$Indicator)[])
#'   
#'   ##### 9.5. Percent of the population with a personal close-ties network of 5 or more relatives ----
#'   # output$plot_cma_social_5 <-
#'   #   func_plot_2(df = "belongingDT_cma",
#'   #             filter_var = unique(belongingDT$Indicator)[])
#'   
#'   ##### 9.6. Percent of the population with a personal close-ties network of 5 or more friends ----
#'   # output$plot_cma_social_6 <-
#'   #   func_plot_2(df = "belongingDT_cma",
#'   #             filter_var = unique(belongingDT$Indicator)[])
#'   
#'   ##### 9.7. Percent of the population with no personal network with weak ties ----
#'   # output$plot_cma_social_7<-
#'   #   func_plot_2(df = "belongingDT_cma",
#'   #             filter_var = unique(belongingDT$Indicator)[])
#'   
#'   ##### 9.8. Percent of the population with a personal weak-ties network of 1 to 19 people ----
#'   # output$plot_cma_social_8 <-
#'   #   func_plot_2(df = "belongingDT_cma",
#'   #             filter_var = unique(belongingDT$Indicator)[])
#'   
#'   ##### 9.9. Percent of the population with a personal weak-ties network of 20 or more people ----
#'   # output$plot_cma_social_9 <-
#'   #   func_plot_2(df = "belongingDT_cma",
#'   #             filter_var = unique(belongingDT$Indicator)[])
#'   
#'   ##### 9.10. Percent of the population with a personal ethnically-diverse network ----
#'   # output$plot_cma_social_10 <-
#'   #   func_plot_2(df = "belongingDT_cma",
#'   #             filter_var = unique(belongingDT$Indicator)[])
#'   #### 9.11. Population reporting that most people can be trusted ----
#'   output$plot_cma_social_11 <-
#'     func_plot_2(df = "belongingDT_cma",
#'                 filter_var = unique(belongingDT$Indicator)[1])
#'   #### 9.12. Population reporting strong sense of belonging to their local community ----
#'   output$plot_cma_social_12 <-
#'     func_plot_2(df = "belongingDT_cma",
#'                 filter_var = unique(belongingDT$Indicator)[2])
#'   #### 9.13. Population reporting strong sense of belonging to their town or city ----
#'   output$plot_cma_social_13 <-
#'     func_plot_2(df = "belongingDT_cma",
#'                 filter_var = unique(belongingDT$Indicator)[3])
#'   #### 9.14. Population reporting strong sense of belonging to their province ----
#'   output$plot_cma_social_14 <-
#'     func_plot_2(df = "belongingDT_cma",
#'                 filter_var = unique(belongingDT$Indicator)[4])
#'   #### 9.15. Population reporting strong sense of belonging to Canada ----
#'   output$plot_cma_social_15 <-
#'     func_plot_2(df = "belongingDT_cma",
#'                 filter_var = unique(belongingDT$Indicator)[5])
#'   
#'   #### 10. Discrimination and victimization ----
#'   ##### 10.1. Experience(s) of discrimination ----
#'   output$plot_cma_discrimination_1 <-
#'     func_plot2_discrimination(filter_var = unique(as.character(discriminationDT$ind))[1])
#'   
#'   # ##### 10.2. Experience(s) of discrimination based on ethnicity or culture ----
#'   output$plot_cma_discrimination_2 <-
#'     func_plot2_discrimination(filter_var = unique(as.character(discriminationDT$ind))[2])
#'   
#'   # ##### 10.3. Experience(s) of discrimination based on race or colour ----
#'   output$plot_cma_discrimination_3 <-
#'     func_plot2_discrimination(filter_var = unique(as.character(discriminationDT$ind))[3])
#'   
#'   # ##### 10.4. Experience(s) of discrimination based on religion ----
#'   output$plot_cma_discrimination_4 <-
#'     func_plot2_discrimination(filter_var = unique(as.character(discriminationDT$ind))[4])
#'   
#'   # ##### 10.5. Experience(s) of discrimination based on language ----
#'   output$plot_cma_discrimination_5 <-
#'     func_plot2_discrimination(filter_var = unique(as.character(discriminationDT$ind))[5])
#'   
#'   # ##### 10.6. Discrimination at work or when applying for a job or promotion ----
#'   output$plot_cma_discrimination_6 <-
#'     func_plot2_discrimination(filter_var = unique(as.character(discriminationDT$ind))[6])
#'   
#'   # ##### 10.7. Discrimination when dealing with the police ----
#'   output$plot_cma_discrimination_7<-
#'     func_plot2_discrimination(filter_var = unique(as.character(discriminationDT$ind))[7])
#'   
#'   # ##### 10.8. Discrimination when in a store, bank or restaurant ----
#'   output$plot_cma_discrimination_8 <-
#'     func_plot2_discrimination(filter_var = unique(as.character(discriminationDT$ind))[8])
#'   
#'   # ##### 10.9. Discrimination when attending school or classes ----
#'   output$plot_cma_discrimination_9 <-
#'     func_plot2_discrimination(filter_var = unique(as.character(discriminationDT$ind))[9])
#'   ##### 11.1. Average employment income of the population ----
#'   output$plot_cma_inc_1 <-
#'     func_plot_2(df = "incomeDT_cma",
#'                 filter_var = unique(as.character(incomeDT$Indicator))[1])
#' 
#'  ##### 11.2. Average weekly wage of paid employees ----
#'   output$plot_cma_inc_2 <-
#'     func_plot_2(df = "incomeDT_cma",
#'                 filter_var = unique(as.character(incomeDT$Indicator))[2])
#' 
#'   #'NOTE [Here is where you should add the next tab // use what's in Tab 1 as a reference -- you might need to reconfigure the function to the breakdown that's relevant to your new tab]
#'   # output$map <- renderPlotly(
#'   #   ggplotly(
#'   #       ggplot(youthDT) +
#'   #       geom_sf(
#'   #         aes(
#'   #           fill = Geography,geometry = geometry)) +
#'   #       ggtitle("Labour Force Status by Groups Designated as Visible Minorities")
#'   #   )
#'   #   
#'   # )
}

# All together now ----
shinyApp(ui, server)
