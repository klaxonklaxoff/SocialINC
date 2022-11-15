source(file = "2_example_ui.R", encoding = "UTF-8")

# Server // determines the data underlying the UI ----
server <- function(input, output, session) {
  ## Themes and Definitions of Indicators ----
  ### Table 1 ----
  #'NOTE [Makes is so the table reacts to the user's selection]
  choose_def <-
    reactive({
      req(input$theme_0)
      template_2 %>%
        filter(Theme %in% input$theme_0)
    })

  #'NOTE [Table is adjusted depending on the theme the user selects]
  output$def_table <-
    renderDataTable({
      choose_def() %>%
        select(-c(Theme,Year)) # Removed theme column because it's not needed in the table
    })

  ## Tab 1: Groups Designated as Visible Minorities ----
  ### Filters ----
  #'NOTE [This makes it so the theme filter affects the indicator filter]
  observe(
    updateSelectizeInput(
      session = session,
      inputId = "indicator_1",
      #'NOTE [Not showing the following indicators until they are ready]
      choices = as.character(unique(template$Indicator[template$Theme == input$theme_1 &
                                                         !template$Indicator %in% c(

                                                           "Knowledge of official languages, English only",
                                                           "Knowledge of official languages, French only",
                                                           "Knowledge of official languages, English and French",
                                                           "Knowledge of official languages, neither English nor French",
                                                          
                                                           "Population living in a dwelling owned by one member of the household",
                                                           "Population living in core need housing",
                                                           "Population living in suitable housing",
                                                           "Population living in an affordable housing",
                                                           
                                                           "Satisfied with personal safety from crime",
                                                           
                                                           "Perceiving the local police force as doing a good job of enforcing the laws",
                                                           "Perceiving the local police force as doing a good job of promptly responding to calls",
                                                           "Perceiving the local police force as doing a good job of being approachable and easy to talk to",
                                                           "Perceiving the local police force as doing a good job of supplying information to the public on ways to prevent crime",
                                                           "Perceiving the local police force as doing a good job of ensuring the safety of the citizens in their area",
                                                           "Perceiving the local police force as doing a good job of treating people fairly",

                                                           "Average total household income, adjusted for the number of persons",
                                                           "Prevalence of MBM low income status",
                                                           "Prevalence of low income (LIM-BT)",
                                                           "Prevalence of low income (LIM-AT)",
                                                           "Difficulty in meeting household financial needs",
                                                           "Ease in meeting household financial needs",

                                                           "Population living alone",
                                                           "Having no one in the local personal network (close relatives and close friends)",
                                                           "Having 1 to 14 people in the local personal network (close relatives and close friends)",
                                                           "Having 15 or more people in the local personal network (close relatives and close friends)",
                                                           "Feeling close to 4 relatives or less",
                                                           "Feeling close to 5 relatives or more", 
                                                           "Having 4 close friends or less",
                                                           "Feeling close to 5 relatives or more",  
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
    }else if (df == "basicDT_public") {
      # Public services and institutions (healthDT)----
      filtered_data <-
        reactive({
          basicDT %>%
            filter(
              Indicator == filter_var,
              VisMin %in% input$health_public_vismin,
              Year  %in% input$health_public_year,
              Geography == input$health_public_geography,
              Confidence == input$health_conf_public_interval,
              char_type == input$health_public_sociodem,
              (Characteristic == input$health_public_age |
                 Characteristic == input$health_public_sex |
                 Characteristic == input$health_public_immigration
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
        #label = Value
        )) +
        geom_col(
          width = 0.7, 
          position = position_dodge(width = 0.9)
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
      theme(legend.position = "right", axis.text.x = element_text(angle = 45,vjust = 0.5,hjust = 1)) +   
        #theme_light() +                                                   
        labs(
          x = "Visible Minority group(s)",
          y = "Percent",
          colour = "Visible Minority group(s)",
          fill = "Year"
        ) +
        scale_y_continuous(labels = comma) +
       geom_text(
              aes(label = Value),
                  color = "black",
                  # vjust = 0,
                  # hjust = 0,
                  size  = 2,
                  angle = 45,
                  font = "bold",
                  position = position_dodge(width = 0.9)
              )

      }, 
    #tooltip = "text"
    ))
  }
  func_plot_income <- function(filter_var){
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
  renderPlotly(ggplotly({
    ggplot(filtered_data(),
           aes(
             x = VisMin,
             y = Value,
             #colour = VisMin,
             fill = Year,
             #label = Value
           )) +
      geom_col(
        width = 0.7, 
        position = position_dodge(width = 0.9)
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
      theme(legend.position = "none", axis.text.x = element_text(angle = 45,vjust = 0.5,hjust = 1)) + 
      #theme_minimal() +                                                   
      labs(
        x = "Visible Minority group(s)",
        y = "Dollars",
        colour = "Visible Minority group(s)",
        fill = "Year"
      ) +
      scale_y_continuous(labels = comma) +
      geom_text(
        aes(label = Value),
        color = "black",
        # vjust = 0,
        # hjust = 0,
        size  = 2,
        angle = 90,
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
      ggplot(filtered_data(),aes(x = before_since,
                                 y = Value,
                                 colour = VisMin,
                                 fill = VisMin,
                                 label = Value)) +
        geom_col(
          width = 0.7,
          position = position_dodge(width = 0.9),
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
              "Reference Period in Relation to the COVID-19 Pandemic: ",
              before_since
            )
           )
        ) + 
        geom_text(
          aes(label = Value),
          color = "black",
          vjust = 0,
          hjust = 0,
          size  = 2,
          angle = 90,
          position = position_dodge(width = 0.9)
        ) +
        theme_light() +  
        scale_y_continuous(labels = comma) +
        labs(
          x = "Reference Period in Relation to the COVID-19 Pandemic",
          y = "Percent" 
        )
    }, tooltip = "text"))
  }
  
  func_plot_hate <- function(filter_var){
    # Hate crime ----
    filtered_data <-
      reactive({
        polData %>%
          filter(
            Year %in% input$hate_year,
            Geography == input$hate_geography,
            motivation_type == input$hate_motivation,
            (
              Motivation %in% input$hate_race |
                Motivation %in% input$hate_police
            )
          )
      })
    
    renderPlotly(ggplotly({
      ggplot(filtered_data(), 
             aes(x = Year,
                 y = Value,
                 colour = Motivation,
                 )) +
        geom_line(group = 1) + # you need this when you want to make a line graph
        theme_light() + 
        #legend(title = "Characteristics") +
        scale_y_continuous(labels = comma) +
        labs(
          x = "Year",
          y = "Number",
          colour = "Motivation",
          fill = "Motivation"
        )
    },
    ))
  }
  
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
  output$plot_vm_rep_3 <-
    func_plot_1(df = "representationDT",
              filter_var = unique(as.character(representationDT$Indicator))[3])

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
  #### 7.9. Percent of the population reporting having a regular healthcare providers ----
  output$plot_vm_health_7 <-
    func_plot_1(df = "basicDT_public",
                filter_var = unique(as.character(basicDT$Indicator)[7]))
  #### 7.10. Percent of the population reporting no need for mental health care ----
  output$plot_vm_health_8 <-
    func_plot_1(df = "basicDT_public",
                filter_var = unique(as.character(basicDT$Indicator)[8]))
  #### 7.11. Percent of the population reporting all needs met for mental health care ----
  output$plot_vm_health_9 <-
    func_plot_1(df = "basicDT_public",
                filter_var = unique(as.character(basicDT$Indicator)[9]))
  #### 7.12. Percent of the population reporting needs partially met for mental health care ----
  output$plot_vm_health_10 <-
    func_plot_1(df = "basicDT_public",
                filter_var = unique(as.character(basicDT$Indicator)[10]))
  #### 7.13. Percent of the population reporting needs partially met or needs not met for mental health care ----
  output$plot_vm_health_11 <-
    func_plot_1(df = "basicDT_public",
                filter_var = unique(as.character(basicDT$Indicator)[11]))
  #### 7.14. Percent of the population reporting needs not met for mental health cares ----
  output$plot_vm_health_12 <-
    func_plot_1(df = "basicDT_public",
                filter_var = unique(as.character(basicDT$Indicator)[12]))
  #### 7.15. Percent of the population reporting unmet health care needs ----
  output$plot_vm_health_13 <-
    func_plot_1(df = "basicDT_public",
                filter_var = unique(as.character(basicDT$Indicator)[13]))

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
    func_plot_income(filter_var = unique(as.character(incomeDT$Indicator))[1])
  ##### 11.2. Average weekly wage of paid employees ----
  output$plot_vm_inc_2 <-
    func_plot_income(filter_var = unique(as.character(incomeDT$Indicator))[2])
  # ##### 10.9. Hate crime ----
  output$plot_vm_hate_crime <- func_plot_hate(filter_var = unique(as.character(polData$ind))[1])

  #'NOTE [Here is where you should add the next tab // use what's in Tab 1 as a reference -- you might need to reconfigure the function to the breakdown that's relevant to your new tab]
  ## Tab 2: Geography ------
  
}

# All together now ----
shinyApp(ui, server)
