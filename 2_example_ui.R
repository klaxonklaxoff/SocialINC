source(file = "1_example_data.R", encoding = "UTF-8")

# Repeated lists ----
## VisMin lists ----
### 10 Values ----
#'NOTE [there's a specific order for vismin]
vm_10 <-
  c(
    "Total, by visible minority group",
    "South Asian",
    "Chinese",
    "Black",
    "Filipino",
    "Latin American",
    "Arab",
    "Southeast Asian",
    "Not a visible minority"
  )

### 15 Values ----
#'NOTE [the order in the values with 15 are correct]

# User interface // create layout ----
ui <-
  fluidPage(
    # titlePanel(
    #   h1("Social Inclusion Data Visualization Tool", align = "center")
    #            ),
    # # title of dashboard
    # h5("This one-stop data tool introduces a framework to organize and access data on social inclusion
    #    for ethnocultural groups and immigrants in Canada and responds to an increased demand for statistical
    #    indicators to support evidence-based decision-making aimed at building a more equitable and inclusive society."),
    tabsetPanel(
      type = "pills",
      # type of navigation button
      ## Themes and Definitions of Indicators ----
      tabPanel(
        "Themes and definitions of indicators",
        h5("This tab presents the complete list of the indicators and their associated short description of derivation."),
        fluid = TRUE,
        sidebarLayout(
          sidebarPanel(
            width = 3,
            #'NOTE [Used a radio button here to present all choices while making it a single selection]
            radioButtons(
              inputId = "theme_0",
              label = "Choose a theme",
              choices =  unique(as.character(template_2$Theme))
            ),
          ),
          #'NOTE [Outputs a table that reacts to the user's selection]
          mainPanel(dataTableOutput("def_table"))
        )
      ), #'NOTE [END OF FIRST TAB]

      ## 1. Theme: Groups designated as visible Minorities  ----
      tabPanel(
        "Visible minority and other sociodemographic characteristics",
        h5("This tab presents the indicators on social inclusion that can be disaggregated by racialized groups (visible minority), other demographic characteristics and level of geography."),
        fluid = TRUE,
        sidebarLayout(
          sidebarPanel(
            width = 3,
            
            ### 1. Theme ----
            selectizeInput(
              inputId = "theme_1",
              label = "Choose a theme",
              #'NOTE [Not showing "Local community" until it's ready]
              choices = unique(as.character(template$Theme)[template$Theme != "Local community"])
            ),

            ### 2. Indicator ----
            selectizeInput(
              inputId = "indicator_1",
              label = "Choose an indicator",
              choices = unique(as.character(template$Indicator))
            ),

            #### 2.1. Participation in the Labour Market ----
            ##### 2.1.1. Participation in the Labour Market (part 1) ----
            #'NOTE [rateDT]
            conditionalPanel(
              condition =
                "input.indicator_1 == 'Participation rate in the Labour Market'
              || input.indicator_1 == 'Employment rate'
              || input.indicator_1 == 'Unemployment rate'
              || input.indicator_1 == 'Population in full-time employment'",
              #'NOTE [indicators 1:4/22]

              ###### Visible Minority ----
              #'NOTE [this is the focal variable for this tab]
              #'NOTE [I made this one different (pickerInput) because I like the select all option but I think overall it's slower so I kept the other ones at selectizeInput]
              pickerInput(
                inputId = "lm_vismin", # name this for the server
                label = "Choose a visible minority status", # label of filter
                choices = as.character(unique(rateDT$VisMin)), # create drop-down list option
                multiple = TRUE,# multi-select
                selected = as.character(unique(rateDT$VisMin))[1],
                options = list(
                  `actions-box` = TRUE,
                  `deselect-all-text` = "Deselect all",
                  `select-all-text` = "Select all"
                )),
           
              ###### Age ----
              selectizeInput(
                inputId = "lm_age",
                label = "Choose an age group or first official language spoken",
                choices = unique(as.character(rateDT$Age))
              ),
              ###### Sex ----
              selectizeInput(
                inputId = "lm_sex",
                label = "Choose a sex",
                choices = unique(as.character(rateDT$Sex))
              ),
              ###### Immigration ----
              selectizeInput(
                inputId = "lm_immigration",
                label = "Choose an immigrant or generation status",
                choices = unique(as.character(rateDT$Immigration))
              ),
              ###### Degree ----
              selectizeInput(
                inputId = "lm_degree",
                label = "Choose a highest certificate, diploma or degree",
                choices = unique(as.character(rateDT$Degree))
              ),
              ###### Geography ----
              selectizeInput(
                inputId = "lm_geography",
                label = "Choose a geography",
                choices = unique(as.character(rateDT$Geography))
              ),
              ###### Year ----
              pickerInput(
                inputId = "lm_year", # name this for the server
                label = "Choose a year of reference", # label of filter
                choices = sort(unique(rateDT$Year), decreasing = TRUE), # create drop-down list option
                selected = sort(unique(rateDT$Year), decreasing = TRUE)[1],
                multiple = TRUE), # multi-select
            ),

            ##### 2.1.2. Participation in the Labour Market (part 2) ----
            #'NOTE [representationDT]
            conditionalPanel(
              condition =
                "input.indicator_1 == 'Population in self-employment (unincorporated)'",
              #'NOTE [indicators 5/22]

              ###### Visible Minority ----
              #'NOTE [this is the focal variable for this tab]
              pickerInput(
                inputId = "lm_rep_vismin", # name this for the server
                label = "Choose a visible minority status", # label of filter
                choices = as.character(unique(representationDT$VisMin)), # create drop-down list option
                multiple = TRUE, # multi-select
                selected = as.character(unique(representationDT$VisMin))[1],
                options = list(
                  `actions-box` = TRUE,
                  `deselect-all-text` = "Deselect all",
                  `select-all-text` = "Select all"
                )),
              
              ###### Age ----
              selectizeInput(
                inputId = "lm_rep_age",
                label = "Choose an age group or first official language spoken",
                choices = unique(as.character(representationDT$Age))
              ),
              ###### Sex ----
              selectizeInput(
                inputId = "lm_rep_sex",
                label = "Choose a sex",
                choices = unique(as.character(representationDT$Sex))
              ),
              ###### Immigration ----
              selectizeInput(
                inputId = "lm_rep_immigration",
                label = "Choose an immigrant or generation status",
                choices = unique(as.character(representationDT$Immigration))
              ),
              ###### Degree ----
              selectizeInput(
                inputId = "lm_rep_degree",
                label = "Choose a highest certificate, diploma or degree",
                choices = unique(as.character(representationDT$Degree))
              ),
              ###### Geography ----
              selectizeInput(
                inputId = "lm_rep_geography",
                label = "Choose a geography",
                choices = unique(as.character(representationDT$Geography))
              ),
              ###### Year ----
              pickerInput(
                inputId = "lm_rep_year", # name this for the server
                label = "Choose a year of reference", # label of filter
                choices = sort(unique(representationDT$Year), decreasing = TRUE), # create drop-down list option
                selected = sort(unique(representationDT$Year), decreasing = TRUE)[1],
                multiple = TRUE), # multi-select
            ),

            ##### 2.1.3. Participation in the Labour Market (part 3) ----
            #'NOTE [OverQualDT]
            conditionalPanel(
              condition =
                "input.indicator_1 == 'Population in overqualification situation'",
              #'NOTE [indicators 6/22]

              ###### Visible Minority ----
              #'NOTE [this is the focal variable for this tab]
              pickerInput(
                inputId = "lm_over_vismin", # name this for the server
                label = "Choose a visible minority status", # label of filter
                choices = as.character(unique(OverQualDT$VisMin)), # create drop-down list option
                multiple = TRUE, # multi-select
                selected = as.character(unique(OverQualDT$VisMin))[1],
                options = list(
                  `actions-box` = TRUE,
                  `deselect-all-text` = "Deselect all",
                  `select-all-text` = "Select all"
                )),
              ###### Location of Study ----
              selectizeInput(
                inputId = "lm_over_location",
                label = "Choose a location of study",
                choices = unique(as.character(OverQualDT$Location))
              ),
              ###### Degree ----
              selectizeInput(
                inputId = "lm_over_degree",
                label = "Choose a highest certificate, diploma or degree",
                choices = unique(as.character(OverQualDT$Degree))
              ),
              ###### Age ----
              selectizeInput(
                inputId = "lm_over_age",
                label = "Choose an age group",
                choices = unique(as.character(OverQualDT$Age))
              ),
              ###### Sex ----
              selectizeInput(
                inputId = "lm_over_sex",
                label = "Choose a sex",
                choices = unique(as.character(OverQualDT$Sex))
              ),
              ###### Immigration ----
              selectizeInput(
                inputId = "lm_over_immigration",
                label = "Groups designated by Immigration and Generational Status",
                choices = unique(as.character(OverQualDT$Immigration))
              ),
              ###### Language ----
              selectizeInput(
                inputId = "lm_over_language",
                label = "Choose a language",
                choices = unique(as.character(OverQualDT$Language))
              ),
              ###### Geography ----
              selectizeInput(
                inputId = "lm_over_geography",
                label = "Choose a geography",
                choices = unique(as.character(OverQualDT$Geography))
              ),
              ###### Year ----
              pickerInput(
                inputId = "lm_over_year", # name this for the server
                label = "Choose a year of reference", # label of filter
                choices = sort(unique(OverQualDT$Year), decreasing = TRUE), # create drop-down list option
                selected = sort(unique(OverQualDT$Year), decreasing = TRUE)[1],
                multiple = TRUE), # multi-select
            ),
            
            ##### 2.1.4. Participation in the Labour Market (part 4) ----
            #'NOTE [youthDT]
            conditionalPanel(
              condition =
                "input.indicator_1 == 'Youth not in employment, education or training'",
              #'NOTE [indicators 7/22]

              ###### Visible Minority ----
              #'NOTE [this is the focal variable for this tab]
              pickerInput(
                inputId = "lm_youth_vismin", # name this for the server
                label = "Choose a visible minority status", # label of filter
                choices = as.character(unique(youthDT$VisMin)), # create drop-down list option
                multiple = TRUE, # multi-select
                selected = as.character(unique(youthDT$VisMin))[1],
                options = list(
                  `actions-box` = TRUE,
                  `deselect-all-text` = "Deselect all",
                  `select-all-text` = "Select all"
                )),
              ###### Age ----
              selectizeInput(
                inputId = "lm_youth_age",
                label = "Choose an age group",
                choices = unique(as.character(youthDT$Age))
              ),
              ###### Sex ----
              selectizeInput(
                inputId = "lm_youth_sex",
                label = "Choose a sex",
                choices = unique(as.character(youthDT$Sex))
              ),
              ###### Immigration ----
              selectizeInput(
                inputId = "lm_youth_immigration",
                label = "Choose an immigrant or generation status",
                choices = unique(as.character(youthDT$Immigration))
              ),
              ###### Language ----
              selectizeInput(
                inputId = "lm_youth_language",
                label = "Choose a language",
                choices = unique(as.character(youthDT$Language))
              ),
              ###### Geography ----
              selectizeInput(
                inputId = "lm_youth_geography",
                label = "Choose a geography",
                choices = unique(as.character(youthDT$Geography))
              ),
              ###### Year ----
              pickerInput(
                inputId = "lm_youth_year", # name this for the server
                label = "Choose a year of reference", # label of filter
                choices = sort(unique(youthDT$Year), decreasing = TRUE), # create drop-down list option
                selected = sort(unique(youthDT$Year), decreasing = TRUE)[1],
                multiple = TRUE), # multi-select
            ),

            ##### 2.1.6. Participation in the Labour Market (part 6) ----
            #'NOTE [employmentDT]
            #' [THIS TABLE HAS NOT BEEN PUBLISHED YET]
            #'NOTE [indicators 10:16/22]

            #' conditionalPanel(
            #'   condition =
            #'     "input.ind_labour_market == 'Paid employees considering their current job good for career advancement'
            #'   || input.ind_labour_market == 'Paid employees considering their current job good for career advancement'
            #'   || input.ind_labour_market == 'Paid employees receiving at least one employment benefit in their current job'
            #'   || input.ind_labour_market == 'Paid employees having pension plan in their current job'
            #'   || input.ind_labour_market == 'Paid employees having paid sick leave in their current job'
            #'   || input.ind_labour_market == 'Paid employees having paid vacation leave in their current job'
            #'   || input.ind_labour_market == 'Paid employees having disability insurance in their current job'",
            #'
            #'   ###### Visible Minority ----
            #'   #'NOTE [this is the focal variable for this tab]
            # pickerInput(
            #   inputId = "lm_employment_vismin", # name this for the server
            #   label = "Choose a visible minority status", # label of filter
            #   choices = as.character(unique(employmentDT$VisMin)),
            #   # create drop-down list option
            #   multiple = TRUE, # multi-select
            #   selected = as.character(unique(employmentDT$VisMin))[1],
            #   options = list(
            #     `actions-box` = TRUE,
            #     `deselect-all-text` = "Deselect all",
            #     `select-all-text` = "Select all"
            #   )),
            #'   ###### Geography ----
            #'   selectizeInput(
            #'     inputId = "lm_employmentgeography",
            #'     label = "Choose a geography",
            #'     choices = unique(as.character(employmentDT$Geography))
            #'   ),
            #'   ###### Characteristic ----
            #'   selectizeInput(
            #'     inputId = "lm_employment_sex",
            #'     label = "Characteristic",
            #'     choices = unique(as.character(employmentDT$Characteristic))
            #'   ),
            #'   ###### Confidence ----
            #'   Confidence(
            #'     inputId = "lm_employment_language",
            #'     label = "Confidence",
            #'     choices = unique(as.character(employmentDT$Confidence))
            #'   )
            #' ),

            #'NOTE [end of Participation in the Labour Market section]
            #'NOTE [MISSING INDICATORS 17:22]
            #'[17 "Paid employees having supplemental medical care in their current job"]
            #'[18 "Paid employees having worker's compensation in their current job"]
            #'[19 "Paid employees having maternity, paternity or lay-off benefits in their current job"]
            #'[20 "Paid employees covered by union contract or collective agreement in their current job"]
            #'[21 "Paid employees receiving formal training in their current job"]
            #'[22 "Paid employees receiving informal training in their current job"]

            #### 2.2. Civic engagement and political participation ----
            ##### 2.2.1. Civic engagement and political participation (part 1) ----
            #'NOTE [civicDT]
            conditionalPanel(
              condition =
                "input.indicator_1 == 'Member or participant of at least one group, organization or association'
              || input.indicator_1 == 'Member or participant in sports or recreational organization'
              || input.indicator_1 == 'Member or participant in cultural, educational or hobby organization'
              || input.indicator_1 == 'Member or participant in union or professional association'
              || input.indicator_1 == 'Member or participant in political party or group'
              || input.indicator_1 == 'Member or participant in religious-affiliated group'
              || input.indicator_1 == 'Member or participant in school group, neighbourhood, civic or community association'
              || input.indicator_1 == 'Member or participant in humanitarian or charitable organization or service club'
              || input.indicator_1 == 'Member or participant in seniors\\' group'
              || input.indicator_1 == 'Member or participant in youth organization'
              || input.indicator_1 == 'Member or participant in immigrant or ethnic association or club'
              || input.indicator_1 == 'Member or participant in environmental group'
              || input.indicator_1 == 'Engaged in political activities'",
              #'NOTE [indicators 1:13/16]
              #'#'NOTE [you need 2 backslashes to escape that single quotation used in "Member or participant in seniors' group" because otherwise it thinks that's where the condition ends (AKA: "Percent of the population members in a seniors")]

              ###### Visible Minority ----
              #'NOTE [this is the focal variable for this tab]
              pickerInput(
                inputId = "civic_vismin", # name this for the server
                label = "Choose a visible minority status", # label of filter
                choices = vm_10, # create drop-down list option
                multiple = TRUE, # multi-select
                selected = vm_10[1],
                options = list(
                  `actions-box` = TRUE,
                  `deselect-all-text` = "Deselect all",
                  `select-all-text` = "Select all"
                )),
              ###### Selected sociodemographic characteristics ----
              selectizeInput(
                inputId = "civic_sociodem",
                label = "Choose a sociodemographic characteristic",
                choices = unique(as.character(civicDT$char_type))
              ),
              ####### Age ----
              conditionalPanel(
                condition = "input.civic_sociodem == 'Age'",
                selectizeInput(
                  inputId = "civic_age",
                  label = "Choose an age group",
                  choices = unique(as.character(civicDT$Characteristic)[civicDT$char_type == "Age"])
                )
              ),
              ####### Gender ----
              conditionalPanel(
                condition = "input.civic_sociodem == 'Gender'",
                selectizeInput(
                  inputId = "civic_sex",
                  label = "Choose a gender",
                  choices = unique(as.character(civicDT$Characteristic)[civicDT$char_type == "Gender"])
                )
              ),
              ####### Immigration Status ----
              conditionalPanel(
                condition = "input.civic_sociodem == 'Immigration Status'",
                selectizeInput(
                  inputId = "civic_immigration",
                  label = "Choose an immigration status",
                  choices = unique(as.character(civicDT$Characteristic)[civicDT$char_type == "Immigration Status"])
                )
              ),
              ####### Generation Status ----
              conditionalPanel(
                condition = "input.civic_sociodem == 'Generation Status'",
                selectizeInput(
                  inputId = "civic_generation",
                  label = "Choose a generation status",
                  choices = unique(as.character(civicDT$Characteristic)[civicDT$char_type == "Generation Status"])
                )
              ),
              ####### Language Spoken ----
              conditionalPanel(
                condition = "input.civic_sociodem == 'Language Spoken'",
                selectizeInput(
                  inputId = "civic_language",
                  label = "Choose an age group",
                  choices = unique(as.character(civicDT$Characteristic)[civicDT$char_type == "Language Spoken"])
                )
              ),
              ####### Education Status ----
              conditionalPanel(
                condition = "input.civic_sociodem == 'Education Status'",
                selectizeInput(
                  inputId = "civic_education",
                  label = "Choose an education status",
                  choices = unique(as.character(civicDT$Characteristic)[civicDT$char_type == "Education Status"])
                )
              ),
              ###### Geography  ----
              selectizeInput(
                inputId = "civic_geography",
                label = "Choose a geography",
                choices = unique(as.character(civicDT$Geography))
              ),
              ###### Confidence Interval ----
              selectizeInput(
                inputId = "civic_conf_interval",
                label = "Choose a confidence interval",
                choices = unique(as.character(civicDT$Confidence))
              )
            ),

            ##### 2.2.2. Civic engagement and political participation (part 2) ----
            #'NOTE [civicDT2]
            conditionalPanel(
              condition =
                "input.indicator_1 == 'Voted in last federal election'
              || input.indicator_1 == 'Voted in last provincial election'
              || input.indicator_1 == 'Voted in last municipal election'",
              #'NOTE [indicators 14:16/16]

              ###### Visible Minority ----
              #'NOTE [this is the focal variable for this tab]
              pickerInput(
                inputId = "civic2_vismin", # name this for the server
                label = "Choose a visible minority status", # label of filter
                choices = vm_10, # create drop-down list option
                multiple = TRUE, # multi-select
                selected = vm_10[1],
                options = list(
                  `actions-box` = TRUE,
                  `deselect-all-text` = "Deselect all",
                  `select-all-text` = "Select all"
                )),
              ###### Selected sociodemographic characteristics ----
              selectizeInput(
                inputId = "civic2_sociodem",
                label = "Choose a sociodemographic characteristic",
                choices = unique(as.character(civicDT2$char_type))
              ),
              ####### Age ----
              conditionalPanel(
                condition = "input.civic2_sociodem == 'Age'",
                selectizeInput(
                  inputId = "civic2_age",
                  label = "Choose an age group",
                  choices = unique(as.character(civicDT2$Characteristic)[civicDT2$char_type == "Age"])
                )
              ),
              ####### Gender ----
              conditionalPanel(
                condition = "input.civic2_sociodem == 'Gender'",
                selectizeInput(
                  inputId = "civic2_sex",
                  label = "Choose a gender",
                  choices = unique(as.character(civicDT2$Characteristic)[civicDT2$char_type == "Gender"])
                )
              ),
              ####### Immigration Status ----
              conditionalPanel(
                condition = "input.civic2_sociodem == 'Immigration Status'",
                selectizeInput(
                  inputId = "civic2_immigration",
                  label = "Choose an immigration status",
                  choices = unique(as.character(civicDT2$Characteristic)[civicDT2$char_type == "Immigration Status"])
                )
              ),
              ####### Generation Status ----
              conditionalPanel(
                condition = "input.civic2_sociodem == 'Generation Status'",
                selectizeInput(
                  inputId = "civic2_generation",
                  label = "Choose a generation status",
                  choices = unique(as.character(civicDT2$Characteristic)[civicDT2$char_type == "Generation Status"])
                )
              ),
              ####### Language Spoken ----
              conditionalPanel(
                condition = "input.civic2_sociodem == 'Language Spoken'",
                selectizeInput(
                  inputId = "civic2_language",
                  label = "Choose an age group",
                  choices = unique(as.character(civicDT2$Characteristic)[civicDT2$char_type == "Language Spoken"])
                )
              ),
              ####### Education Status ----
              conditionalPanel(
                condition = "input.civic2_sociodem == 'Education Status'",
                selectizeInput(
                  inputId = "civic2_education",
                  label = "Choose an education status",
                  choices = unique(as.character(civicDT2$Characteristic)[civicDT2$char_type == "Education Status"])
                )
              ),
              ###### Geography  ----
              selectizeInput(
                inputId = "civic2_geography",
                label = "Choose a geography",
                choices = unique(as.character(civicDT2$Geography))
              ),
              ###### Confidence Interval ----
              selectizeInput(
                inputId = "civic2_conf_interval",
                label = "Choose a confidence interval",
                choices = unique(as.character(civicDT2$Confidence))
              )
            ),

            #### 2.3. Representation in decision-making positions ----
            #'NOTE [representationDT]
            conditionalPanel(
              condition =
                "input.theme_1 == 'Representation in decision-making positions'",
              #'NOTE [indicators 1:4/4]

              ##### Visible Minority ----
              #'NOTE [this is the focal variable for this tab]
              pickerInput(
                inputId = "rep_vismin", # name this for the server
                label = "Choose a visible minority status", # label of filter
                choices = unique(as.character(representationDT$VisMin)), # create drop-down list option
                multiple = TRUE, # multi-select
                selected = unique(as.character(representationDT$VisMin))[1],
                options = list(
                  `actions-box` = TRUE,
                  `deselect-all-text` = "Deselect all",
                  `select-all-text` = "Select all"
                )),
              ##### Age group and first official language spoken ----
              selectizeInput(
                inputId = "rep_age",
                label = "Choose an age group or first official language spoken",
                choices = unique(as.character(representationDT$Age))
              ),
              ##### Gender ----
              selectizeInput(
                inputId = "rep_sex",
                label = "Choose a gender",
                choices = unique(as.character(representationDT$Sex))
              ),
              ##### Immigrant and generation status ----
              selectizeInput(
                inputId = "rep_immigration",
                label = "Choose an immigrant or generation status",
                choices = unique(as.character(representationDT$Immigration))
              ),
              ##### Highest certificate, diploma or degree ----
              selectizeInput(
                inputId = "rep_degree",
                label = "Choose a highest certificate, diploma or degree",
                choices = unique(as.character(representationDT$Degree))
              ),
              ##### Geography ----
              selectizeInput(
                inputId = "rep_geography",
                label = "Choose a geography",
                choices = unique(as.character(representationDT$Geography))
              ),
              ###### Year ----
              pickerInput(
                inputId = "rep_year", # name this for the server
                label = "Choose a year of reference", # label of filter
                choices = sort(unique(representationDT$Year), decreasing = TRUE), # create drop-down list option
                selected = sort(unique(representationDT$Year), decreasing = TRUE)[1],
                multiple = TRUE), # multi-select
            ),

            #### 2.4. Basic needs and housing ----
            #'NOTE [basicDT]
            conditionalPanel(
              condition =
                "input.theme_1 == 'Basic needs and housing'",
              #'NOTE [is there a reason why in the originaly code we don't see the following indicators:]
              #'[Population living in a dwelling owned by one member of the household]
              #'[Population living in core need housing]
              #'[Population living in suitable housing]
              #'[Population living in an affordable housing]

              ##### Visible Minority ----
              #'NOTE [this is the focal variable for this tab]
              pickerInput(
                inputId = "basic_vismin", # name this for the server
                label = "Choose a visible minority status", # label of filter
                choices = vm_10, # create drop-down list option
                multiple = TRUE, # multi-select
                selected = vm_10[1],
                options = list(
                  `actions-box` = TRUE,
                  `deselect-all-text` = "Deselect all",
                  `select-all-text` = "Select all"
                )),
              ##### Selected sociodemographic characteristics ----
              selectizeInput(
                inputId = "basic_sociodem",
                label = "Choose a sociodemographic characteristic",
                choices = unique(as.character(basicDT$char_type))
              ),
              ###### Age ----
              conditionalPanel(
                condition = "input.basic_sociodem == 'Age'",
                selectizeInput(
                  inputId = "basic_age",
                  label = "Choose an age group",
                  choices = unique(as.character(basicDT$Characteristic)[basicDT$char_type == "Age"])
                )
              ),
              ###### Gender ----
              conditionalPanel(
                condition = "input.basic_sociodem == 'Gender'",
                selectizeInput(
                  inputId = "basic_sex",
                  label = "Choose a gender",
                  choices = unique(as.character(basicDT$Characteristic)[basicDT$char_type == "Gender"])
                )
              ),
              ###### Immigration Status ----
              conditionalPanel(
                condition = "input.basic_sociodem == 'Immigration Status'",
                selectizeInput(
                  inputId = "basic_immigration",
                  label = "Choose an immigration status",
                  choices = unique(as.character(basicDT$Characteristic)[basicDT$char_type == "Immigration Status"])
                )
              ),
              ##### Geography  ----
              selectizeInput(
                inputId = "basic_geography",
                label = "Choose a geography",
                choices = unique(as.character(basicDT$Geography))
              ),
              ###### Year ----
              pickerInput(
                inputId = "basic_year", # name this for the server
                label = "Choose a year of reference", # label of filter
                choices = sort(unique(basicDT$Year), decreasing = TRUE), # create drop-down list option
                selected = sort(unique(basicDT$Year), decreasing = TRUE)[1],
                multiple = TRUE), # multi-select
              ##### Confidence Interval ----
              selectizeInput(
                inputId = "basic_conf_interval",
                label = "Choose a confidence interval",
                choices = unique(as.character(basicDT$Confidence))
              )
            ),

            #### 2.5. Local community ----
            #'NOTE [it doesn't look like there'a any conditions following this theme?]
            #'[from my notes it looks like it should take from incomeDT]

            #### 2.6. Health and wellbeing ----
            #'NOTE [basicDT]
            conditionalPanel(
              condition =
                "input.theme_1 == 'Health and wellbeing'",
              ##### Visible Minority ----
              #'NOTE [this is the focal variable for this tab]
              pickerInput(
                inputId = "health_vismin", # name this for the server
                label = "Choose a visible minority status", # label of filter
                choices = vm_10, # create drop-down list option
                multiple = TRUE, # multi-select
                selected = vm_10[1],
                options = list(
                  `actions-box` = TRUE,
                  `deselect-all-text` = "Deselect all",
                  `select-all-text` = "Select all"
                )),
              ##### Selected sociodemographic characteristics ----
              selectizeInput(
                inputId = "health_sociodem",
                label = "Choose a sociodemographic characteristic",
                choices = unique(as.character(basicDT$char_type))
              ),
              ###### Age ----
              conditionalPanel(
                condition = "input.health_sociodem == 'Age'",
                selectizeInput(
                  inputId = "health_age",
                  label = "Choose an age group",
                  choices = unique(as.character(basicDT$Characteristic)[basicDT$char_type == "Age"])
                )
              ),
              ###### Gender ----
              conditionalPanel(
                condition = "input.health_sociodem == 'Gender'",
                selectizeInput(
                  inputId = "health_sex",
                  label = "Choose a gender",
                  choices = unique(as.character(basicDT$Characteristic)[basicDT$char_type == "Gender"])
                )
              ),
              ###### Immigration Status ----
              conditionalPanel(
                condition = "input.health_sociodem == 'Immigration Status'",
                selectizeInput(
                  inputId = "health_immigration",
                  label = "Choose an immigration status",
                  choices = unique(as.character(basicDT$Characteristic)[basicDT$char_type == "Immigration Status"])
                )
              ),
              ##### Geography  ----
              selectizeInput(
                inputId = "health_geography",
                label = "Choose a geography",
                choices = unique(as.character(basicDT$Geography))
              ),
              ###### Year ----
              pickerInput(
                inputId = "health_year", # name this for the server
                label = "Choose a year of reference", # label of filter
                choices = sort(unique(basicDT$Year), decreasing = TRUE), # create drop-down list option
                selected = sort(unique(basicDT$Year), decreasing = TRUE)[1],
                multiple = TRUE), # multi-select
              ##### Confidence Interval ----
              selectizeInput(
                inputId = "health_conf_interval",
                label = "Choose a confidence interval",
                choices = unique(as.character(basicDT$Confidence))
              )
            ),

            #### 2.7. Public services and institutions (Part 1)----
            #'NOTE [confidenceDT]
            conditionalPanel(
              condition =
                "input.indicator_1 == 'Confidence in Federal Parliament'
              || input.indicator_1 == 'Confidence in the Canadian media'
              || input.indicator_1 == 'Confidence in the school system'
              || input.indicator_1 == 'Confidence in the justice system and courts'
              || input.indicator_1 == 'Confidence in the police service'
              || input.indicator_1 == 'Confidence in major corporations'
              || input.indicator_1 == 'Confidence in merchants and local business people'
              || input.indicator_1 == 'Confidence in banks'",
              ##### Visible Minority ----

              ##### Visible Minority ----
              #'NOTE [this is the focal variable for this tab]
              pickerInput(
                inputId = "public_vismin", # name this for the server
                label = "Choose a visible minority status", # label of filter
                choices = vm_10, # create drop-down list option
                multiple = TRUE, # multi-select
                selected = vm_10[1],
                options = list(
                  `actions-box` = TRUE,
                  `deselect-all-text` = "Deselect all",
                  `select-all-text` = "Select all"
                )),
              ##### Selected sociodemographic characteristics ----
              selectizeInput(
                inputId = "public_sociodem",
                label = "Choose a sociodemographic characteristic",
                choices = unique(as.character(confidenceDT$char_type))
              ),

              ###### Age ----
              conditionalPanel(
                condition = "input.public_sociodem == 'Age'",
                selectizeInput(
                  inputId = "public_age",
                  label = "Choose an age group",
                  choices = unique(as.character(confidenceDT$Characteristic)[confidenceDT$char_type == "Age"])
                )
              ),
              ###### Gender ----
              conditionalPanel(
                condition = "input.public_sociodem == 'Gender'",
                selectizeInput(
                  inputId = "public_sex",
                  label = "Choose a gender",
                  choices = unique(as.character(confidenceDT$Characteristic)[confidenceDT$char_type == "Gender"])
                )
              ),
              ###### Immigration Status ----
              conditionalPanel(
                condition = "input.public_sociodem == 'Immigration Status'",
                selectizeInput(
                  inputId = "public_immigration",
                  label = "Choose an immigration status",
                  choices = unique(as.character(confidenceDT$Characteristic)[confidenceDT$char_type == "Immigration Status"])
                )
              ),
              ###### Generation Status ----
              conditionalPanel(
                condition = "input.public_sociodem == 'Generation Status'",
                selectizeInput(
                  inputId = "public_generation",
                  label = "Choose an immigration status",
                  choices = unique(as.character(confidenceDT$Characteristic)[confidenceDT$char_type == "Generation Status"])
                )
              ),
              ###### Language Spoken ----
              conditionalPanel(
                condition = "input.public_sociodem == 'Language Spoken'",
                selectizeInput(
                  inputId = "public_language",
                  label = "Choose a language spoken",
                  choices = unique(as.character(confidenceDT$Characteristic)[confidenceDT$char_type == "Language Spoken"])
                )
              ),
              ###### Education Status ----
              conditionalPanel(
                condition = "input.public_sociodem == 'Education Status'",
                selectizeInput(
                  inputId = "public_education",
                  label = "Choose an education status",
                  choices = unique(as.character(confidenceDT$Characteristic)[confidenceDT$char_type == "Education Status"])
                )
              ),
              ##### Geography ----
              selectizeInput(
                inputId = "public_geography",
                label = "Choose a geography",
                choices = unique(as.character(confidenceDT$Geography))
              ),
              ##### Confidence Interval ----
              selectizeInput(
                inputId = "public_conf_interval",
                label = "Choose a confidence interval",
                choices = unique(as.character(confidenceDT$Confidence))
              )
            ),
            #### 2.7.1 Public services and institutions (Part 2)----
            #'NOTE [healthDT]
            conditionalPanel(
              condition =
                "input.indicator_1 == 'Has a regular health care provider'
              || input.indicator_1 == 'Perceived need for mental health care, no need'
              || input.indicator_1 == 'Perceived need for mental health care, all needs met'
              || input.indicator_1 == 'Perceived need for mental health care, needs partially met'
              || input.indicator_1 == 'Perceived need for mental health care, needs partially met or needs not met'
              || input.indicator_1 == 'Perceived need for mental health care, needs not met'
              || input.indicator_1 == 'Unmet health care needs'",
              ##### Visible Minority ----
              #'NOTE [this is the focal variable for this tab]
              pickerInput(
                inputId = "health_public_vismin", # name this for the server
                label = "Choose a visible minority status", # label of filter
                choices = vm_10, # create drop-down list option
                multiple = TRUE, # multi-select
                selected = vm_10[1],
                options = list(
                  `actions-box` = TRUE,
                  `deselect-all-text` = "Deselect all",
                  `select-all-text` = "Select all"
                )),
              ##### Selected sociodemographic characteristics ----
              selectizeInput(
                inputId = "health_public_sociodem",
                label = "Choose a sociodemographic characteristic",
                choices = unique(as.character(basicDT$char_type))
              ),
              ###### Age ----
              conditionalPanel(
                condition = "input.health_public_sociodem == 'Age'",
                selectizeInput(
                  inputId = "health_public_age",
                  label = "Choose an age group",
                  choices = unique(as.character(basicDT$Characteristic)[basicDT$char_type == "Age"])
                )
              ),
              ###### Gender ----
              conditionalPanel(
                condition = "input.health_public_sociodem == 'Gender'",
                selectizeInput(
                  inputId = "health_public_sex",
                  label = "Choose a gender",
                  choices = unique(as.character(basicDT$Characteristic)[basicDT$char_type == "Gender"])
                )
              ),
              ###### Immigration Status ----
              conditionalPanel(
                condition = "input.health_public_sociodem == 'Immigration Status'",
                selectizeInput(
                  inputId = "health_public_immigration",
                  label = "Choose an immigration status",
                  choices = unique(as.character(basicDT$Characteristic)[basicDT$char_type == "Immigration Status"])
                )
              ),
              ##### Geography  ----
              selectizeInput(
                inputId = "health_public_geography",
                label = "Choose a geography",
                choices = unique(as.character(basicDT$Geography))
              ),
              ###### Year ----
              pickerInput(
                inputId = "health_public_year", # name this for the server
                label = "Choose a year of reference", # label of filter
                choices = sort(unique(basicDT$Year), decreasing = TRUE), # create drop-down list option
                selected = sort(unique(basicDT$Year), decreasing = TRUE)[1],
                multiple = TRUE), # multi-select
              ##### Confidence Interval ----
              selectizeInput(
                inputId = "health_conf_public_interval",
                label = "Choose a confidence interval",
                choices = unique(as.character(basicDT$Confidence))
              )
            ),
            
            #### 2.8. Education, training and skills ----
            #'NOTE [educationDT]
            conditionalPanel(
              condition =
                "input.theme_1 == 'Education and training skills'",
              #'NOTE [indicators 1:6/6]

              ##### Visible Minority ----
              #'NOTE [this is the focal variable for this tab]
              pickerInput(
                inputId = "education_vismin", # name this for the server
                label = "Choose a visible minority status", # label of filter
                choices = unique(as.character(educationDT$VisMin)), # create drop-down list option
                multiple = TRUE, # multi-select
                selected = unique(as.character(educationDT$VisMin))[1],
                options = list(
                  `actions-box` = TRUE,
                  `deselect-all-text` = "Deselect all",
                  `select-all-text` = "Select all"
                )),
              
              ##### Age group  ----
              selectizeInput(
                inputId = "education_age",
                label = "Choose an age group",
                choices = unique(as.character(educationDT$Age))
              ),
              ##### Gender ----
              selectizeInput(
                inputId = "education_sex",
                label = "Choose a gender",
                choices = unique(as.character(educationDT$Sex))
              ),
              ##### Immigrant and generation status ----
              selectizeInput(
                inputId = "education_immigration",
                label = "Choose an immigrant or generation status",
                choices = unique(as.character(educationDT$Immigration))
              ),
              ##### Language ----
              selectizeInput(
                inputId = "education_language",
                label = "Choose a first official language spoken",
                choices = unique(as.character(educationDT$Language))
              ),
              ##### Geography ----
              selectizeInput(
                inputId = "education_geography",
                label = "Choose a geography",
                choices = unique(as.character(educationDT$Geography))
              ),
              ###### Year ----
              pickerInput(
                inputId = "education_year", # name this for the server
                label = "Choose a year of reference", # label of filter
                choices = sort(unique(educationDT$Year), decreasing = TRUE), # create drop-down list option
                selected = sort(unique(educationDT$Year), decreasing = TRUE)[1],
                multiple = TRUE), # multi-select
            ),
            #### 2.9. Social connections and personnal networks ----
            #'NOTE [belongingDT]
            conditionalPanel(
              condition =
                "input.indicator_1 == 'Reported that most people can be trusted in general'
              || input.indicator_1 == 'Strong sense of belonging to the local community'
              || input.indicator_1 == 'Srong sense of belonging to the town or city'
              || input.indicator_1 == 'Strong sense of belonging to the province'
              || input.indicator_1 == 'Strong sense of belonging to Canada'",

              ##### Visible Minority ----
              #'NOTE [this is the focal variable for this tab]
              pickerInput(
                inputId = "belonging_vismin", # name this for the server
                label = "Choose a visible minority status", # label of filter
                choices = vm_10, # create drop-down list option
                multiple = TRUE, # multi-select
                selected = vm_10[1],
                options = list(
                  `actions-box` = TRUE,
                  `deselect-all-text` = "Deselect all",
                  `select-all-text` = "Select all"
                )),
              ##### Selected sociodemographic characteristics ----
              selectizeInput(
                inputId = "belonging_sociodem",
                label = "Choose a sociodemographic characteristic",
                choices = unique(as.character(belongingDT$char_type))
              ),

              ###### Age ----
              conditionalPanel(
                condition = "input.belonging_sociodem == 'Age'",
                selectizeInput(
                  inputId = "belonging_age",
                  label = "Choose an age group",
                  choices = unique(as.character(belongingDT$Characteristic)[belongingDT$char_type == "Age"])
                )
              ),
              ###### Gender ----
              conditionalPanel(
                condition = "input.belonging_sociodem == 'Gender'",
                selectizeInput(
                  inputId = "public_income_social_gender",
                  label = "Choose a gender",
                  choices = unique(as.character(belongingDT$Characteristic)[belongingDT$char_type == "Gender"])
                )
              ),
              ###### Immigration Status ----
              conditionalPanel(
                condition = "input.belonging_sociodem == 'Immigration Status'",
                selectizeInput(
                  inputId = "belonging_immigration",
                  label = "Choose an immigration status",
                  choices = unique(as.character(belongingDT$Characteristic)[belongingDT$char_type == "Immigration Status"])
                )
              ),
              ###### Generation Status ----
              conditionalPanel(
                condition = "input.belonging_sociodem == 'Generation Status'",
                selectizeInput(
                  inputId = "belonging_generation",
                  label = "Choose an immigration status",
                  choices = unique(as.character(belongingDT$Characteristic)[belongingDT$char_type == "Generation Status"])
                )
              ),
              ###### Language Spoken ----
              conditionalPanel(
                condition = "input.belonging_sociodem == 'Language Spoken'",
                selectizeInput(
                  inputId = "belonging_language",
                  label = "Choose a language spoken",
                  choices = unique(as.character(belongingDT$Characteristic)[belongingDT$char_type == "Language Spoken"])
                )
              ),
              ###### Education Status ----
              conditionalPanel(
                condition = "input.belonging_sociodem == 'Education Status'",
                selectizeInput(
                  inputId = "belonging_education",
                  label = "Choose an education status",
                  choices = unique(as.character(belongingDT$Characteristic)[belongingDT$char_type == "Education Status"])
                )
              ),
              ##### Geography ----
              selectizeInput(
                inputId = "belonging_geography",
                label = "Choose a geography",
                choices = unique(as.character(belongingDT$Geography))
              ),
              ##### Confidence Interval ----
              selectizeInput(
                inputId = "belonging_conf_interval",
                label = "Choose a confidence interval",
                choices = unique(as.character(belongingDT$Confidence))
              )
            ),
           
            #### 2.10. Discrimination and victimization ----
            ##### 2.10.1. Discrimination and victimization (part 1) ----
            #'NOTE [discriminationDT]
            conditionalPanel(
              condition =
                "input.theme_1 == 'Discrimination and victimization'
              && input.indicator_1 != 'Police-reported hate crimes motivated by religion and race or ethnicity'",

              ##### Visible Minority ----
              #'NOTE [this is the focal variable for this tab]
              pickerInput(
                inputId = "discrimination_vismin", # name this for the server
                label = "Choose a visible minority status", # label of filter
                choices = vm_10, # create drop-down list option
                multiple = TRUE, # multi-select
                selected = vm_10[1],
                options = list(
                  `actions-box` = TRUE,
                  `deselect-all-text` = "Deselect all",
                  `select-all-text` = "Select all"
                )),
              ##### Selected sociodemographic characteristics ----
              selectizeInput(
                inputId = "discrimination_sociodem",
                label = "Choose a sociodemographic characteristic",
                choices = unique(as.character(discriminationDT$char_type))
              ),
              ###### Age ----
              conditionalPanel(
                condition = "input.discrimination_sociodem == 'Age'",
                selectizeInput(
                  inputId = "discrimination_age",
                  label = "Choose an age group",
                  choices = unique(as.character(discriminationDT$Characteristic)[discriminationDT$char_type == "Age"])
                )
              ),
              ###### Gender ----
              conditionalPanel(
                condition = "input.discrimination_sociodem == 'Gender'",
                selectizeInput(
                  inputId = "discrimination_sex",
                  label = "Choose a gender",
                  choices = unique(as.character(discriminationDT$Characteristic)[discriminationDT$char_type == "Gender"])
                )
              ),
              ###### Immigration Status ----
              conditionalPanel(
                condition = "input.discrimination_sociodem == 'Immigration Status'",
                selectizeInput(
                  inputId = "discrimination_immigration",
                  label = "Choose an immigration status",
                  choices = unique(as.character(discriminationDT$Characteristic)[discriminationDT$char_type == "Immigration Status"])
                )
              ),
              ###### Generation Status ----
              conditionalPanel(
                condition = "input.discrimination_sociodem == 'Generation Status'",
                selectizeInput(
                  inputId = "discrimination_generation",
                  label = "Choose a gender",
                  choices = unique(as.character(discriminationDT$Characteristic)[discriminationDT$char_type == "Generation Status"])
                )
              ),
              ###### Language Spoken ----
              conditionalPanel(
                condition = "input.discrimination_sociodem == 'Language Spoken'",
                selectizeInput(
                  inputId = "discrimination_language",
                  label = "Choose a language spoken",
                  choices = unique(as.character(discriminationDT$Characteristic)[discriminationDT$char_type == "Language Spoken"])
                )
              ),
              ###### Education Status ----
              conditionalPanel(
                condition = "input.discrimination_sociodem == 'Education Status'",
                selectizeInput(
                  inputId = "discrimination_education",
                  label = "Choose an education status",
                  choices = unique(as.character(discriminationDT$Characteristic)[discriminationDT$char_type == "Education Status"])
                )
              ),
              ##### Geography ----
              selectizeInput(
                inputId = "discrimination_geography",
                label = "Choose a geography",
                choices = unique(as.character(discriminationDT$Geography))
              ),
              ##### Confidence Interval ----
              selectizeInput(
                inputId = "discrimination_conf_interval",
                label = "Choose a confidence interval",
                choices = unique(as.character(discriminationDT$Confidence))
              )
            ),
            ##### 2.10.2. Discrimination and victimization (part 2) ----
            #'NOTE [polData]
            conditionalPanel(
              condition =
              "input.indicator_1 == 'Police-reported hate crimes motivated by religion and race or ethnicity'",
              ##### Year----
              pickerInput(
                inputId = "hate_year",
                label = "Choose a year of reference",
                choices = unique(as.character(polData$Year)),
                selected = unique(as.character(polData$Year))[1:15], # I made it 15 to overcapture in case more years come in
                multiple = TRUE,
                options = list(
                  `actions-box` = TRUE,
                  `deselect-all-text` = "Deselect all",
                  `select-all-text` = "Select all"
              )),
              
              ##### Motivation----
                selectizeInput(
                   inputId = "hate_motivation",
                   label = "Choose a motivation",
                   choices = unique(as.character(polData$motivation_type)),
                   selected = unique(as.character(polData$motivation_type))[1],              
                ),
              ##### Race or ethnicity ----
              conditionalPanel(
                condition = "input.hate_motivation == 'Race or ethnicity'",
                pickerInput(
                  inputId = "hate_race",
                  label = "Choose a race or ethnicity or other characteristics",
                  choices = unique(as.character(polData$Motivation)[polData$motivation_type == "Race or ethnicity"]),
                  selected = unique(as.character(polData$Motivation)[polData$motivation_type == "Race or ethnicity"]),
                  multiple = TRUE,
                  options = list(
                    `actions-box` = TRUE,
                    `deselect-all-text` = "Deselect all",
                    `select-all-text` = "Select all"
                  )
                )
              ), 
              ##### Total police-reported hate crime -----
              conditionalPanel(
                condition = "input.hate_motivation == 'Total police-reported hate crime'",
                pickerInput(
                  inputId = "hate_police",
                  label = "Choose a police reported hate crime type",
                  choices = unique(as.character(polData$Motivation)[polData$motivation_type == "Total police-reported hate crime"]),
                  selected = unique(as.character(polData$Motivation)[polData$motivation_type == "Total police-reported hate crime"]),
                  multiple = TRUE,
                  options = list(
                    `actions-box` = TRUE,
                    `deselect-all-text` = "Deselect all",
                    `select-all-text` = "Select all"
                  )
                )
              ),
              ##### Geography----
              selectizeInput(
                inputId = "hate_geography",
                label = "Choose a geography",
                choices = unique(as.character(polData$Geography)),
                selected = unique(as.character(polData$Geography))[1],              
              ),
            ), 
            
               #### 2.11. Income and wealth ----
               #'NOTE [incomeDT]
              conditionalPanel(
                condition =
                  "input.indicator_1 == 'Average employment income'
               || input.indicator_1 == 'Average weekly earnings (full-time)'",
               #'NOTE [indicators 1:2/8]
               ###### Visible Minority ----
               #'NOTE [this is the focal variable for this tab]
               pickerInput(
                 inputId = "lm_income_vismin", # name this for the server
                 label = "Choose a visible minority status", # label of filter
                 choices = as.character(unique(incomeDT$VisMin)), # create drop-down list option
                 multiple = TRUE, # multi-select
                 selected = as.character(unique(incomeDT$VisMin))[1],
                 options = list(
                   `actions-box` = TRUE,
                   `deselect-all-text` = "Deselect all",
                   `select-all-text` = "Select all"
                 )),
               ###### Year ----
               pickerInput(
                 inputId = "lm_income_year", # name this for the server
                 label = "Choose a year of reference", # label of filter
                 choices = sort(unique(incomeDT$Year), decreasing = TRUE), # create drop-down list option
                 selected = sort(unique(incomeDT$Year), decreasing = TRUE)[1],
                 multiple = TRUE), # multi-select
               ###### Degree ----
               selectizeInput(
                 inputId = "lm_income_degree",
                 label = "Choose a highest certificate, diploma or degree",
                 choices = unique(as.character(incomeDT$Degree))
               ),
               ###### Geography ----
               selectizeInput(
                 inputId = "lm_income_geography",
                 label = "Choose a geography",
                 choices = unique(as.character(incomeDT$Geography))
               ),
               ###### Immigration ----
               selectizeInput(
                 inputId = "lm_income_immigration",
                 label = "Choose an immigrant or generation status",
                 choices = unique(as.character(incomeDT$Immigration))
               ),
               ###### Age ----
               selectizeInput(
                 inputId = "lm_income_age",
                 label = "Choose an age group or first official language spoken",
                 choices = unique(as.character(incomeDT$Age))
               ),
               ###### Sex ----
               selectizeInput(
                 inputId = "lm_income_sex",
                 label = "Choose a sex",
                 choices = unique(as.character(incomeDT$Sex))
               )
              ),
        
          ), # sidebarPanel closing bracket // should be blue

          ### Main panel ----
          mainPanel(
            #h2("Groups Designated as Visible Minorities"),

            #'NOTE [EXAMPLE OF PREVIOUS CODE]
            # conditionalPanel(
            #   condition = "input.dim == 'Health and wellbeing' & input.dimHealth == 'Perceived mental health, very good or excellent' & input.healthCharacteristics == 'Immigration Status'",
            #   br(),
            #   br(),
            #   plotlyOutput("sBarHealth3",
            #                inline = TRUE,
            #                width = 700,
            #                height = 500),
            #   br(),
            #   helpText("Source: Canadian Community Health Survey (CCHS), September to December 2020")
            # )

            #### 1. Participation in the Labour Market ----
            ##### 1.1. Participation rate in the Labour Market ----
            conditionalPanel(
              condition = "input.indicator_1 == 'Participation rate in the Labour Market'",
              h4("Working-age population in the labour force (participation rate)"),
              br(),
              br(),
              plotlyOutput("plot_vm_lm_1",
                           inline = TRUE),
              br(),
              helpText(source_census_nhs_census)
            ),
            ##### 1.2. Employment rate -----
            conditionalPanel(
              condition = "input.indicator_1 == 'Employment rate'",
              h4("Working-age population in employment (employment rate)"),
              br(),
              br(),
              plotlyOutput("plot_vm_lm_2",
                           inline = TRUE),
              br(),
              helpText(source_census_nhs_census)
            ),

            ##### 1.3. Unemployment rate ----
            conditionalPanel(
              condition = "input.indicator_1 == 'Unemployment rate'",
              h4("Working-age population in unemployment (unemployment rate)"),
              br(),
              br(),
              plotlyOutput("plot_vm_lm_3",
                           inline = TRUE),
              br(),
              helpText(source_census_nhs_census)
            ),

            ##### 1.4. Population in full-time employment (Population in full-time employment) ----
            conditionalPanel(
              condition = "input.indicator_1 == 'Population in full-time employment'",
              h4("Workers working mainly full-time weeks in the previous year"),
              br(),
              br(),
              plotlyOutput("plot_vm_lm_4",
                           inline = TRUE),
              br(),
              helpText(source_census_nhs_census)
            ),

            ##### 1.5. Population in self-employment (unincorporated) ----
            conditionalPanel(
              condition = "input.indicator_1 == 'Population in self-employment (unincorporated)'",
              h4("Self-employed workers in the labour force (unincorporated)"),
              br(),
              br(),
              plotlyOutput("plot_vm_lm_5",
                           inline = TRUE),
              br(),
              helpText(source_census_nhs_census)
            ),

            ##### 1.6. Population in overqualification situation ----
            conditionalPanel(
              condition = "input.indicator_1 == 'Population in overqualification situation'",
              h4("Overqualified workers with a university degree"),
              br(),
              br(),
              plotlyOutput("plot_vm_lm_6",
                           inline = TRUE),
              br(),
              helpText(source_census_nhs)
            ),

            ##### 1.7. Youth not in employment, education or training ----
            conditionalPanel(
              condition = "input.indicator_1 == 'Youth not in employment, education or training'",
              h4("Youth not in employment, education or training (NEET)"),
              br(),
              br(),
              plotlyOutput("plot_vm_lm_7",
                           inline = TRUE),
              br(),
              helpText(source_census_nhs_census)
            ),
            #### 2. Civic engagement and political participation ----
            ##### 2.1. Member or participant of at least one group, organization or association ----
            conditionalPanel(
              condition = "input.indicator_1 == 'Member or participant of at least one group, organization or association'",
              h4("Percent of the population members of at least one civic group or organization"),
              br(),
              br(),
              plotlyOutput("plot_vm_civic_1",
                           inline = TRUE),
              br(),
              helpText(source_gss)
            ),

            ##### 2.2. Member or participant in sports or recreational organization ----
            conditionalPanel(
              condition = "input.indicator_1 == 'Member or participant in sports or recreational organization'",
              h4("Percent of the population members in a sports or recreational organization"),
              br(),
              br(),
              plotlyOutput("plot_vm_civic_2",
                           inline = TRUE),
              br(),
              helpText(source_gss)
            ),

            ##### 2.3. Member or participant in cultural, educational or hobby organization ----
            conditionalPanel(
              condition = "input.indicator_1 == 'Member or participant in cultural, educational or hobby organization'",
              h4("Percent of the population members in a cultural, educational or hobby organization)"),
              br(),
              br(),
              plotlyOutput("plot_vm_civic_3",
                           inline = TRUE),
              br(),
              helpText(source_gss)
            ),

            ##### 2.4. Member or participant in union or professional association ----
            conditionalPanel(
              condition = "input.indicator_1 == 'Member or participant in union or professional association'",
              h4("Percent of the population members in union or professional association"),
              br(),
              br(),
              plotlyOutput("plot_vm_civic_4",
                           inline = TRUE),
              br(),
              helpText(source_gss)
            ),

            ##### 2.5. Member or participant in political party or group ----
            conditionalPanel(
              condition = "input.indicator_1 == 'Member or participant in political party or group'",
              h4("Percent of the population members in a political party or group"),
              br(),
              br(),
              plotlyOutput("plot_vm_civic_5",
                           inline = TRUE),
              br(),
              helpText(source_gss)
            ),

            ##### 2.6. Member or participant in religious-affiliated group ----
            conditionalPanel(
              condition = "input.indicator_1 == 'Member or participant in religious-affiliated group'",
              h4("PPercent of the population members in a religious-affiliated group"),
              br(),
              br(),
              plotlyOutput("plot_vm_civic_6",
                           inline = TRUE),
              br(),
              helpText(source_gss)
            ),

            ##### 2.7. Member or participant in school group, neighbourhood, civic or community association ----
            conditionalPanel(
              condition = "input.indicator_1 == 'Member or participant in school group, neighbourhood, civic or community association'",
              h4("Percent of the population members in a school group, neighbourhood, civic or community association"),
              br(),
              br(),
              plotlyOutput("plot_vm_civic_7",
                           inline = TRUE),
              br(),
              helpText(source_gss)
            ),

            ##### 2.8. Member or participant in humanitarian or charitable organization or service club ----
            conditionalPanel(
              condition = "input.indicator_1 == 'Member or participant in humanitarian or charitable organization or service club'",
              h4("Percent of the population members in a humanitarian or charitable organization or service club"),
              br(),
              br(),
              plotlyOutput("plot_vm_civic_8",
                           inline = TRUE),
              br(),
              helpText(source_gss)
            ),

            ##### 2.9. Member or participant in seniors' group ----
            conditionalPanel(
              condition = "input.indicator_1 == 'Member or participant in seniors\\' group'",
              h4("Percent of the population members in a seniors' group"),
              br(),
              br(),
              plotlyOutput("plot_vm_civic_9",
                           inline = TRUE),
              br(),
              helpText(source_gss)
            ),

            ##### 2.10. Member or participant in youth organization ----
            conditionalPanel(
              condition = "input.indicator_1 == 'Member or participant in youth organization'",
              h4("Percent of the population members in a youth organization"),
              br(),
              br(),
              plotlyOutput("plot_vm_civic_10",
                           inline = TRUE),
              br(),
              helpText(source_gss)
            ),

            ##### 2.11. Member or participant in immigrant or ethnic association or club ----
            conditionalPanel(
              condition = "input.indicator_1 == 'Member or participant in immigrant or ethnic association or club'",
              h4("Percent of the population members in an immigrant or ethnic association or club"),
              br(),
              br(),
              plotlyOutput("plot_vm_civic_11",
                           inline = TRUE),
              br(),
              helpText(source_gss)
            ),

            ##### 2.12. Member or participant in environmental group ----
            conditionalPanel(
              condition = "input.indicator_1 == 'Member or participant in environmental group'",
              h4("Percent of the population members in an environmental group "),
              br(),
              br(),
              plotlyOutput("plot_vm_civic_12",
                           inline = TRUE),
              br(),
              helpText(source_gss)
            ),

            ##### 2.13. Engaged in political activities ----
            conditionalPanel(
              condition = "input.indicator_1 == 'Engaged in political activities'",
              h4("Percent of the population engaged in political activities"),
              br(),
              br(),
              plotlyOutput("plot_vm_civic_13",
                           inline = TRUE),
              br(),
              helpText(source_gss)
            ),

            ##### 2.14 Voted in last federal election ----
            conditionalPanel(
              condition = "input.indicator_1 == 'Voted in last federal election'",
              h4("Percent of the population voting in the last federal election"),
              br(),
              br(),
              plotlyOutput("plot_vm_civic_14",
                           inline = TRUE),
              br(),
              helpText(source_gss)
            ),

            ##### 2.15 Voted in last provincial election ----
            conditionalPanel(
              condition = "input.indicator_1 == 'Voted in last provincial election'",
              h4("Percent of the population voting in the last provincial election"),
              br(),
              br(),
              plotlyOutput("plot_vm_civic_15",
                           inline = TRUE),
              br(),
              helpText(source_gss)
            ),

            ##### 2.16 Voted in last municipal election ----
            conditionalPanel(
              condition = "input.indicator_1 == 'Voted in last municipal election'",
              h4("Percent of the population voting in the last municipal election"),
              br(),
              br(),
              plotlyOutput("plot_vm_civic_16",
                           inline = TRUE),
              br(),
              helpText(source_gss)
            ),

            #### 3. Representation in decision-making positions ----
            ##### 3.1. Workers in all management occupations ----
            conditionalPanel(
              condition = "input.indicator_1 == 'Workers in all management occupations'",
              h4("Percent of workers in all management occupations"),
              br(),
              br(),
              plotlyOutput("plot_vm_rep_1",
                           inline = TRUE),
              br(),
              helpText(source_census_nhs_census)
            ),

            ##### 3.2. Workers in senior management occupations ----
            conditionalPanel(
              condition = "input.indicator_1 == 'Workers in senior management occupations'",
              h4("Percent of workers in senior management occupations"),
              br(),
              br(),
              plotlyOutput("plot_vm_rep_2",
                           inline = TRUE),
              br(),
              helpText(source_census_nhs_census)
            ),

            #'NOTE [WHY IS THIS SEPARATED?]
            ##### 3.3. Specialized middle management occupations ----
            conditionalPanel(
              condition = "input.indicator_1 == 'Specialized middle management occupations'",
            h4("Percent of workers in specialized middle management occupations"),
              br(),
              br(),
              plotlyOutput("plot_vm_rep_3",
                           inline = TRUE),
              br(),
              helpText(source_census_nhs_census)
            ),

            

            #### 4. Basic needs and housing ----
            ##### 4.1. Population living in a dwelling owned by one member of the household ----
            # conditionalPanel(
            #   condition = "input.indicator_1 == 'Population living in a dwelling owned by one member of the household'",
            #h4("Population living in a dwelling owned by one member of the household"),
            #   br(),
            #   br(),
            #   plotlyOutput("plot_vm_basic_1",
            #                inline = TRUE),
            #   br(),
            #   helpText(source_cchs)
            # ),

            ##### 4.2. Population living in core need housing ----
            # conditionalPanel(
            #   condition = "input.indicator_1 == 'Population living in core need housing'",
            #h4("Population living in core need housing"),
            #   br(),
            #   br(),
            #   plotlyOutput("plot_vm_basic_2",
            #                inline = TRUE),
            #   br(),
            #   helpText(source_cchs)
            # ),

            ##### 4.3. Population living in suitable housing ----
            # conditionalPanel(
            #   condition = "input.indicator_1 == 'Population living in suitable housing'",
            #h4("Population living in suitable housing"),
            #   br(),
            #   br(),
            #   plotlyOutput("plot_vm_basic_3",
            #                inline = TRUE),
            #   br(),
            #   helpText(source_cchs)
            # ),

            ##### 4.4. Population living in an affordable housing ----
            # conditionalPanel(
            #   condition = "input.indicator_1 == 'Population living in an affordable housing'",
            #h4("Population living in an affordable housing"),
            #   br(),
            #   br(),
            #   plotlyOutput("plot_vm_basic_4",
            #                inline = TRUE),
            #   br(),
            #   helpText(source_cchs)
            # ),

            ##### 4.5. Household food secure----
            conditionalPanel(
              condition = "input.indicator_1 == 'Household food secure'",
              h4("Percent of the population living in a food-secure household"),
              br(),
              br(),
              plotlyOutput("plot_vm_basic_5",
                           inline = TRUE),
              br(),
              helpText(source_cchs)
            ),

            ##### 4.6. Household marginally food insecure ----
            conditionalPanel(
              condition = "input.indicator_1 == 'Household marginally food insecure'",
              h4("Percent of the population living in a household with marginal food security"),
              br(),
              br(),
              plotlyOutput("plot_vm_basic_6",
                           inline = TRUE),
              br(),
              helpText(source_cchs)
            ),

            ##### 4.7. Household moderately or severely food insecure ----
            conditionalPanel(
              condition = "input.indicator_1 == 'Household moderately or severely food insecure'",
              h4("Percent of the population living in a food-insecure household, moderate or severe"),
              br(),
              br(),
              plotlyOutput("plot_vm_basic_7",
                           inline = TRUE),
              br(),
              helpText(source_cchs)
            ),

            ##### 4.8. Household moderately food insecure ----
            conditionalPanel(
              condition = "input.indicator_1 == 'Household moderately food insecure'",
              h4("Percent of the population living in a household with moderate food insecurity"),
              br(),
              br(),
              plotlyOutput("plot_vm_basic_8",
                           inline = TRUE),
              br(),
              helpText(source_cchs)
            ),

            ##### 4.9. Household severely food insecure ----
            conditionalPanel(
              condition = "input.indicator_1 == 'Household severely food insecure'",
              h4("Percent of the population living in a household with severe food insecurity"),
              br(),
              br(),
              plotlyOutput("plot_vm_basic_9",
                           inline = TRUE),
              br(),
              helpText(source_cchs)
            ),

            #### 5. Local community ----
            #'NOTE [TBD]

            #### 6. Health and wellbeing ----
            ##### 6.1. Perceived health, very good or excellent ----
            conditionalPanel(
              condition = "input.indicator_1 == 'Perceived health, very good or excellent'",
              h4("Percent of the population reporting very good or excellent general health"),
              br(),
              br(),
              plotlyOutput("plot_vm_health_1",
                           inline = TRUE),
              br(),
              helpText(source_cchs)
            ),

            ##### 6.2. Perceived health, fair or poor ----
            conditionalPanel(
              condition = "input.indicator_1 == 'Perceived health, fair or poor'",
              h4("Percent of the population reporting fair or poor general health"),
              br(),
              br(),
              plotlyOutput("plot_vm_health_2",
                           inline = TRUE),
              br(),
              helpText(source_cchs)
            ),

            ##### 6.3. Perceived mental health, very good or excellent ----
            conditionalPanel(
              condition = "input.indicator_1 == 'Perceived mental health, very good or excellent'",
              h4("Percent of the population reporting very good or excellent mental health"),
              br(),
              br(),
              plotlyOutput("plot_vm_health_3",
                           inline = TRUE),
              br(),
              helpText(source_cchs)
            ),

            ##### 6.4. Perceived mental health, fair or poor ----
            conditionalPanel(
              condition = "input.indicator_1 == 'Perceived mental health, fair or poor'",
              h4("Percent of the population reporting fair or poor mental health"),
              br(),
              br(),
              plotlyOutput("plot_vm_health_4",
                           inline = TRUE),
              br(),
              helpText(source_cchs)
            ),

            ##### 6.5. Perceived life stress, most days quite a bit or extremely stressful ----
            conditionalPanel(
              condition = "input.indicator_1 == 'Perceived life stress, most days quite a bit or extremely stressful'",
              h4("Percent of the population reporting their life stressful"),
              br(),
              br(),
              plotlyOutput("plot_vm_health_5",
                           inline = TRUE),
              br(),
              helpText(source_cchs)
            ),
            ##### 6.6. Life satisfaction, satisfied or very satisfied ----
            conditionalPanel(
              condition = "input.indicator_1 == 'Life satisfaction, satisfied or very satisfied'",
              h4("Percent of the population reporting life satisfaction, satisfied or very satisfied"),
              br(),
              br(),
              plotlyOutput("plot_vm_health_6",
                           inline = TRUE),
              br(),
              helpText(source_cchs)
            ),

            #### 7. Public services and institutions ----
            ##### 7.1. Confidence in Federal Parliament ----
            conditionalPanel(
              condition = "input.indicator_1 == 'Confidence in Federal Parliament'",
              h4("Population expressing confidence in Federal Parliament"),
              br(),
              br(),
              plotlyOutput("plot_vm_public_1",
                           inline = TRUE),
              br(),
              helpText(source_gss)
            ),

            ##### 7.2. Confidence in the Canadian media ----
            conditionalPanel(
              condition = "input.indicator_1 == 'Confidence in the Canadian media'",
              h4("Population expressing confidence in the Canadian media"),
              br(),
              br(),
              plotlyOutput("plot_vm_public_2",
                           inline = TRUE),
              br(),
              helpText(source_gss)
            ),

            ##### 7.3. Confidence in the school system ----
            conditionalPanel(
              condition = "input.indicator_1 == 'Confidence in the school system'",
              h4("Population expressing confidence in the school system"),
              br(),
              br(),
              plotlyOutput("plot_vm_public_3",
                           inline = TRUE),
              br(),
              helpText(source_gss)
            ),

            ##### 7.4. Confidence in the justice system and courts ----
            conditionalPanel(
              condition = "input.indicator_1 == 'Confidence in the justice system and courts'",
              h4("Population expressing confidence in the justice system and courts"),
              br(),
              br(),
              plotlyOutput("plot_vm_public_4",
                           inline = TRUE),
              br(),
              helpText(source_gss)
            ),

            ##### 7.5. Confidence in the police service ----
            conditionalPanel(
              condition = "input.indicator_1 == 'Confidence in the police service'",
              h4("Population expressing confidence in the police service"),
              br(),
              br(),
              plotlyOutput("plot_vm_public_5",
                           inline = TRUE),
              br(),
              helpText(source_gss)
            ),

            ##### 7.6. Confidence in major corporations ----
            conditionalPanel(
              condition = "input.indicator_1 == 'Confidence in major corporations'",
              h4("Population expressing confidence in major corporations"),
              br(),
              br(),
              plotlyOutput("plot_vm_public_6",
                           inline = TRUE),
              br(),
              helpText(source_gss)
            ),

            ##### 7.7. Confidence in merchants and local business people ----
            conditionalPanel(
              condition = "input.indicator_1 == 'Confidence in merchants and local business people'",
              h4("Population expressing confidence in merchants and local business people"),
              br(),
              br(),
              plotlyOutput("plot_vm_public_7",
                           inline = TRUE),
              br(),
              helpText(source_gss)
            ),

            ##### 7.8. Confidence in banks ----
            conditionalPanel(
              condition = "input.indicator_1 == 'Confidence in banks'",
              h4("Population expressing confidence in banks"),
              br(),
              br(),
              plotlyOutput("plot_vm_public_8",
                           inline = TRUE),
              br(),
              helpText(source_gss)
            ),
            ##### 7.9. Has a regular health care provider ----
            conditionalPanel(
              condition = "input.indicator_1 == 'Has a regular health care provider'",
              h4("Percent of the population reporting having a regular healthcare provider"),
              br(),
              br(),
              plotlyOutput("plot_vm_health_7",
                           inline = TRUE),
              br(),
              helpText(source_cchs)
            ),
            ##### 7.10. Perceived need for mental health care, no need ----
            conditionalPanel(
              condition = "input.indicator_1 == 'Perceived need for mental health care, no need'",
              h4("Percent of the population reporting no need for mental health care"),
              br(),
              br(),
              plotlyOutput("plot_vm_health_8",
                           inline = TRUE),
              br(),
              helpText(source_cchs)
            ),
            ##### 7.11. Perceived need for mental health care, all needs met ----
            conditionalPanel(
              condition = "input.indicator_1 == 'Perceived need for mental health care, all needs met'",
              h4("Percent of the population reporting all needs met for mental health care"),
              br(),
              br(),
              plotlyOutput("plot_vm_health_9",
                           inline = TRUE),
              br(),
              helpText(source_cchs)
            ),
            ##### 7.12. Perceived need for mental health care, needs partially met ----
            conditionalPanel(
              condition = "input.indicator_1 == 'Perceived need for mental health care, needs partially met'",
              h4("Percent of the population reporting needs partially met for mental health care"),
              br(),
              br(),
              plotlyOutput("plot_vm_health_10",
                           inline = TRUE),
              br(),
              helpText(source_cchs)
            ),
            ##### 7.13. Perceived need for mental health care, needs partially met or needs not met ----
            conditionalPanel(
              condition = "input.indicator_1 == 'Perceived need for mental health care, needs partially met or needs not met'",
              h4("Percent of the population reporting needs partially met or needs not met for mental health care"),
              br(),
              br(),
              plotlyOutput("plot_vm_health_11",
                           inline = TRUE),
              br(),
              helpText(source_cchs)
            ),
            ##### 7.14. Perceived need for mental health care, needs not met ----
            conditionalPanel(
              condition = "input.indicator_1 == 'Perceived need for mental health care, needs not met'",
              h4("Percent of the population reporting needs not met for mental health care"),
              br(),
              br(),
              plotlyOutput("plot_vm_health_12",
                           inline = TRUE),
              br(),
              helpText(source_cchs)
            ),
            ##### 7.15. Unmet health care needs ----
            conditionalPanel(
              condition = "input.indicator_1 == 'Unmet health care needs'",
              h4("Percent of the population reporting unmet health care needs"),
              br(),
              br(),
              plotlyOutput("plot_vm_health_13",
                           inline = TRUE),
              br(),
              helpText(source_cchs)
            ),
            #### 8. Education and training skills ----
            ##### 8.1. Population with no certificate, diploma or degree ----
            conditionalPanel(
              condition = "input.indicator_1 == 'Population with no certificate, diploma or degree'",
              h4("Population with no certificate, diploma or degree"),
              br(),
              br(),
              plotlyOutput("plot_vm_education_1",
                           inline = TRUE),
              br(),
              helpText(source_census_nhs_census)
            ),

            ##### 8.2. Population with high school diploma or equivalency certificate ----
            conditionalPanel(
              condition = "input.indicator_1 == 'Population with high school diploma or equivalency certificate'",
              h4("Working-age population in the labour force (participation rate)"),
              br(),
              br(),
              plotlyOutput("plot_vm_education_2",
                           inline = TRUE),
              br(),
              helpText(source_census_nhs_census)
            ),
            ##### 8.3. Population with postsecondary certificate or diploma below bachelor level ----
            conditionalPanel(
              condition = "input.indicator_1 == 'Population with postsecondary certificate or diploma below bachelor level'",
              h4("Population with postsecondary certificate or diploma below bachelor level"),
              br(),
              br(),
              plotlyOutput("plot_vm_education_3",
                           inline = TRUE),
              br(),
              helpText(source_census_nhs_census)
            ),
            ##### 8.4. Population with university certificate or diploma above bachelor level ----
            conditionalPanel(
              condition = "input.indicator_1 == 'Population with university certificate or diploma above bachelor level'",
              h4("Population with university certificate or diploma above bachelor level"),
              br(),
              br(),
              plotlyOutput("plot_vm_education_4",
                           inline = TRUE),
              br(),
              helpText(source_census_nhs_census)
            ),
            ##### 8.5. Population with bachelor's degree ----
            conditionalPanel(
              condition = "input.indicator_1 == 'Population with bachelor\\'s degree'",
              h4("Population with bachelor's degree"),
              br(),
              br(),
              plotlyOutput("plot_vm_education_5",
                           inline = TRUE),
              br(),
              helpText(source_census_nhs_census)
            ),
            ##### 8.6. Population with university certificate or diploma or degree at bachelor level or above ----
            conditionalPanel(
              condition = "input.indicator_1 == 'Population with university certificate or diploma or degree at bachelor level or above'",
              h4("Population with university certificate or diploma or degree at bachelor level or above"),
              br(),
              br(),
              plotlyOutput("plot_vm_education_6",
                           inline = TRUE),
              br(),
              helpText(source_census_nhs_census)
            ),
            #### 9. Social connections and personnal networks ----
            ##### 9.1. Population living alone----
            # conditionalPanel(
            #   condition = "input.indicator_1 == 'Population living alone'",
            #h4("Percent of the population living alone"),
            #   br(),
            #   br(),
            #   plotlyOutput("plot_vm_social_1",
            #                inline = TRUE),
            #   br(),
            #   helpText(source_gss)
            # ),
            
            ##### 9.4. Having no one in the local personal network (close relatives and close friends) ----
            # conditionalPanel(
            #   condition = "input.indicator_1 == 'Having no one in the local personal network (close relatives and close friends)'",
            #h4("Percent of the population having no one in the local personal network (close relatives and close friends)"),
            #   br(),
            #   br(),
            #   plotlyOutput("plot_vm_social_4",
            #                inline = TRUE),
            #   br(),
            #   helpText(source_gss)
            # ),

            ##### 9.5. Having 1 to 14 people in the local personal network (close relatives and close friends) ----
            # conditionalPanel(
            #   condition = "input.indicator_1 == 'Having 1 to 14 people in the local personal network (close relatives and close friends)'",
            #h4("Percent of the population having 1 to 14 people in the local personal network (close relatives and close friends"),
            #   br(),
            #   br(),
            #   plotlyOutput("plot_vm_social_5",
            #                inline = TRUE),
            #   br(),
            #   helpText(source_gss)
            # ),

            ##### 9.6. Having 15 or more people in the local personal network (close relatives and close friends) ----
            # conditionalPanel(
            #   condition = "input.indicator_1 == 'Having 15 or more people in the local personal network (close relatives and close friends)'",
            #h4("Percent of the population having 15 or more people in the local personal network (close relatives and close friends)"),
            #   br(),
            #   br(),
            #   plotlyOutput("plot_vm_social_6",
            #                inline = TRUE),
            #   br(),
            #   helpText(source_gss)
            # ),

            ##### 9.7. Feeling close to 4 relatives or less ----
            # conditionalPanel(
            #   condition = "input.indicator_1 == 'Feeling close to 4 relatives or less'",
            #h4("Percent of the population feeling close to 4 relatives or less"),
            #   br(),
            #   br(),
            #   plotlyOutput("plot_vm_social_7",
            #                inline = TRUE)
            #   br(),
            #   helpText(source_gss)
            # ),

            ##### 9.8. Having 4 close friends or less----
            # conditionalPanel(
            #   condition = "input.indicator_1 == 'Having 4 close friends or less'",
            #h4("Percent of the population having 4 close friends or less"),
            #   br(),
            #   br(),
            #   plotlyOutput("plot_vm_social_8",
            #                inline = TRUE),
            #   br(),
            #   helpText(source_gss)
            # ),

            ##### 9.9. Feeling close to 5 relatives or more ----
            # conditionalPanel(
            #   condition = "input.indicator_1 == 'Feeling close to 5 relatives or more'",
            #h4("Percent of the population feeling close to 5 relatives or more "),
            #   br(),
            #   br(),
            #   plotlyOutput("plot_vm_social_9",
            #                inline = TRUE),
            #   br(),
            #   helpText(source_gss)
            # ),

            ##### 9.10. Having ethnically diverse networks of friends and acquaintances ----
            # conditionalPanel(
            #   condition = "input.indicator_1 == 'Having ethnically diverse networks of friends and acquaintances'",
            #h4("Percent of the population with a personal ethnically-diverse network"),
            #   br(),
            #   br(),
            #   plotlyOutput("plot_vm_social_10",
            #                inline = TRUE),
            #   br(),
            #   helpText(source_gss)
            # ),
            
            ##### 9.11. Reported that most people can be trusted in general ----
            conditionalPanel(
              condition = "input.indicator_1 == 'Reported that most people can be trusted in general'",
              h4("Population reporting that most people can be trusted"),
              br(),
              br(),
              plotlyOutput("plot_vm_social_11",
                           inline = TRUE),
              br(),
              helpText(source_gss)
            ),
            ##### 9.12. Strong sense of belonging to the local community ----
            conditionalPanel(
              condition = "input.indicator_1 == 'Strong sense of belonging to the local community'",
              h4("Population reporting strong sense of belonging to their local community"),
              br(),
              br(),
              plotlyOutput("plot_vm_social_12",
                           inline = TRUE),
              br(),
              helpText(source_gss)
            ),
            ##### 9.13. Strong sense of belonging to the town or city ----
            conditionalPanel(
              condition = "input.indicator_1 == 'Strong sense of belonging to the town or city'",
              h4("Population reporting strong sense of belonging to their town or city"),
              br(),
              br(),
              plotlyOutput("plot_vm_social_13",
                           inline = TRUE),
              br(),
              helpText(source_gss )
            ),
            ##### 9.14. Strong sense of belonging to the province ----
            conditionalPanel(
              condition = "input.indicator_1 == 'Strong sense of belonging to the province'",
              h4("Population reporting strong sense of belonging to their province"),
              br(),
              br(),
              plotlyOutput("plot_vm_social_14",
                           inline = TRUE),
              br(),
              helpText(source_gss)
            ),
            ##### 9.15. Strong sense of belonging to Canada ----
            conditionalPanel(
              condition = "input.indicator_1 == 'Strong sense of belonging to Canada'",
              h4("Population reporting strong sense of belonging to Canada"),
              br(),
              br(),
              plotlyOutput("plot_vm_social_15",
                           inline = TRUE),
              br(),
              helpText(source_gss)
            ),
            
            ##### 9.16. Having no other friends or acquaintances ----
            # conditionalPanel(
            #   condition = "input.indicator_1 == 'Having no other friends or acquaintances'",
            #h4("Percent of the population having no other friends or acquaintances"),
            #   br(),
            #   br(),
            #   plotlyOutput("plot_vm_social_16",
            #                inline = TRUE),
            #   br(),
            #   helpText(source_gss)
            # ),
            ##### 9.17. Having 1 to 19 other friends or acquaintances ----
            # conditionalPanel(
            #   condition = "input.indicator_1 == 'Having 1 to 19 other friends or acquaintances'",
            #h4("Percent of the population having 1 to 19 other friends or acquaintances"),
            #   br(),
            #   br(),
            #   plotlyOutput("plot_vm_social_17",
            #                inline = TRUE),
            #   br(),
            #   helpText(source_gss)
            # ),
            ##### 9.18. Having 20 or more other friends or acquaintances ----
            # conditionalPanel(
            #   condition = "input.indicator_1 == 'Having 20 or more other friends or acquaintances'",
            #h4("Percent of the population having 20 or more other friends or acquaintances"),
            #   br(),
            #   br(),
            #   plotlyOutput("plot_vm_social_18",
            #                inline = TRUE),
            #   br(),
            #   helpText(source_gss)
            # ),
            

            #### 10. Discrimination and victimization ----
            ##### 10.1. Experience(s) of discrimination ----
            conditionalPanel(
              condition = "input.indicator_1 == 'Experience(s) of discrimination'",
              h4("Experience(s) of discrimination"),
              br(),
              br(),
              plotlyOutput("plot_vm_discrimination_1",
                           inline = TRUE),
              br(),
              helpText(source_gss)
            ),

            ##### 10.2. Experience(s) of discrimination based on ethnicity or culture ----
            conditionalPanel(
              condition = "input.indicator_1 == 'Experience(s) of discrimination based on ethnicity or culture'",
              h4("Experience(s) of discrimination based on ethnicity or culture"),
              br(),
              br(),
              plotlyOutput("plot_vm_discrimination_2",
                           inline = TRUE),
              br(),
              helpText(source_gss)
            ),

            ##### 10.3. Experience(s) of discrimination based on race or colour ----
            conditionalPanel(
              condition = "input.indicator_1 == 'Experience(s) of discrimination based on race or colour'",
              h4("Experience(s) of discrimination based on race or colour"),
              br(),
              br(),
              plotlyOutput("plot_vm_discrimination_3",
                           inline = TRUE),
              br(),
              helpText(source_gss)
            ),

            ##### 10.4. Experience(s) of discrimination based on religion ----
            conditionalPanel(
              condition = "input.indicator_1 == 'Experience(s) of discrimination based on religion'",
              h4("Experience(s) of discrimination based on religion"),
              br(),
              br(),
              plotlyOutput("plot_vm_discrimination_4",
                           inline = TRUE),
              br(),
              helpText(source_gss)
            ),

            ##### 10.5. Experience(s) of discrimination based on language ----
            conditionalPanel(
              condition = "input.indicator_1 == 'Experience(s) of discrimination based on language'",
              h4("Experience(s) of discrimination based on language"),
              br(),
              br(),
              plotlyOutput("plot_vm_discrimination_5",
                           inline = TRUE),
              br(),
              helpText(source_gss)
            ),

            ##### 10.6. Discrimination at work or when applying for a job or promotion ----
            conditionalPanel(
              condition = "input.indicator_1 == 'Discrimination at work or when applying for a job or promotion'",
              h4("Discrimination at work or when applying for a job or promotion"),
              br(),
              br(),
              plotlyOutput("plot_vm_discrimination_6",
                           inline = TRUE),
              br(),
              helpText(source_gss)
            ),

            ##### 10.7. Discrimination when dealing with the police ----
            conditionalPanel(
              condition = "input.indicator_1 == 'Discrimination when dealing with the police'",
              h4("Discrimination when dealing with the police"),
              br(),
              br(),
              plotlyOutput("plot_vm_discrimination_7",
                           inline = TRUE),
              br(),
              helpText(source_gss)
            ),

            ##### 10.8. Discrimination when in a store, bank or restaurant ----
            conditionalPanel(
              condition = "input.indicator_1 == 'Discrimination when in a store, bank or restaurant'",
              h4("Discrimination when in a store, bank or restaurant"),
              br(),
              br(),
              plotlyOutput("plot_vm_discrimination_8",
                           inline = TRUE),
              br(),
              helpText(source_gss)
            ),

            ##### 10.9. Discrimination when attending school or classes ----
            conditionalPanel(
              condition = "input.indicator_1 == 'Discrimination when attending school or classes'",
              h4("Discrimination when attending school or classes"),
              br(),
              br(),
              plotlyOutput("plot_vm_discrimination_9",
                           inline = TRUE),
              br(),
              helpText(source_gss)
            ),
            ##### 10.10.  Hate crime ----
             conditionalPanel(
              condition = "input.indicator_1 == 'Police-reported hate crimes motivated by religion and race or ethnicity'",
              h4("Number of police-reported hate crimes motivated by religion and race or ethnicity"),
              br(),
              br(),
              plotlyOutput("plot_vm_hate_crime",
                              inline = TRUE),
              br(),
              helpText(source_census_nhs_census)
             ),
             
            #### 11. Income and wealth ----
            #'NOTE [TBD because the incomeDT was used in the Participation in the Labour Market section]
            ##### 11.1. Average employment income ----
            conditionalPanel(
              condition = "input.indicator_1 == 'Average employment income'",
              h4("Average employment income of the population"),
              br(),
              br(),
              plotlyOutput("plot_vm_inc_1",
                           inline = TRUE),
              br(),
              helpText(source_census_nhs_census)
            ),

            ##### 11.2. Average weekly earnings (full-time) ----
            conditionalPanel(
              condition = "input.indicator_1 == 'Average weekly earnings (full-time)'",
              h4("Average weekly wage of paid employees"),
              br(),
              br(),
              plotlyOutput("plot_vm_inc_2",
                           inline = TRUE),
              br(),
              helpText(source_census_nhs_census)
            ),

          ) # Main panel closing bracket // should be blue

        ) # sidebarLayout closing bracket // should be greenish-blue
      ),
      #'NOTE [END OF VISMIN TAB]

      #'NOTE [HERE IS WHERE YOU WOULD ADD A NEW TAB // use what's in the Visible Minority tab as a reference]
      
      # Geography ------
      tabPanel(
        "Geography",
        fluid = TRUE,
        fluidRow(
          column (
          width = 2,
       
          tags$iframe(src = "https://sgcdev.stc.ca/dev/csge-main/index-emb-en.html",width = "655%", height = "805px"),
                      
          ),

        ),
      ),

 
 
    )
  )


gc()
