# Install packages ----
package_list <- c(
  "arrow",
  "dplyr",
  "stringr",
  "scales",
  "plotly",
  "shiny",
  "shinyWidgets"
)

new_packages <-
  package_list[!(package_list %in% installed.packages()[, "Package"])]
if (length(new_packages))
  install.packages(new_packages)

rm(package_list, new_packages)

# Load libraries ----
library(arrow)
library(dplyr)
library(stringr)
library(scales)
library(plotly)
library(shiny)
library(shinyWidgets)

# Import data ----
## Datasets ----
geo_filter <-
  c(
    "Canada",
    "Newfoundland and Labrador",
    "Prince Edward Island Nova Scotia",
    "Nova Scotia",
    "New Brunswick",
    "Quebec region",
    "Ontario region",
    "Manitoba",
    "Saskatchewan",
    "Alberta",
    "British Columbia region",
    "Northwest Territories",
    "Yukon",
    "Nunavut",
    "Atlantic provinces",
    "Ontario",
    "Quebec",
    "Prairie provinces",
    "British Columbia",
    "Canada, selected police services",
    "Canada (selected provinces - see notes)",

    "Toronto, Ontario",
    "Montréal, Quebec",
    "Vancouver, British Columbia",
    "Ottawa-Gatineau, Ontario/Quebec",
    "Calgary, Alberta",
    "Edmonton, Alberta",
    "Quebec, Quebec",
    "Winnipeg, Manitoba",
    "Hamilton, Ontario",
    "Kitchener–Cambridge–Waterloo, Ontario",
    "London, Ontario"

) # can only retrieve at the provincial level because otherwise it costs too much memory

#' NOTE [you can use names() and unique() to figure out stuff about the data]

df_list <-
  list.files("./_tempdata/", ".*\\.parquet$", ignore.case = TRUE) %>%
  as.data.frame() %>%
  rename(df_list = ".") %>%
  mutate(df_list = sub(
    pattern = "\\.parquet",
    replacement = "",
    x = df_list
  )) %>%
  unlist()

### Filter data--it's too much to hande ----
#'NOTE [make sure the working directory is pointing to the right location]
for (i in df_list) {
  assign(i, {
    read_parquet(file = paste0("./_tempdata/", i, ".parquet")) %>%
      filter(Geography %in% geo_filter)
  })
}

gc()

rm(i, df_list, geo_filter)

## Return list of current data tables ----
dfs_all <- names(which(unlist(eapply(
  .GlobalEnv, is.data.frame
))))

# ### Add date ----
# for (i in dfs_all) {
#   assign(i, {
#     get(i) %>%
#       mutate(Year = as.Date(paste0(Year, "-01-01")))
#     # adding "01-01" so R recognizes and treats "Year" as a date column
#   })
# }
# rm(i, dfs_all)

#' NOTE [only the following data sets have the characteristic variable]
dfs_characteristics <-
  c(
    "basicDT",
    "belongingDT",
    "civicDT",
    "civicDT2",
    "confidenceDT",
    "discriminationDT",
    "healthDT"
  )

### Define characteristics
for (i in dfs_characteristics) {
  assign(i, {
    get(i) %>%
      mutate(
        char_type = case_when(
          Characteristic %in% c(
            "Total, 15 years and over",
            "15 to 24 years",
            "25 to 64 years",
            "65 years and over",
            "Total, 18 years and over",
            "18 to 24 years",
            "Total, 12 years and over",
            "12 to 17 years",
            "18 to 64 years"
          ) ~ "Age",
          Characteristic %in% c("Total, by gender of person", "Men", "Women") ~ "Gender",
          Characteristic %in% c(
            "Immigrants",
            "Non-Immigrants",
            "Total, by immigration status",
            "Landed immigrants",
            "Immigrant, less than 10 years in Canada",
            "Immigrant, 10 or more years in Canada",
            "Born in Canada"
          ) ~ "Immigration Status",
          Characteristic %in% c(
            "First generation",
            "Second generation",
            "Third generation or more"
          ) ~ "Generation Status",
          Characteristic %in% c(
            "First official language spoken, English only",
            "First official language spoken, French only"
          ) ~ "Language Spoken",
          Characteristic %in% c(
            "Secondary (high) school diploma or equivalency certificate or less",
            "Postsecondary certificate or diploma (non-university)",
            "University certificate or diploma"
          ) ~ "Education Status",
          Characteristic %in% c("Total, by visible minority group",
            "Total visible minority population", "South Asian", "Chinese",
            "Black", "Filipino", "Arab", "Latin American", "Southeast Asian",
            "Not a visible minority")~ "Visible minority status"

        ) # adding a column to group relevant characteristics together
      )
  })
}

gc()

rm(i, dfs_characteristics)

## Discrimination data ----
discriminationDT <-
  discriminationDT %>%
  mutate(
    before_since = ifelse(
      Indicator == "Experience(s) of discrimination since the beginning of COVID-19 pandemic",
      "Since the beginning of covid-19 pandemic",
      str_to_sentence(trimws(
        sub(
          pattern = ".*,",
          replacement = "",
          x = Indicator
        )
      ))
    ),
    before_since = sub(
      pattern = "  ",
      replacement = " ",
      x = before_since
    ),
    ind = case_when(
      Indicator == "Experience(s) of discrimination since the beginning of COVID-19 pandemic" ~ "Experience(s) of discrimination",
      Indicator %in% c(
        "Discrimination at work or when applying for a job or promotion, since the beginning of COVID-19 pandemic",
        "Discrimination at work or when applying for a job or a promotion, 5 years before COVID-19 pandemic"
      ) ~ "Discrimination at work or when applying for a job or a promotion",
      Indicator %in% c(
        "Discrimination in a store, bank or restaurant, since the  beginning of COVID-19 pandemic",
        "Discrimination in a store, bank or restaurant, 5 years before COVID-19 pandemic"
      ) ~ "Discrimination in a store, bank or restaurant",
      TRUE ~ trimws(sub(
        pattern = ", .*",
        replacement = "",
        x = Indicator
      ))
    )
  )

## Police data ----
polData <-
  polData %>%
  mutate(motivation_type = ifelse(
    Motivation %in% c(
      "Total police-reported hate crime Race or ethnicity",
      "Race or ethnicity",
      "Religion",
      "Sexual orientation",
      "Language",
      "Disability",
      "Gender",
      "Age",
      "Unknown motivation"
    ),
    "Total police-reported hate crime",
    "Race or ethnicity"
  ))

## Indicators template ----
template <-
  read.csv("indicators_template.csv") %>%
  mutate_all(trimws)

# Repetitive code ----
## Sources ----
#'NOTE [these are the reoccuring sources I seen, I might be missing something]
source_ucrs <-
  "Source: Uniform Crime Reporting Survey (UCR), 2022"
source_cchs <-
  "Source: Canadian Community Health Survey (CCHS), September to December 2020"
source_gss <-
  "Source: Statistics Canada, General Social Survey – Social Identity, 2020."
source_census_nhs_census <-
  "Source: Census of Population, 2016, National Household Survey, 2011,  Census of Population, 2006."
source_census_nhs <-
  "Source: Censuses of population, 2006 and 2016; National Household Survey, 2011"
