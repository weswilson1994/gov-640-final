
library(readxl)
library(openxlsx)
library(httr)
library(tidyverse)
library(keyring)
library(tidycensus)

# Access the data from the G.I. Bill Comparison tool. 
data_url <- "https://www.benefits.va.gov/GIBILL/docs/job_aids/ComparisonToolData.xlsx"

GET(data_url, write_disk(path = tf <- tempfile(fileext = ".xls")))

data <- read_xlsx(path = tf, sheet = "Comparison_Tool_Data_Full", guess_max = 1000000) %>% 
  filter(type == c("PRIVATE", "PUBLIC", "FOR PROFIT") & approved == TRUE)

dictionary <- read.xlsx(data_url, sheet = "Data Dictionary")


# Now I am going to pull data from the census. 
key_get("census_api_key_wes")

#List and save the available variables. 
variables <- load_variables(2021, "acs5", cache = FALSE)
  

# Get the number of veterans by zip code from the 2017-2021 ACS survey, which surveys geographies with a population of 20,000 or more. 
vets_by_zip <- get_acs(geography = "zcta", variables = "B21001_002", year = 2021, survey = "acs5") 


  




