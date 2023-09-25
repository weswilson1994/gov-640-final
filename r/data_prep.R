library(readxl)
library(openxlsx)
library(httr)
library(tidyverse)
library(keyring)
library(tidycensus)
options(scipen = 999)


# Access the data from the G.I. Bill Comparison tool. 
data_url <- "https://www.benefits.va.gov/GIBILL/docs/job_aids/ComparisonToolData.xlsx"

GET(data_url, write_disk(path = tf <- tempfile(fileext = ".xlsx")))

data <- read_excel(path = tf, sheet = "Comparison_Tool_Data_Full", guess_max = 1000000) %>% 
  filter(type %in% c("PRIVATE", "PUBLIC", "FOR PROFIT") & approved == TRUE)

dictionary <- read.xlsx(data_url, sheet = "Data Dictionary")

# Fix the names of the data so they are easier to work with: 
names(data) <- names(data) %>%
  gsub(pattern = " ", replacement = "_", x = names(data)) %>%
  str_to_lower()

# Now I am going to pull data from the census. 

#List and save the available variables. 
variables <- load_variables(2021, "acs5", cache = FALSE)
  
# Get the number of veterans by zip code from the 2017-2021 ACS survey, which surveys geographies with a population of 20,000 or more. 
vets_by_zip <- get_acs(geography = "zcta", variables = "B21001_002", year = 2021, survey = "acs5") 

# Prep the G.I. Bill data a bit
gi_bill <- data %>%
  # Select only the variables that are useful.
  select(facility_code, ope, ope6, institution, city, state, zip, type, bah,
    n_gibill = gibill, undergrad_enrollment, p911_recipients, p911_yr_recipients,
    yr_member = yr, poe_member = poe, eight_keys_member = eight_keys,
    dodmou, credit_for_mil_training, vet_poc, student_vet_grp_ipeds,
    graduation_rate_all_students, retention_all_students_ba, salary_all_students,
    annual_tuition = tuition_in_state, p911_tuition_fees, p911_yellow_ribbon
  ) %>%
  # I am going to limit my analysis to only private schools, whose tuition exceeds $25,000; this way I can isolate Yellow Ribbon scholarships.
  filter(type == "PRIVATE" & annual_tuition > 25000) %>% 
  #Make sure the data types are correct. 
  mutate(across(c(zip), ~ as.character(.))) %>% 
  # fix the zip code so I can join later. 
  mutate(zip = case_when(
    nchar(zip) == 4 ~ str_c("0", zip, sep = ""), 
    nchar(zip) == 3 ~ str_c("00", zip, sep = ""),
    TRUE ~ zip
  )) %>% 
  # Now replace NA values for select columns. 
  mutate(across(c(eight_keys_member, dodmou), ~ replace_na(., FALSE))) %>% 
  # Now replace NA values for gi bill beneficiaries with zero. 
  mutate(across(c(n_gibill, p911_yr_recipients, p911_recipients, p911_tuition_fees, p911_yellow_ribbon), ~ replace_na(., 0)))

# Prep the census data for join. 
census_vets <- vets_by_zip %>% 
  mutate(zip = as.character(GEOID)) %>% 
  select(zip, n_vets_census = estimate, moe)

# Try to join the G.I. Bill data with census data. There are about about ten observations that aren't
missing_matches <- left_join(x = gi_bill, y = census_vets) %>% 
  filter(is.na(n_vets_census) & !duplicated(zip)) %>% 
  select(institution, state, city, zip, n_vets_census) 

missing <- which(gi_bill$zip %in% missing_matches$zip)

new_zip_codes <- c("10011", "71103", "28617", "17602", "37204", "73135", "79698", "03755", "29426", "79699", "18711" , "53233" , "31404", 
                   "02881", "77074" , "33410" , "17603", "75604", "91505", "06524" , "80205", "27109", "90606" , "05403" , "11575" , "01611", 
                   "47725", "19610" , "33125" , "16546" , "18766" , "17606", "75270")

replacing_missing <- tibble(old_zip = missing_matches$zip, new_zip = new_zip_codes) %>% 
  left_join(y = census_vets, by = c("new_zip" = "zip"))

# Now join all the data together.  
gi_bill <- left_join(x = gi_bill, y = census_vets) %>% 
  left_join(y = replacing_missing, by = c("zip" = "old_zip")) %>% 
  mutate(n_vets_census = coalesce(n_vets_census.x, n_vets_census.y)) %>% 
  mutate(moe = coalesce(moe.x, moe.y)) %>% 
  select(-moe.x, - moe.y, - n_vets_census.x, - n_vets_census.y, - new_zip)

# add/modify a few variables 
gi_bill <- gi_bill %>% 
  mutate(avg_yellow_ribbon_payment = round(p911_yellow_ribbon / p911_recipients)) %>% 
  mutate(n_vets_census = ifelse(n_vets_census < n_gibill, n_vets_census + moe, n_vets_census)) %>% 
  select(-moe)

#export the original data
saveRDS(object = data, file = "./data/original/gi_bill_comparison_tool.rds")
saveRDS(object = census_vets, file = "./data/original/gi_bill_data_dictionary")
saveRDS(object = census_vets, file = "./data/original/vets_acs_survey_by_zip_code.rds")

#export the final data for analysis
saveRDS(object = gi_bill, file = "./data/final/final_gi_bill_data_for_analysis.rds")











