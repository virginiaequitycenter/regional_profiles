## ...........................
## Script name: regional_data.R
##
## Authors: Michele Claibourn
## derived from albemarle county equity profile
##   data_sources.R
## Date Created: 2022-03-24
## Purpose: Generate data for regional equity profiles
##    sources for common county measures of AHDI only 
##   
## ...........................


## ...........................
# Setup ----

library(tidyverse)
library(readxl)
library(tidycensus)
# census_api_key("", install = TRUE, overwrite = TRUE)

# Region
data(fips_codes)
counties <-
  fips_codes %>%
  filter(state_code == "51") %>% 
  filter(county_code %in% c("003", "540", "065", "079", "109", "125"))


acs5 <- load_variables(2019, "acs5", cache = TRUE)


## ...........................
# Non ACS data ----
# Only needs to be downloaded once; contains data for all localities

## Initial download
## County Level Life Expectancy:  https://www.countyhealthrankings.org/app/virginia/2020/measure/outcomes/147/data
# url <- "https://www.countyhealthrankings.org/sites/default/files/media/document/2020%20County%20Health%20Rankings%20Virginia%20Data%20-%20v1_0.xlsx"
# destfile <- "data/health_rankings.xlsx"
# download.file(url, destfile)

## US Level Life Expectancy: https://www.countyhealthrankings.org/sites/default/files/media/document/analytic_data2020_0.csv
# url <- "https://www.countyhealthrankings.org/sites/default/files/media/document/analytic_data2020_0.csv"
# destfile <- "data/health_rankings_analytic.csv"
# download.file(url, destfile)

## Tract Level Life Expectancy: https://www.cdc.gov/nchs/nvss/usaleep/usaleep.html <- use this for life expectancy for the tract disaggregated AHDI
# url <- "https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NVSS/USALEEP/XLSX/VA_A.XLSX"
# destfile <- "data/tract_expectancy.xlsx"
# download.file(url, destfile)

## ALICE cost of living and estimates
# https://www.unitedforalice.org/virginia
# url <- "https://www.unitedforalice.org/Attachments/StateDataSheet/DataSheet_VA.xlsx"
# destfile <- "data/alice_va.xlsx"
# download.file(url, destfile)


## ...........................
# AHDI Table (region) ----
# Only needs to be completed once; contains data for all localities

## Calculation of AHDI: http://measureofamerica.org/Measure_of_America2013-2014MethodNote.pdf
## Use 2019 1-year estimates data 
## income goal posts in 2019 dollars determined with https://data.bls.gov/cgi-bin/cpicalc.pl

# life expectancies
us_life_expectancy <- read_csv("data/health_rankings_analytic.csv") 
county_life_expectancy <- read_excel("data/health_rankings.xlsx", sheet = 5, skip = 1) 

# US life expectancy overall
us_life_expectancy <- us_life_expectancy %>% 
  select(fips = `5-digit FIPS Code`, state = Name, contains("Life expectancy")) %>% 
  rename_with(~tolower(str_replace_all(.x, " ", "_")))  %>%
  select(-contains(c("_ci_", "numerator", "denominator"))) %>% 
  rename("life_expectancy_(asian)" = "life_expectancy_(asian/pacific_islander)") %>% 
  filter(state == "United States") %>% 
  mutate(county = NA_character_, 
         fips = "00000") %>% 
  select(fips, state, county, life_expectancy = life_expectancy_raw_value, 
         everything()) %>% 
  mutate(across(contains("life"), as.numeric))

# Life expectancies, all regional localities 
county_life_expectancy <-
  county_life_expectancy %>% 
  select(FIPS, State, County, contains("Life Expectancy")) %>% 
  rename_with(~tolower(str_replace_all(.x, "\\.", ""))) %>%
  rename_with(~tolower(str_replace_all(.x, " ", "_")))  %>%
  select(everything(), -contains("ci")) %>%
  bind_rows(us_life_expectancy) %>% 
  gather(label, number, -fips, -state, -county) %>%
  separate(label, c("label", "demographic"), sep = "\\(") %>%
  mutate(demographic = str_replace_all(demographic, "\\)", ""),
         demographic = case_when(
           is.na(demographic) ~ "total",
           TRUE ~ demographic 
         ),
         county = case_when(
           is.na(county) ~ "total",
           TRUE ~ county 
         )
  ) %>%
  select(-label) %>%
  filter(county %in% c("total", "Albemarle", 
                       "Charlottesville City", 
                       "Fluvanna", "Greene",
                       "Louisa", "Nelson"
  ))


## AHDI, ACS measures (county) ----
# Pull ACS 5 for all (for better comparability) 
table_labs <-
  tibble(
    variable = c("S2001_C01_002", # Median personal earnings of all workers with earnings ages 16 and older
                 "S1501_C02_014", # Percent high school graduate or higher
                 "S1501_C02_015", # Percent bachelor's degree or higher
                 "S1501_C02_013" # Percent graduate degree or higher
    ),
    label = c("pers_earn",  "hs_grad", "bac_deg", "grad_deg")
  )

table_dat <- 
  get_acs(
    year = 2019,
    geography = "county",
    state = "VA",
    county = counties$county_code,
    variables =  table_labs$variable,
    survey = "acs5"
  )  %>%
  select(GEOID, NAME, variable, estimate) %>%
  left_join(table_labs) %>%
  
  # State Data
  bind_rows(
    get_acs(
      year = 2019,
      geography = "state",
      state = "VA",
      variables =  table_labs$variable,
      survey = "acs5"
    )  %>%
      select(GEOID, NAME, variable, estimate) %>%
      left_join(table_labs) %>%
      mutate(GEOID = "51000")
  ) %>% 
  
  # US Data
  bind_rows(
    get_acs(
      year = 2019,
      geography = "us",
      # state = "VA",
      variables =  table_labs$variable,
      survey = "acs5"
    )  %>%
      select(GEOID, NAME, variable, estimate) %>%
      left_join(table_labs) %>%
      mutate(GEOID = "00000")    
  )


## Education Enrollment Data 
## tract_schl: 6 groups (3-4, 5-9, 10-14, 15-17, 18-19, 20-24) must be summed
##             population and enrolled, and divided
county_enroll <- get_acs(geography = "county", 
                         table = "S1401", 
                         state = "VA", 
                         county = counties$county_code, 
                         survey = "acs5", 
                         year = 2019, 
                         cache_table = TRUE)

state_enroll <- get_acs(geography = "state", 
                        table = "S1401", 
                        state = "VA", 
                        survey = "acs5", 
                        year = 2019, 
                        cache_table = TRUE)

us_enroll <- get_acs(geography = "us", 
                     table = "S1401", 
                     survey = "acs5", 
                     year = 2019, 
                     cache_table = TRUE)


## Derive County Level School Enrollment 
county_schl_num <- county_enroll %>% 
  filter(variable %in% c("S1401_C01_014", "S1401_C01_016", "S1401_C01_018", "S1401_C01_020", "S1401_C01_022", "S1401_C01_024")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(schl_num = sum(estimate), 
            schl_numM = moe_sum(moe = moe, estimate = estimate))

county_schl_den <- county_enroll %>% 
  filter(variable %in% c("S1401_C01_013", "S1401_C01_015", "S1401_C01_017", "S1401_C01_019", "S1401_C01_021", "S1401_C01_023")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(schl_den = sum(estimate), 
            schl_denM = moe_sum(moe = moe, estimate = estimate))

county_schl_ratio <- left_join(county_schl_num, county_schl_den)

county_schl <- county_schl_ratio %>% 
  summarize(schlE = round((schl_num/schl_den)*100, 1),
            schlM = moe_prop(schl_num, schl_den, schl_numM, schl_denM),
            schlM = round(schlM*100,1)) %>%
  select(fips = GEOID, school_enroll = schlE)

## State Level School Enrollment Data 
state_schl_num <- state_enroll %>% 
  filter(variable %in% c("S1401_C01_014", "S1401_C01_016", "S1401_C01_018", "S1401_C01_020", "S1401_C01_022", "S1401_C01_024")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(schl_num = sum(estimate), 
            schl_numM = moe_sum(moe = moe, estimate = estimate))

state_schl_den <- state_enroll %>% 
  filter(variable %in% c("S1401_C01_013", "S1401_C01_015", "S1401_C01_017", "S1401_C01_019", "S1401_C01_021", "S1401_C01_023")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(schl_den = sum(estimate), 
            schl_denM = moe_sum(moe = moe, estimate = estimate))

state_schl_ratio <- left_join(state_schl_num, state_schl_den)

state_schl <- state_schl_ratio %>% 
  summarize(schlE = round((schl_num/schl_den)*100, 1),
            schlM = moe_prop(schl_num, schl_den, schl_numM, schl_denM),
            schlM = round(schlM*100,1)) %>%
  select(fips = GEOID, school_enroll = schlE) %>%
  mutate(fips = "51000")

## US Level School Enrollment Data 
us_schl_num <- us_enroll %>% 
  filter(variable %in% c("S1401_C01_014", "S1401_C01_016", "S1401_C01_018", "S1401_C01_020", "S1401_C01_022", "S1401_C01_024")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(schl_num = sum(estimate), 
            schl_numM = moe_sum(moe = moe, estimate = estimate))

us_schl_den <- us_enroll %>% 
  filter(variable %in% c("S1401_C01_013", "S1401_C01_015", "S1401_C01_017", "S1401_C01_019", "S1401_C01_021", "S1401_C01_023")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(schl_den = sum(estimate), 
            schl_denM = moe_sum(moe = moe, estimate = estimate))

us_schl_ratio <- left_join(us_schl_num, us_schl_den)

us_schl <- us_schl_ratio %>% 
  summarize(schlE = round((schl_num/schl_den)*100, 1),
            schlM = moe_prop(schl_num, schl_den, schl_numM, schl_denM),
            schlM = round(schlM*100,1)) %>%
  select(fips = GEOID, school_enroll = schlE) %>%
  mutate(fips = "00000")


## Enrollment totals
enrollment <-
  county_schl %>%
  bind_rows(state_schl) %>% 
  bind_rows(us_schl)

## Finalize Table 1 Data
table_final <-
  table_dat %>%
  select(-variable) %>%
  spread(label, estimate) %>%
  rename(fips = GEOID) %>%
  select(-NAME) %>%
  left_join(county_life_expectancy %>%
              filter(demographic == "total") %>%
              rename(life_exp = number) %>% select(-demographic)) %>%
  left_join(enrollment) %>%
  mutate(
    ahdi_health = (life_exp - 66)/ (90-66)*10,
    ahdi_ed_attainment = ((hs_grad/100 + bac_deg/100 + grad_deg/100) - .5)/(2 - .5)*10,
    ahdi_ed_enroll = (school_enroll - 60)/(95 - 60) *10,
    ahdi_ed = (2*ahdi_ed_attainment/3) + (ahdi_ed_enroll/3),
    ahdi_income  = (log10(pers_earn)  -  log10(17234.09))   /(log10(72914) - log10(17234.09)) * 10
  ) %>%
  mutate(
    ahdi = (ahdi_health + ahdi_ed + ahdi_income)/3   ) %>%
  select(fips, county, everything(), -state) %>% 
  mutate(county = if_else(fips == "00000", "United States", county))

table_final

write_csv(table_final, "data/region/ahdi_table.csv")


## ...........................
## Life expectancy by race (county) ----
# all regional localities as available

write_csv(county_life_expectancy, "data/region/race_exp.csv")


## ...........................
# Gini Index ----
## to match Alice at county level ACS5
gini_index <-
  map_df(seq(2010,2019,1),
         ~get_acs(geography = "county", 
                  table = "B19083", 
                  state = "VA", 
                  county = counties$county_code, 
                  survey = "acs5", 
                  year = .x, 
                  cache_table = TRUE) %>%
           mutate(year = .x)
  )

gini_index_state <-
  map_df(seq(2010,2019,1),
         ~get_acs(geography = "state", 
                  table = "B19083", 
                  state = "VA", 
                  survey = "acs5", 
                  year = .x, 
                  cache_table = TRUE) %>%
           mutate(year = .x)
  )

gini_index_us <-
  map_df(seq(2010,2019,1),
         ~get_acs(geography = "us", 
                  table = "B19083", 
                  survey = "acs5", 
                  year = .x, 
                  cache_table = TRUE) %>%
           mutate(year = .x)
  )

gini_index_all <- bind_rows(gini_index, gini_index_state, gini_index_us)

write_csv(gini_index, path = "data/region/gini_index.csv")
write_csv(gini_index_all, path = "data/region/gini_index_state_us.csv")


## ...........................
# Cost Burdened ----
## B25074 or B25070; B19051 is total housing

county_housing_total <- get_acs(geography = "county", 
                                variable = "B19051_001", 
                                state = "VA", 
                                county = counties$county_code, 
                                survey = "acs5", 
                                year = 2019, 
                                cache_table = TRUE) %>%
  select(geoid = GEOID, total_households = estimate) 

county_housing_cost <- get_acs(geography = "county", 
                               table = "B25070", 
                               state = "VA", 
                               county = counties$county_code, 
                               survey = "acs5", 
                               year = 2019, 
                               cache_table = TRUE)

county_housing <-
  county_housing_cost %>%
  left_join(acs5 %>% rename(variable = name)) %>%
  separate(label, c(NA, "Total", "level"), sep = "!!")  %>%
  mutate(level = case_when(is.na(level) ~ "Total",
                           TRUE ~ level)) %>%
  select(estimate, level, GEOID)  %>%
  spread(level, estimate) %>%
  rename_with( ~ str_replace_all(
    tolower(
      str_replace_all(.x, " ", "_")
    ),
    "_percent",
    "")
  ) %>%
  mutate(
    denom = total - not_computed,
    Burdened = `30.0_to_34.9` + `35.0_to_39.9` + `40.0_to_49.9`,
    `Severely Burdened` =  `50.0_or_more`,
    `Not Burdened` = less_than_10.0 + `10.0_to_14.9` + `15.0_to_19.9` + `20.0_to_24.9` +`25.0_to_29.9`
  ) %>%
  select(geoid, denom, Burdened, `Severely Burdened`, `Not Burdened`, total_renting = total) %>%
  left_join(county_housing_total) %>%
  mutate(across(c(Burdened, `Severely Burdened`, `Not Burdened`), ~.x/denom), `Renting` = total_renting/total_households) 

write_csv(county_housing, "data/region/housing_costs.csv",
          num_threads = 1)

