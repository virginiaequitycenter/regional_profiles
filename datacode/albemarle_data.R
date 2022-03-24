## ...........................
## Script name: albemarle_data.R
##
## Authors: Michele Claibourn
## derived from albemarle county equity profile
##   data_sources.R
## Date Created: 2022-03-24
## Purpose: Generate data for regional equity profiles
##    county-specific demographic data only
##   
## ...........................


## ...........................
# Setup ----

library(tidyverse)
library(readxl)
library(tidycensus)
# census_api_key("", install = TRUE, overwrite = TRUE)

# Region
countyfips = "003" 
longcountyfips = "51003"


## ...........................
# ACS/Census data (county) ----

## Find varnames
# acs1 <- load_variables(2019, "acs1", cache = TRUE)
acs5 <- load_variables(2019, "acs5", cache = TRUE)
# acs1_2019 <- load_variables(2019, "acs1/subject", cache = TRUE)
# acs1_2019_prof <- load_variables(2019, "acs1/profile", cache = TRUE)


## ...........................
## Data Profile, Demographic Breakdown ----
## ACS 2010-2019, 1-year estimates

# demographics over time
demographic_tables <-
  map_df(2019:2010,
         ~ get_acs(
           year = .x,
           geography = "county",
           state = "VA",
           county = countyfips,
           # variables =  race_vars$variable,
           table = "DP05",
           survey = "acs1", 
           cache = TRUE
         ) %>%
           mutate(year = .x)
  )

# demographic table labels
acs1_dp_labels <- 
  map_df(2019:2010,
         ~ load_variables(.x, 
                          "acs1/profile", 
                          cache = TRUE) %>%
           mutate(year = .x)
  ) %>%
  rename(variable = name)

# combine
demographic_profile_2010_2019 <-
  demographic_tables %>%
  left_join(acs1_dp_labels) %>%
  separate(label,
           c("stat", "category", "group", "level", "restriction", "etc"),
           sep = "!!") %>%  
  filter(stat %in% c("Percent", "Percent Estimate"))  %>%
  filter(!is.na(estimate)) %>%
  mutate(
    final_level =
      case_when(
        category == "SEX AND AGE" &
          group == "Total population" & !is.na(level) ~ level,
        category == "SEX AND AGE"  ~ group,
        category == "RACE" & group == "Total population" & level == "One race" & !is.na(restriction) & is.na(etc) ~ restriction,
        category == "RACE" & group == "One race" & !is.na(level) & is.na(restriction) ~ level,
        category == "RACE" & group == "Total population" & level == "Two or more races" & is.na(restriction)  ~ level,
        category == "RACE" & group == "Two or more races" & is.na(level)  ~ group,
        category == "HISPANIC OR LATINO AND RACE" & group == "Total population" & !is.na(level) & is.na(restriction) ~ level,
        category == "HISPANIC OR LATINO AND RACE" & !is.na(group)  & is.na(level) ~ group,
        
        TRUE ~ NA_character_
        
      )
  ) %>%
  filter(
    !final_level %in% c(
      "Under 18 years",
      "16 years and over",
      "18 years and over",
      "21 years and over",
      "62 years and over",
      "65 years and over"
    ),
    !is.na(final_level),
    !variable %in% c(
      "DP05_0023P",
      "DP05_0024P",
      "DP05_0026P",
      "DP05_0027P"
    )
  ) %>% 
  select(variable, estimate, moe, year, category, final_level ) 

demographic_table <-
  demographic_profile_2010_2019 %>%
  select(-moe, - variable) %>%
  distinct() %>%
  spread(year, estimate) #%>%

write_csv(demographic_table, "data/albemarle/demographic_table.csv")


## ...........................
## 2019 Age By Sex ----

sex_age <-
  get_acs(
    year = 2019,
    geography = "county",
    state = "VA",
    county = countyfips,
    # variables =  race_vars$variable,
    table = "B01001",
    survey = "acs5", 
    cache = TRUE
  )

sex_age <-
  sex_age %>%
  left_join(acs5 %>%
              rename(variable = name)) %>%
  mutate(label = str_replace_all(label, ":", "")) %>%
  separate(label,
           c(NA, "Total", "sex", "level"),
           sep = "!!")  %>%
  filter(!is.na(sex), !is.na(level)) %>%
  select(sex, level, estimate) %>%
  mutate(
    level = str_replace_all(level, " ", ""),
    level = str_replace_all(level, "years", ""),
    level = str_replace_all(level, "Under", "0to"),
    level = str_replace_all(level, "over", "500")
  ) %>% 
  separate(level, c("start", "end"), sep = "to|and") %>% 
  mutate(
    end = case_when(
      is.na(end) ~ start,
      TRUE ~ end
    ),
    start = as.numeric(start),
    end = as.numeric(end),
    age_group = 
      case_when(
        end < 6 ~ "Under 5 years",
        start > 4 & end < 10 ~ "5 to 9 years",
        start > 9 & end < 15 ~ "10 to 14 years",
        start > 14 & end < 20 ~ "15 to 19 years",
        start > 19 & end < 25 ~ "20 to 24 years",
        start > 24 & end < 35 ~ "25 to 34 years",
        start > 34 & end < 45 ~ "35 to 44 years",
        start > 44 & end < 55 ~ "45 to 54 years",
        start > 54 & end < 60 ~ "55 to 59 years",
        start > 59 & end < 65 ~ "60 to 64 years",
        start > 64 & end < 75 ~ "65 to 74 years",
        start > 74 & end < 85 ~ "75 to 84 years",
        start > 84 ~ "85 years and over"
      )
  ) %>% 
  group_by(sex, age_group) %>%
  summarize(estimate = sum(estimate))


write_csv(sex_age, "data/albemarle/sex_age.csv")


## ...........................
## Education ----

## No Grad, but All races 
race_disag_ed_5C <-
  map_df(c("C15002A","C15002B", "C15002C", "C15002D", "C15002E", "C15002F", "C15002G", "C15002H", "C15002I"),
         ~ get_acs(geography = "county",
                   table = .x,
                   state = "VA", 
                   county = countyfips, 
                   survey = "acs5",
                   year = 2019)  %>%
           left_join(acs5 %>% rename(variable = name))
  )

ed_table_race_total <-
  race_disag_ed_5C %>%
  mutate(label = str_replace_all(label, ":", "")) %>%
  separate(label, c("Estimate", "Total", "Sex", "Level"), sep = "!!") %>%
  mutate(
    Sex = case_when(is.na(Sex) ~ "Both",
                    TRUE ~ Sex),
    
    Level = case_when(is.na(Level)  ~ "All",
                      TRUE ~ Level)
  ) %>%
  separate(concept, c(NA, "Race"), sep = "\\(") %>%
  mutate(Race = str_replace_all(Race, "\\)", ""))  %>%
  select(-Estimate, -Total) %>%
  mutate(
    Total = case_when(
      Sex == "Both" & Level == "All" ~ estimate,
      TRUE ~ 0
    )  
  ) %>%
  group_by(Race) %>%
  mutate(Total = sum(Total))  %>% 
  group_by(Race, Level) %>%
  summarize(estimate = sum(estimate), Total = min(Total)) %>% 
  filter(!Level %in% c("All")) %>%
  mutate(pct = estimate/Total * 100) %>%
  select(-estimate, -Total) %>%
  spread(Level, pct) %>%
  mutate(Sex = "All")

ed_table_sex_race_disag <-
  race_disag_ed_5C %>%
  mutate(label = str_replace_all(label, ":", "")) %>%
  separate(label, c("Estimate", "Total", "Sex", "Level"), sep = "!!") %>%
  mutate(
    Sex = case_when(is.na(Sex) ~ "Both",
                    TRUE ~ Sex),
    
    Level = case_when(is.na(Level)  ~ "All",
                      TRUE ~ Level)
  ) %>%
  separate(concept, c(NA, "Race"), sep = "\\(") %>%
  mutate(Race = str_replace_all(Race, "\\)", ""))  %>%
  select(-Estimate, -Total) %>%
  mutate(
    Total = case_when(
      Level == "All" ~ estimate,
      TRUE ~ 0
    )  
  ) %>%
  group_by(Race, Sex) %>%
  mutate(Total = sum(Total))  %>% 
  group_by(Race, Sex, Level) %>%
  summarize(estimate = sum(estimate, na.rm = TRUE), Total = min(Total, na.rm = TRUE)) %>% 
  filter(!Level %in% c("All")) %>%
  mutate(pct = estimate/Total * 100) %>%
  select(-estimate, -Total) %>%
  spread(Level, pct)

ed_table_sex_total <- 
  race_disag_ed_5C %>%
  mutate(label = str_replace_all(label, ":", "")) %>%
  separate(label, c("Estimate", "Total", "Sex", "Level"), sep = "!!")  %>%
  mutate(
    Sex = case_when(is.na(Sex) ~ "Both",
                    TRUE ~ Sex),
    
    Level = case_when(is.na(Level)  ~ "All",
                      TRUE ~ Level)
  )  %>%
  select(-Estimate, -Total) %>%
  mutate(
    Total = case_when(
      Level == "All" ~ estimate,
      TRUE ~ 0
    )  
  ) %>%
  group_by(Sex) %>%
  mutate(Total = sum(Total)) %>%
  group_by(Sex, Level) %>%
  summarize(estimate = sum(estimate, na.rm = TRUE), Total = min(Total, na.rm = TRUE)) %>% 
  filter(!Level %in% c("All")) %>%
  mutate(pct = estimate/Total * 100) %>%
  select(-estimate, -Total) %>%
  spread(Level, pct) %>%
  mutate(Race = "All")

ed_table_total <-
  race_disag_ed_5C %>%
  mutate(label = str_replace_all(label, ":", "")) %>%
  separate(label, c("Estimate", "Total", "Sex", "Level"), sep = "!!")  %>%
  mutate(
    Sex = case_when(is.na(Sex) ~ "Both",
                    TRUE ~ Sex),
    
    Level = case_when(is.na(Level)  ~ "All",
                      TRUE ~ Level)
  )  %>%
  select(-Estimate, -Total) %>%
  mutate(
    Total = case_when(
      Sex == "Both" & Level == "All" ~ estimate,
      TRUE ~ 0
    )  
  ) %>%
  # group_by(Sex) %>%
  mutate(Total = sum(Total)) %>%
  group_by(Level) %>%
  summarize(estimate = sum(estimate, na.rm = TRUE), Total = min(Total, na.rm = TRUE))  %>% 
  filter(!Level %in% c("All")) %>%
  mutate(pct = estimate/Total * 100) %>%
  select(-estimate, -Total) %>%
  spread(Level, pct) %>%
  mutate(Race = "All", Sex = "All")

final_ed_table <-
  bind_rows(
    ed_table_race_total,
    ed_table_sex_race_disag,
    ed_table_sex_total,
    ed_table_total 
  )

final_ed_table %>%
  rename(`High school graduate` = `High school graduate (includes equivalency)`) %>%
  gather(degree, percent, -c(Race, Sex)) %>%
  write_csv(., path = "data/albemarle/education_distrbution.csv")


## ...........................
## Nativity -----

# Nativity in 2019: B05012
nativity_1B <-
  get_acs(geography = "county",
          table = "B05012",
          state = "VA", 
          county = countyfips, 
          survey = "acs5",
          year = 2019)  %>%
  left_join(acs5 %>% rename(variable = name)
  )

nativity <- nativity_1B %>%
  mutate(label = str_replace_all(label, ":", "")) %>%
  separate(label, c("Estimate", "Total", "Nativity"), sep = "!!") %>%
  mutate(denom = case_when(is.na(Nativity) ~ estimate,
                           TRUE ~ 0)) %>%
  group_by(NAME) %>%
  mutate(denom = sum(denom),
         pct = estimate / denom * 100) %>%
  ungroup() %>%
  select(Nativity, pct) %>%
  filter(!is.na(Nativity)) 

citizenship_1B <-
  get_acs(geography = "county",
          table = "B05001",
          state = "VA", 
          county = countyfips, 
          survey = "acs5",
          year = 2019)  %>%
  left_join(acs5 %>% rename(variable = name)
  )

citizenship <- citizenship_1B %>%
  mutate(label = str_replace_all(label, ":", "")) %>%
  separate(label, c("Estimate", "Total", "Citizenship"), sep = "!!") %>%
  mutate(denom = case_when(is.na(Citizenship) ~ estimate,
                           TRUE ~ 0)) %>%
  group_by(NAME) %>%
  mutate(denom = sum(denom),
         pct = estimate / denom * 100) %>%
  ungroup() %>%
  select(Citizenship, pct) %>%
  filter(!is.na(Citizenship)) 

## Origin of foreign born: B05006
origin_1B <-
  get_acs(geography = "county",
          table = "B05006",
          state = "VA", 
          county = countyfips, 
          survey = "acs5",
          year = 2019)  %>%
  left_join(acs5 %>% rename(variable = name)
  )

origin <- origin_1B %>%
  mutate(label = str_replace_all(label, ":", "")) %>%
  separate(label, c("Estimate", "Total", "Continent", "Country"), sep = "!!")  %>%
  filter(!is.na(Continent), is.na(Country)) %>%
  select(Continent, estimate)

save(origin, citizenship, nativity, file = "data/albemarle/origins.Rda")


## ...........................
# Tract level Data ----
## AHDI ----
## Life expectancy
tract_expectancy <- read_excel("data/tract_expectancy.xlsx")

tract_expectancy <- 
  tract_expectancy %>%
  rename_with(
    ~tolower(
      str_replace_all(.x, 
                      " ", "_")
    )    
  ) %>%
  rename(GEOID = tract_id, state_fips = state2kx, county_fips = cnty2kx, tract_fips = tract2kx, life_expectancy = `e(0)`, se = `se(e(0))` ) %>%
  select(-abridged_life_table_flag) %>%
  mutate(fips = paste0(state_fips, county_fips)) %>%
  filter(county_fips == countyfips)

## AHDI inputs from ACS: Personal Earnings, HS Grad, Bac Degree, Grad Degree
table_labs <-
  tibble(
    variable = c("S2001_C01_002", # Median personal earnings of all workers with earnings ages 16 and older
                 "S1501_C02_014", # Percent high school graduate or higher
                 "S1501_C02_015", # Percent bachelor's degree or higher
                 "S1501_C02_013" # Percent graduate degree or higher
    ),
    label = c("pers_earn",  "hs_grad", "bac_deg", "grad_deg")
  )

tract_facts <- get_acs(geography = "tract",
                       variables = table_labs$variable,
                       state = "VA", 
                       county = countyfips, 
                       survey = "acs5",
                       year = 2019) %>%
  select(GEOID, NAME, variable, estimate) %>%
  left_join(table_labs)


## AHDI inputs from ACS: School enrollment
tract_enroll <- get_acs(geography = "tract", 
                        table = "S1401", 
                        state = "VA", 
                        county = countyfips, 
                        survey = "acs5", 
                        year = 2019, 
                        cache_table = TRUE)

tract_schl_num <- tract_enroll %>% 
  filter(variable %in% c("S1401_C01_014", "S1401_C01_016", "S1401_C01_018", "S1401_C01_020", "S1401_C01_022", "S1401_C01_024")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(schl_num = sum(estimate), 
            schl_numM = moe_sum(moe = moe, estimate = estimate))

tract_schl_den <- tract_enroll %>% 
  filter(variable %in% c("S1401_C01_013", "S1401_C01_015", "S1401_C01_017", "S1401_C01_019", "S1401_C01_021", "S1401_C01_023")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(schl_den = sum(estimate), 
            schl_denM = moe_sum(moe = moe, estimate = estimate))

tract_schl_ratio <- left_join(tract_schl_num, tract_schl_den)

tract_schl <- tract_schl_ratio %>% 
  summarize(schlE = round((schl_num/schl_den)*100, 1),
            schlM = moe_prop(schl_num, schl_den, schl_numM, schl_denM),
            schlM = round(schlM*100,1)) %>%
  select(GEOID, school_enroll = schlE)

## Put it all together
tract_ahdi <-
  tract_facts %>%
  select(-variable) %>%
  spread(label, estimate) %>%
  select(-NAME) %>%
  left_join(tract_expectancy %>%
              select(GEOID, life_exp = life_expectancy)
  ) %>% 
  left_join(tract_schl) %>%
  mutate(
    ahdi_health = (life_exp - 66)/ (90-66)*10,
    ahdi_ed_attainment = ((hs_grad/100 + bac_deg/100 + grad_deg/100) - .5)/(2 - .5)*10,
    ahdi_ed_enroll = (school_enroll - 60)/(95 - 60) *10,
    ahdi_ed = (2*ahdi_ed_attainment/3) + (ahdi_ed_enroll/3),
    ahdi_income  = (log10(pers_earn)  -  log10(17234.09))   /(log10(72914) - log10(17234.09)) * 10
  ) %>%
  mutate(
    ahdi = (ahdi_health + ahdi_ed + ahdi_income)/3  ) 

write_csv(tract_ahdi, "data/albemarle/tract_ahdi.csv")

# not sure we need this, but just in case
write_csv(tract_schl, "data/albemarle/tract_enroll.csv")


## ...........................
## Education ----
## collapse this as finishing vs not finishing bachelors degrees 
disag_ed_5B_tract <-
  map_df(c("B15002"),
         ~ get_acs(geography = "tract",
                   table = .x,
                   state = "VA", 
                   county = countyfips, 
                   survey = "acs5",
                   year = 2019)  %>%
           left_join(acs5 %>% rename(variable = name))
  )

geo_ed <-disag_ed_5B_tract %>%
  mutate(label = str_replace_all(label, ":", "")) %>%
  separate(label, c("Estimate", "Total", "Sex", "Level"), sep = "!!")  %>%
  mutate(Sex = case_when(
    is.na(Sex) ~ "All",
    TRUE ~ Sex
  ),
  Level = case_when(
    is.na(Level) & Sex == "All" ~ "Total",
    TRUE ~ Level
  )
  ) %>%
  select(GEOID, estimate, Sex, Level) %>%
  group_by(GEOID, Level) %>%
  summarize(estimate = sum(estimate)) %>%
  filter(!is.na(Level)) %>%
  mutate(
    bac = case_when(
      Level %in% c("Bachelor's degree", "Doctorate degree", "Master's degree", "Professional school degree") ~ "bac",
      Level %in% c("Total") ~ "tot",
      TRUE ~ "less"
    )
  ) %>%
  group_by(GEOID, bac) %>% 
  summarize(est = sum(estimate)) %>%
  spread(bac, est) %>%
  mutate(perc_bac = bac/tot*100)

write_csv(geo_ed, "data/albemarle/tract_education.csv")


## ...........................
## Cost Burdened ----
## B25074 or B25070; B19051 is total housing
tract_housing_total <- get_acs(geography = "tract", 
                               variable = "B19051_001", 
                               state = "VA", 
                               county = countyfips, 
                               survey = "acs5", 
                               year = 2019, 
                               cache_table = TRUE) %>%
  select(total_households = estimate, geoid = GEOID) 


tract_housing_cost <- get_acs(geography = "tract", 
                              table = "B25070", 
                              state = "VA", 
                              county = countyfips, 
                              survey = "acs5", 
                              year = 2019, 
                              cache_table = TRUE)

## Tract level stats 
housing_costs <- 
  tract_housing_cost %>%
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
  
  mutate(denom = total - not_computed,
         Burdened = `30.0_to_34.9` + `35.0_to_39.9` + `40.0_to_49.9`,
         `Severely Burdened` =  `50.0_or_more`,
         `Not Burdened` = less_than_10.0 + `10.0_to_14.9` + `15.0_to_19.9` + `20.0_to_24.9` +`25.0_to_29.9`
         
  ) %>%
  select(geoid, denom, Burdened, `Severely Burdened`, `Not Burdened`, total_renting = total) %>%
  left_join(tract_housing_total) %>%
  mutate(across(c(Burdened, `Severely Burdened`, `Not Burdened`), ~.x/denom), Renting = total_renting/total_households) %>%
  mutate(county_type = "Census Tracts") 

write_csv(housing_costs, "data/albemarle/housing_costs.csv")


## ...........................
# ALICE data ----

## Stacked Area Chart of Poverty, Alice, + Alice
sheets <- excel_sheets("data/alice_va.xlsx")

alice_va <- read_excel("data/alice_va.xlsx", sheet = sheets[2]) %>%
  rename_with(~ tolower(str_replace_all(.x, ":|-| ", "_") )
  ) %>%
  filter(geo.id2 == longcountyfips)

alice_county <- alice_va %>%
  select(year, household, poverty_household, alice_household, above_alice_household) %>%
  gather(level, number, -year, -household) %>%
  mutate(pct = number/household*100 )

write_csv(alice_county, "data/albemarle/alice_hhs.csv")


## ...........................
## Alice & Median HHInc by Race ----
## S1901, B19013

## Household incomes by race: B190001A-I
table_list <- c(
  "B19013",
  "B19013A", 
  "B19013B", 
  "B19013C", 
  "B19013D", 
  "B19013E", 
  "B19013F", 
  "B19013G", 
  "B19013H",
  "B19013I"  
)

year_list <- seq(2010,2018, 2)
year_tables <- expand.grid(table_list, year_list)

med_hhinc <- 
  map2_df(year_tables$Var1, year_tables$Var2,
          ~ get_acs(geography = "county", 
                    table = .x, 
                    state = "VA", 
                    county = countyfips, 
                    survey = "acs5", 
                    year = .y, 
                    cache_table = TRUE)  %>%
            mutate(year = .y)
  )


## Alice Threshold vs. Median income 
alice_hhinc_thresh <- med_hhinc %>%
  left_join(acs5 %>% rename(variable = name)) %>%
  separate(concept, c(NA, NA, "race"), sep = "\\(") %>%
  mutate(
    race = case_when(
      is.na(race) ~ "Overall",
      TRUE ~ str_trim(str_replace_all(str_to_title(race), "\\)|Householder|Alone", ""))
    )
  ) %>%
  select(year, race, `Median Household Income` = estimate) %>%
  left_join(
    alice_va %>%
      select(year, `ALICE Threshold` = alice_threshold___hh_under_65) 
  ) 

write_csv(alice_hhinc_thresh, "data/albemarle/alice_thresh.csv")


## Tract Median Income
tract_med_inc   <- get_acs(geography = "tract", 
                           # table = "S1901", 
                           variable = "S1901_C01_012",
                           state = "VA", 
                           county = countyfips, 
                           survey = "acs5", 
                           year = 2019, 
                           cache_table = TRUE) 


write_csv(tract_med_inc, "data/albemarle/med_inc_tract.csv")

