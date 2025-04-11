
# LOAD PACKAGES -----------------------------------------------------------
library(ipumsr)
library(tidycensus)
library(tigris)
library(tidyverse)
library(blsR)
library(httr)
library(janitor)
library(jsonlite)
library(mapview)
library(sf)
library(viridisLite)
library(viridis)
library(kableExtra)
library(readxl)
library(RColorBrewer)
library(broom)
library(biscale)
library(cowplot)
library(tidyverse)
library(ipumsr)
library(survey)  # For proper survey analysis
library(ggplot2)
library(sf)      # For mapping if needed

# patch tigris (if necessary)
# library(pak)
# pak::pak("walkerke/tigris@ftp-patch")
# options(tigris_use_cache = TRUE)

# check your working directory
# getwd()

# turn off scientific notation
options(scipen=999)
# OBJECTS -----------------------------------------------------

# county fips codes
county_fips <-
  tidycensus::fips_codes %>%
  filter(state == "MD") %>%
  mutate(county_code = str_c(state_code, county_code))

# if you need census variable definitions
# census_variables <-
#   load_variables(
#     "acs5",
#     year = 2023,
#     cache = T
#   )

# NAICS codes
naics_codes <-
  read_xlsx(paste0(here::here(), "/data/raw/indnaics_crosswalk_2023.xlsx")) %>%
  clean_names() %>%
  select(x2023_2027_acs_prcs_indnaics_code, industry_title) %>%
  rename(naics_code = x2023_2027_acs_prcs_indnaics_code) %>%
  filter(!is.na(naics_code))


# industry codes
naics_codes <-
  read_csv(paste0(here::here(), "/data/raw/industry-titles.csv"))

## IPUMS SETUP
### ddi codebook

#### 2023
ddi <- read_ipums_ddi(paste0(here::here(),"/data/raw/2023"))

micro_data <- read_ipums_micro(ddi)

#### 1940
ddi_1940 <- read_ipums_ddi(paste0(here::here(),"/data/raw/1940"))

micro_data_1940 <- read_ipums_micro(ddi_1940)




# HELPERS -----------------------------------------------------------------


# our helper function for grabbing stuff from the census so we don't have to type this all 100 times
md_govies <-
  function(geography = "tract") {
    get_acs(
      geography = geography,
      year = 2023,
      state = "md",
      variables = c("B24080_009", # fed workers (men)
                    "B24080_019", # fed workers (women)
                    "B24080_001", # total workers
                    "B19013_001", # median hh income
                    "B02001_001", # total pop (race)
                    "B02001_003", # Black or AA alone
                    "B02001_002" # white alone
      ),
      output = "wide",
      geometry = T) %>%
      rename(
        fed_men = B24080_009E,
        fed_women = B24080_019E,
        tot_workers = B24080_001E,
        median_hh_income = B19013_001E,
        total_pop = B02001_001E,
        black_aa = B02001_003E,
        white_pop = B02001_002E
      ) %>%
      mutate(fed_tot = fed_men + fed_women,
             fed_pct = fed_tot / tot_workers * 100,
             black_pct = black_aa / total_pop * 100,
             white_pct = white_pop / total_pop * 100
      ) %>%
      select(GEOID,
             NAME,
             fed_tot,
             tot_workers,
             fed_pct,
             black_pct,
             white_pct,
             median_hh_income)
  }

# Making some crosswalks
column_names <- colnames(micro_data) %>% tibble()

col_names_list <- list()

for(i in colnames(micro_data)) {

  obj_name <- str_to_lower(paste0(i, "_crosswalk"))

  # column_name <- print(paste0("micro_data$", i))

  crosswalk_df <- ipums_val_labels(micro_data[[i]])


  if(nrow(crosswalk_df) > 0){

    new_lbl <- str_to_lower(paste0(i, "_lbl"))

    col_names_list[i] <- i

    crosswalk_df <-
      crosswalk_df %>%
      rename(!!i := val,
             !!new_lbl := lbl)

    assign(obj_name, crosswalk_df, envir = .GlobalEnv)
  }

  else{
    print(paste("no crosswalk for", str_to_lower(i)))
  }

}


# Functions to parse IPUMS data

# The whole state
md_ipums <- function(race = 2, federal = "no"){

  if(race == "all"){

    md_analysis <-
      micro_data_clean %>%
      # Filter for working-age employed civilians
      filter(AGE >= 16,
             EMPSTAT == 1,
             ) %>%
      # Binary federal vs non-federal indicator
      mutate(is_federal = ifelse(CLASSWKRD == 25, "Federal worker", "Non-federal worker")) %>%
      filter(CLASSWKRD != 26)

      if(federal == "yes"){

      md_analysis <-
        md_analysis %>%
        filter(CLASSWKRD == 25)

    }


  }

  else if(race == "non-black") {

    md_analysis <-
      micro_data_clean %>%
      # Filter for working-age employed civilians
      filter(AGE >= 16,
             EMPSTAT == 1,
             RACE != 2
             ) %>%
      # Binary federal vs non-federal indicator
      mutate(is_federal = ifelse(CLASSWKRD == 25, "Federal worker", "Non-federal worker")) %>%
      filter(CLASSWKRD != 26)

      if(federal == "yes"){

      md_analysis <-
        md_analysis %>%
        filter(CLASSWKRD == 25)

    }


  }

  else {

    md_analysis <-
      micro_data_clean %>%
      # Filter for working-age employed civilians
      filter(AGE >= 16,
             EMPSTAT == 1,
             RACE == race
             ) %>%
      # Binary federal vs non-federal indicator
      mutate(is_federal = ifelse(CLASSWKRD == 25, "Federal worker", "Non-federal worker")) %>%
      filter(CLASSWKRD != 26)

      if(federal == "yes"){

      md_analysis <-
        md_analysis %>%
        filter(CLASSWKRD == 25)

    }

  }

  return(md_analysis)

}


# PG & Charles
pg_charles_ipums <- function(race = 2, federal = "no"){

  if(race == "all"){

    md_analysis <-
      micro_data_clean %>%
      # Filter for working-age employed civilians
      filter(AGE >= 16,
             EMPSTAT == 1,
             COUNTYFIP %in% c(33, 17)
             ) %>%
      # Binary federal vs non-federal indicator
      mutate(is_federal = ifelse(CLASSWKRD == 25, "Federal worker", "Non-federal worker")) %>%
      filter(CLASSWKRD != 26)%>%
      as.data.frame()

      if(federal == "yes"){

      md_analysis <-
        md_analysis %>%
        filter(CLASSWKRD == 25)%>%
        as.data.frame()

    }


  }

  else if(race == "non-black") {

    md_analysis <-
      micro_data_clean %>%
      # Filter for working-age employed civilians
      filter(AGE >= 16,
             EMPSTAT == 1,
             RACE != 2,
             COUNTYFIP %in% c(33, 17)
             ) %>%
      # Binary federal vs non-federal indicator
      mutate(is_federal = ifelse(CLASSWKRD == 25, "Federal worker", "Non-federal worker")) %>%
      filter(CLASSWKRD != 26) %>%
      as.data.frame()

      if(federal == "yes"){

      md_analysis <-
        md_analysis %>%
        filter(CLASSWKRD == 25) %>%
        as.data.frame()

    }


  }

  else {

    md_analysis <-
      micro_data_clean %>%
      # Filter for working-age employed civilians
      filter(AGE >= 16,
             EMPSTAT == 1,
             RACE == race,
             COUNTYFIP %in% c(33, 17)
             ) %>%
      # Binary federal vs non-federal indicator
      mutate(is_federal = ifelse(CLASSWKRD == 25, "Federal worker", "Non-federal worker")) %>%
      filter(CLASSWKRD != 26) %>%
      as.data.frame()

      if(federal == "yes"){

      md_analysis <-
        md_analysis %>%
        filter(CLASSWKRD == 25) %>%
        as.data.frame()

    }

  }

  return(md_analysis)

}

# Baltimore + Baltimore County
bmore_ipums <- function(race = 2, federal = "no"){

  if(race == "all"){

    md_analysis <-
      micro_data_clean %>%
      # Filter for working-age employed civilians
      filter(AGE >= 16,
             EMPSTAT == 1,
             COUNTYFIP %in% c(510, 5)
             ) %>%
      # Binary federal vs non-federal indicator
      mutate(is_federal = ifelse(CLASSWKRD == 25, "Federal worker", "Non-federal worker")) %>%
      filter(CLASSWKRD != 26) %>%
      as.data.frame()

    if(federal == "yes"){

      md_analysis <-
        md_analysis %>%
        filter(CLASSWKRD == 25) %>%
        as.data.frame()

    }


  }

  else if(race == "non-black") {

    md_analysis <-
      micro_data_clean %>%
      # Filter for working-age employed civilians
      filter(AGE >= 16,
             EMPSTAT == 1,
             RACE != 2,
             COUNTYFIP %in% c(510, 5)
             ) %>%
      # Binary federal vs non-federal indicator
      mutate(is_federal = ifelse(CLASSWKRD == 25, "Federal worker", "Non-federal worker")) %>%
      filter(CLASSWKRD != 26) %>%
      as.data.frame()

      if(federal == "yes"){

      md_analysis <-
        md_analysis %>%
        filter(CLASSWKRD == 25) %>%
        as.data.frame()

    }


  }

  else {

    md_analysis <-
      micro_data_clean %>%
      # Filter for working-age employed civilians
      filter(AGE >= 16,
             EMPSTAT == 1,
             RACE == race,
             COUNTYFIP %in% c(510, 5)
             ) %>%
      # Binary federal vs non-federal indicator
      mutate(is_federal = ifelse(CLASSWKRD == 25, "Federal worker", "Non-federal worker")) %>%
      filter(CLASSWKRD != 26) %>%
      as.data.frame()

      if(federal == "yes"){

      md_analysis <-
        md_analysis %>%
        filter(CLASSWKRD == 25) %>%
        as.data.frame()

    }

  }

  return(md_analysis)

}

svy_design_function <-
  function(data = md_analysis){

    svydesign(
      id = ~CLUSTER,
      strata = ~STRATA,
      weights = ~PERWT,
      data = data
    )
  }



# fix race (for 1940)
race_1940s <-
  race_crosswalk %>%
  mutate(
    race_lbl = case_when(
      RACE == 1 ~ "White",
      RACE == 2 ~ "Negro",
      RACE == 3 ~ "Indian (american)",
      RACE == 4 ~ "Japanese",
      RACE == 5 ~ "Chinese",
      RACE == 6 ~ "Filipino",
      RACE == 7 ~ "Hindu",
      RACE == 8 ~ "Korean",
      RACE == 9 ~ "Other",
    )
  )


# do my joins -- clean microdata and join it to crosswalks for each year
micro_data_clean <-
  micro_data %>%
  left_join(educ_crosswalk) %>%
  left_join(ownershp_crosswalk) %>%
  left_join(ownershpd_crosswalk) %>%
  left_join(race_crosswalk) %>%
  left_join(raced_crosswalk) %>%
  left_join(sex_crosswalk) %>%
  left_join(statefip_crosswalk) %>%
  left_join(classwkrd_crosswalk) %>%
  left_join(classwkr_crosswalk) %>%
  left_join(naics_codes,
            by = c("INDNAICS" = "industry_code")) %>%
  select(
    YEAR:GQ,
    everything()
  )


micro_data_clean_1940 <-
  micro_data_1940 %>%
  left_join(educ_crosswalk) %>%
  left_join(race_1940s) %>%
  # left_join(raced_crosswalk) %>%
  left_join(sex_crosswalk) %>%
  # left_join(countyicp_crosswalk) %>%
  select(
    YEAR:GQ,
    everything()
  ) %>%
  mutate(is_pg = ifelse(COUNTYICP == 0310, "y", NA_character_))

pg_micro_data_clean_1940 <-
  micro_data_clean_1940 %>%
  filter(is_pg == "y")


# Objects -----------------------------------------------------------------
## CENSUS
md_govies_tract <-
  md_govies("tract")

maj_black_tracts <-
  md_govies_tract %>%
  mutate(maj_black = ifelse(black_pct > 50, "y", "n")) %>%
  filter(maj_black == "y")

non_maj_black_tracts <-
  md_govies_tract %>%
  mutate(maj_black = ifelse(black_pct > 50, "y", "n")) %>%
  filter(maj_black == "n")

pg_charles_tracts <-
  md_govies_tract %>%
  filter(str_detect(NAME, "Prince George's|Charles")) %>%
  mutate(maj_black = ifelse(black_pct > 50, "y", "n")) %>%
  filter(maj_black == "n")

## IPUMS
# The whole state
md_analysis <-md_ipums(race = "all")
# only federal workers
md_federal_analysys <- md_ipums(race = "all", federal = "yes")
# all Black Marylanders
black_md_analysis <-md_ipums(race = 2)
# all Black federal employees
federal_black_md_analysis <-md_ipums(race = 2, federal = "yes")
# all non-Black Marylanders
non_black_md_analysis <-md_ipums(race = "non-black")
# all non-Black Maryland federal workers
federal_non_black_md_analysis <-md_ipums(race = "non-black", federal = "yes")
# Prince George's and Charles
pg_charles_analysis <-pg_charles_ipums(race = "all")
# all fed employees
federal_pg_charles_analysis <-pg_charles_ipums(race = "all", federal = "yes")
# Black
black_pg_charles_analysis <-pg_charles_ipums(race = 2)
# Black fed workers PG Charles
federal_black_pg_charles_analysis <-pg_charles_ipums(race = 2, federal = "yes")
# non-Black
non_black_pg_charles_analysis <-pg_charles_ipums(race = "non-black")
# non-Black
federal_non_black_pg_charles_analysis <-pg_charles_ipums(race = "non-black", federal = "yes")
# Baltimore
bmore_analysis <- bmore_ipums(race = "all")
# all fed employees
federal_bmore_analysis <-bmore_ipums(race = "all", federal = "yes")
# Black
black_bmore_analysis <-bmore_ipums(race = 2)
# Black fed workers PG Charles
federal_black_bmore_analysis <-bmore_ipums(race = 2, federal = "yes")
# non-Black
non_black_bmore_analysis <-bmore_ipums(race = "non-black")
# non-Black
federal_non_black_bmore_analysis <-bmore_ipums(race = "non-black", federal = "yes")

# Survey designs
# Whole State
## everyone
md_survey <- svy_design_function(md_analysis)
black_md_survey <- svy_design_function(black_md_analysis)
non_black_md_survey <- svy_design_function(non_black_md_analysis)
## just federal workers
fed_md_survey <- svy_design_function(md_federal_analysys)
fed_black_md_survey <- svy_design_function(federal_black_md_analysis)
fed_non_black_md_survey <-svy_design_function(federal_non_black_md_analysis)

# PG & Charles
## everyone
pg_charles_survey <- svy_design_function(pg_charles_analysis)
black_pg_charles_survey <- svy_design_function(black_pg_charles_analysis)
non_black_pg_charles_survey <- svy_design_function(non_black_pg_charles_analysis)
## just federal workers
fed_pg_charles_survey <- svy_design_function(federal_pg_charles_analysis)
fed_black_pg_charles_survey <- svy_design_function(federal_black_pg_charles_analysis)
fed_non_black_pg_charles_survey <-svy_design_function(federal_non_black_pg_charles_analysis)


# Baltimore City + Baltimore County
## everyone
bmore_survey <- svy_design_function(bmore_analysis)
black_bmore_survey <- svy_design_function(black_bmore_analysis)
non_black_bmore_survey <- svy_design_function(non_black_bmore_analysis)
## just federal workers
fed_bmore_survey <- svy_design_function(federal_bmore_analysis)
fed_black_bmore_survey <- svy_design_function(federal_black_bmore_analysis)
fed_non_black_bmore_survey <-svy_design_function(federal_non_black_bmore_analysis)
