
# What? ------------------------------------------------------------------------
# ESIF (European Structural and Investment Funds) 2014-2020
# 1. ESIF 2014-2020 Finance Implementation  - https://cohesiondata.ec.europa.eu/resource/99js-gm52.json

# Pckgs ------------------------------------------------------------------------
if (!require("pacman")) {
  install.packages("pacman")
}
library(pacman) # for loading packages
p_load(
  tidyverse, here,
  assertr,lubridate,
  # readxl, writexl, lubridate, janitor,
  # ggmap, ggrepel, lazyeval, magrittr,
  rjson, jsonify, httr, jsonlite, xml2,  rvest,
  RSocrata # API cohesiondata.ec
)


# Ingest from downloaded (csv)--------------------------------------------------
# library(readr)
# ESIF_2014_2020_csv <- read_csv("rawdata/ESIF_2014-2020_Finance_Implementation_Details.csv")


# Ingest from site (json) using RSocrata ---------------------------------------
# In my .Rprofile I have
# Sys.setenv("SG_API"="SG.5tX6d...._4cKPs")
Cohesion_API <- Sys.getenv("Cohesion_API")
Cohesion_token <- Sys.getenv("Cohesion_token")
# library("RSocrata")


### 1) ESIF_2014_20 Finance Implementation Details --------------------------------------------
# https://dev.socrata.com/foundry/cohesiondata.ec.europa.eu/99js-gm52
# This data set provides time series information on the financial implementation on the ground of the 530+ ESI Funded programmes. The data is cumulative, i.e. 2016 values included the finances implemented for 2015. Therefore the value for different years MUST NOT BE AGGREGATED

# d <- read.socrata("https://cohesiondata.ec.europa.eu/2014-2020-Finances/ESIF-2014-2020-Finance-Implementation-Details/99js-gm52")
# str(d)

ESIF_2014_20 <- read.socrata(
   # "https://cohesiondata.ec.europa.eu/resource/99js-gm52.json"#, JSON restituisce datatype chr
    "https://cohesiondata.ec.europa.eu/2014-2020-Finances/ESIF-2014-2020-Finance-Implementation-Details/99js-gm52"#,   just copy/paste the URL from address bar, RSocrata defaults to download from CSV
    # app_token = Cohesion_token,
    # email     = "lmmm76@georgetown.edu",
    # password  = "!cohesion2020"
)

str(ESIF_2014_20)
# 2) ESIF_2014_20_plan2imp ESIF 2014-2020 categorisation ERDF-ESF-CF planned vs implemented --------------------------------------------
# https://dev.socrata.com/foundry/cohesiondata.ec.europa.eu/3kkx-ekfq
# This file contains categorisation data from the ERDF/ESF/Cohesion Fund programmes and in particular it compares PLANNED AMOUNTS TO IMPLEMENTED INVESTMENTS.
# This dataset currently contains data to end-2016 and end- 2017. As the data is cumulative the **ANNUAL VALUES SHOULD NOT BE AGGREGATED *** but may be compared to see progress.

ESIF_2014_20_plan2imp <- read.socrata(
  # "https://cohesiondata.ec.europa.eu/resource/3kkx-ekfq.json"#, imprecise datatype
  "https://cohesiondata.ec.europa.eu/2014-2020-Categorisation/ESIF-2014-2020-categorisation-ERDF-ESF-CF-planned-/3kkx-ekfq"#, better datatype
  # app_token = Cohesion_token,
  # email     = "lmmm76@georgetown.edu",
  # password  = "!cohesion2020"
)

## 3) ESIF_2014_20_categ lookup categorization --------------------------------------------
# https://dev.socrata.com/foundry/cohesiondata.ec.europa.eu/xns4-t7ym
ESIF_2014_20_categ <- read.socrata("https://cohesiondata.ec.europa.eu/2014-2020-Categorisation/ESIF-2014-2020-Categorisation-Crosscutting-Themes-/xns4-t7ym")

# Change Datatypes ---------------------------------------------------------
# now redundant

    # # 1 ESIF_2014_20 (char -> number)
    # tibble::glimpse(ESIF_2014_20)
    #
    # #=== NUMERIC
    # Num <- c("eu_amount",
    #          "national_amount",
    #          "total_amount",
    #          #"year",
    #          "eu_co_financing",
    #          "total_eligible_cost",
    #          "total_eligible_expenditure")
    # ESIF_2014_20[,Num] <- as.numeric(unlist(ESIF_2014_20[,Num]))
    # ESIF_2014_20[ ,Num][is.na(ESIF_2014_20[ ,Num])]  <- 0
    #
    # ESIF_2014_20[,"year"] <-  as.integer(unlist(ESIF_2014_20[,"year"]))
    # tibble::glimpse(ESIF_2014_20)
    #
    #
    # #  2 ESIF_2014_20_categ (char -> number)
    # tibble::glimpse(ESIF_2014_20_categ)
    # Num <- c("intervention_field_code",
    #           "climate_weighting_",
    #          "biodiversity_weighting_" )
    # ESIF_2014_20_categ[,Num] <- as.numeric(unlist(ESIF_2014_20_categ[,Num]))
    # ESIF_2014_20_categ[ ,Num][is.na(ESIF_2014_20_categ[ ,Num])]  <- 0
    #
    # #  3 ESIF_2014_20_categ (char -> number)
    # tibble::glimpse(ESIF_2014_20_plan2imp)
    #
    # dput(names(ESIF_2014_20_plan2imp))
    # Num <-  c("eu_cofinancing_rate",
    #           "planned_total_amount_notional", "planned_eu_amount",
    #           "total_eligible_costs_selected_fin_data",
    #           "eu_eligible_costs_selected_fin_data_notional",
    #           "public_eligible_costs_fin_data",
    #           "total_elig_expenditure_declared_fin_data",
    #           "eu_elig_expenditure_declared_fin_data_notional",
    #           "planned_eu_amount_climate_change", # date
    #           "eu_eligible_costs_notional_climate_change",
    #           "eu_eligible_expenditure_notional_climate_change",
    #           "planned_eu_amount_biodiversity",
    #           "eu_eligible_costs_notional_biodiversity",
    #           "eu_eligible_expenditure_notional_biodiversity",
    #           "planned_eu_amount_clean_air",
    #           "eu_eligible_costs_notional_clean_air",
    #           "eu_eligible_expenditure_notional_clean_air",
    #           "number_of_operations" )
    #
    # ESIF_2014_20_plan2imp[,Num] <- as.numeric(unlist(ESIF_2014_20_plan2imp[,Num]))
    # ESIF_2014_20_plan2imp[ ,Num][is.na(ESIF_2014_20_plan2imp[ ,Num])]  <- 0
    #
    # ESIF_2014_20_plan2imp[,"year"] <-  as.integer(unlist(ESIF_2014_20_plan2imp[,"year"]))
    # tibble::glimpse(ESIF_2014_20_plan2imp)
    #

ESIF_2014_20$reference_date <- lubridate::as_date(ESIF_2014_20$reference_date)

# Select interesting stuff -----------------------------------------------------

### 1) "ESIF_2014_20" 99js-gm52 Finance Implementation Details -------------------
# Each row compares a planned allocation to a detailed category (by country, programme, fund, priority axis, category of region) to the reported value of selected projects (operations) and to the expenditure reported by those projects.-> different years MUST NOT BE AGGREGATED
glimpse(ESIF_2014_20)

# select only ERDF  - bc I'm interested in infrastructure (ESF is for social issue)
ERDF_2014_20  <- ESIF_2014_20 %>%
  filter(., fund == 'ERDF') # n 12678

# count unique
skimr::skim(ERDF_2014_20)
table(ERDF_2014_20$category_of_region, useNA = "ifany")

ERDF_2014_20$category_of_region <- factor(ERDF_2014_20$category_of_region,
                    levels = c("Less developed", "More developed","Transition", "Outermost or Northern Sparsely Populated", "VOID" ),
                    labels = c("LessDev", "MoreDev","Transition", "Remote", "InterRegio" ))


# assert + not_na
ERDF_2014_20 %>%  assert(not_na, cci  ) %>% summarise(N = n_distinct(cci))#   OK
# assert + is_uniq
ERDF_2014_20 %>%  assert(is_uniq, cci  ) %>% summarise(N = n_distinct(cci))#  not unique !

# assert + not_na
ERDF_2014_20 %>%  assert(not_na, reference_date  ) %>% summarise(N = n_distinct(reference_date))#   has na
table(ERDF_2014_20$reference_date, ERDF_2014_20$year, useNA = "ifany")

# assert + is_uniq
ERDF_2014_20 %>%  assert(is_uniq, reference_date  ) %>% summarise(N = n_distinct(reference_date))#  not unique!

table( ERDF_2014_20$reference_date)
table(ERDF_2014_20$cci)
table(ERDF_2014_20$cci, ERDF_2014_20$reference_date) # At any given point, I have multiple instances of 1 cci ? why?

ERDF_2014_20_y  <- ERDF_2014_20 %>%
  select(-eafrd_fa, -eafrd_measure, -measure_short_description) %>% # EAFRD related
  filter( reference_date != "2014-09-30") %>% # half years
  filter( reference_date != "2015-06-30") %>%
  filter( reference_date != "2020-06-30")

table( ERDF_2014_20_y$reference_date)

table( ERDF_2014_20_y$to)
table( ERDF_2014_20_y$to_short)

# Important values
ALLOCATED
total_amount
  eu_amount
  national_amount (co-financing)
v

total_eligible_cost    ??   (Total amount (EU+National) allocated to the projects (operations) selected by the programme managers)

v
total_eligible_expenditure


# NEXT  - I'm HERE  --------------------------------------------------------

# attaccare quel che serve
# 2) "ESIF_2014_20_plan2imp" 3kkx-ekfq ERDF-ESF-CF planned vs implemented -------
# categorization by fund and comparison PLANNED AMOUNTS TO IMPLEMENTED INVESTMENTS.
# This dataset currently contains data to end-2016 and end- 2017. As the data is cumulative the **ANNUAL VALUES SHOULD NOT BE AGGREGATED *** but may be compared to see progress.


## 3) ESIF_2014_20_categ lookup categorization --------------------------------------------


# # Loading files -------------------------------------------------------------------------------------
# from_dir <- here::here("rawdata/")
#
# from_dir %>%
#   list.files() %>%
#   .[str_detect(., "Data_FY")] -> files_xls # selecting my ones
# files_xls
#
# # Load everything into the Global Environment
# files_xls %>%
#   purrr::map(function(file_name) { # iterate through each file name
#     assign(
#       x = str_remove(file_name, ".xlsx"), # Remove file extension ".csv"
#       value = read_xlsx(paste0(from_dir, file_name)),
#       envir = .GlobalEnv
#     )
#   })


# Checking variables across FY ----------------------------------------------------------------
names(ESIF_2014_20)
names(ESIF_2014_20_categ)
names(Data_FY17)
names(ESIF_2014_20_plan2imp)

(ESIF_2014_20$fund)




# Intervals encompassing whole process in 1 Admin
TRUMP <- interval(ymd("2017-01-01 UTC"), ymd("2019-03-29 UTC"))
OBAMA_comp <- interval(ymd("2014-01-01 UTC"), ymd("2016-03-29 UTC"))

# dat$MONTH2 <- stringr::str_pad(dat$MONTH, 2, side = "left", pad = 0)

# add cols about processing times
dat <- FY_15_19 %>%
  dplyr::mutate(
    YEAR = lubridate::year(DECISION_DATE),
    MONTH = lubridate::month(DECISION_DATE),
    YEAR_MONTH = paste0(as.character(YEAR), "-", MONTH),
    TIME = ymd(DECISION_DATE) - ymd(CASE_RECEIVED_DATE),
    TIMEd = time_length(TIME, unit = "days"),
    TIMEm = time_length(TIME, unit = "months"),
    TIMEy = time_length(TIME, unit = "years")
  ) %>%
  # A col to tell me if WHOLE process WITHIN entire admin term
  dplyr::mutate(
    ENTIRE_TERM = case_when(
      .$CASE_RECEIVED_DATE %within% TRUMP & .$DECISION_DATE %within% TRUMP ~ "TRUMP",
      .$CASE_RECEIVED_DATE %within% OBAMA_comp & .$DECISION_DATE %within% OBAMA_comp ~ "OBAMA_cmp",
      TRUE ~ "MIXED"
    )
  ) %>%
  # Add cols for Country in travel ban
  dplyr::mutate(SPECIAL_COUNTRY = case_when(
    .$COUNTRY_OF_BIRTH %in% c("IRAN", "LIBYA", "NORTH KOREA", "SYRIA", "SOMALIA", "VENEZUELA", "YEMEN") ~ "TravelBan",
    # other revised
    .$COUNTRY_OF_BIRTH %in% c(
      "EGYPT", "JORDAN", "KUWAIT", "PALESTINIAN TERRITORIES", "QATAR", "BAHRAIN",
      "IRAQ", "MOROCCO", "OMAN", "UNITED ARAB EMIRATES", "TUNISIA", "ALGERIA", "ISRAEL",
      "SAUDI ARABIA", "LEBANON", "PALESTINE"
    ) ~ "MiddleEast & NorthAfr",

    .$COUNTRY_OF_BIRTH %in% c(
      "BURMA (MYANMAR)", "INDONESIA",
      "NEW ZEALAND", "PAPUA NEW GUINEA",
      "TAIWAN", "THAILAND",
      "MACAU", "WESTERN SAMOA", "BRUNEI", "CAMBODIA", "FIJI", "HONG KONG", "JAPAN",
      "KIRIBATI", "LAOS", "MALAYSIA", "MICRONESIA", "PALAU", "SAMOA", "SINGAPORE",
      "VIETNAM", "PHILIPPINES", "AUSTRALIA", "SOUTH KOREA", "MONGOLIA",
      "CHINA"
    ) ~ "EastAsia",

    .$COUNTRY_OF_BIRTH %in% c(
      "BHUTAN", "SRI LANKA", "MALDIVES", "NEPAL", "PAKISTAN",
      "AFGHANISTAN", "BANGLADESH", "INDIA"
    ) ~ "SouthAsia",

    .$COUNTRY_OF_BIRTH %in% c(
      "MOLDOVA", "GEORGIA", "PORTUGAL", "ROMANIA", "ARMENIA", "BELARUS", "KYRGYZSTAN",
      "NETHERLANDS", "RUSSIA", "SOVIET UNION", "SWEDEN", "SWITZERLAND", "ANDORRA",
      "AZERBAIJAN", "BELGIUM", "BOSNIA AND HERZEGOVINA", "BULGARIA",
      "CROATIA", "ESTONIA", "FINLAND", "FRANCE", "GIBRALTAR", "GREECE", "HUNGARY", "ICELAND",
      "IRELAND", "KOSOVO", "MONACO", "MONTENEGRO", "MALTA", "SLOVAKIA", "SLOVENIA",
      "SPAIN", "UZBEKISTAN", "TURKEY", "TURKMENISTAN",
      "UKRAINE", "SERBIA", "SERBIA AND MONTENEGRO", "LITHUANIA", "LUXEMBOURG", "MACEDONIA",
      "TAJIKISTAN", "YUGOSLAVIA", "ALBANIA", "AUSTRIA", "LATVIA", "ITALY",
      "UNITED KINGDOM", "GERMANY", "POLAND", "CYPRUS", "CZECH REPUBLIC", "CZECHOSLOVAKIA",
      "DENMARK", "KAZAKHSTAN", "NORWAY"
    ) ~ "Europe & CentrAsia",

    .$COUNTRY_OF_BIRTH %in% c(
      "MONTSERRAT", "NETHERLANDS ANTILLES", "SINT MAARTEN", "ST VINCENT", "SURINAME", "NICARAGUA", "PANAMA",
      "ANGUILLA", "BARBADOS", "BELIZE", "BRITISH VIRGIN ISLANDS",
      "CAYMAN ISLANDS", "CUBA", "CURACAO", "DOMINICA", "GRENADA", "GUYANA", "HAITI", "JAMAICA", "PERU",
      "SAINT VINCENT AND THE GRENADINES", "URUGUAY", "ANTIGUA AND BARBUDA", "ARGENTINA", "ARUBA",
      "COLOMBIA", "COSTA RICA", "ECUADOR", "ST KITTS AND NEVIS", "ST LUCIA",
      "BRAZIL", "CHILE", "MEXICO", "PARAGUAY", "BOLIVIA",
      "DOMINICAN REPUBLIC", "HONDURAS", "EL SALVADOR", "GUATEMALA", "BAHAMAS",
      "TRINIDAD AND TOBAGO"
    ) ~ "LatAm & Caribb",

    .$COUNTRY_OF_BIRTH %in% c(
      "BENIN", "IVORY COAST", "MAURITIUS",
      "LESOTHO", "LIBERIA", "MADAGASCAR", "MALAWI",
      "MAURITANIA", "NAMIBIA", "NIGER", "REPUBLIC OF CONGO",
      "RWANDA", "SAO TOME AND PRINCIPE", "SEYCHELLES", "SIERRA LEONE",
      "SOUTH SUDAN", "ANGOLA", "BURKINA FASO", "BURUNDI",
      "CAMEROON", "CAPE VERDE", "CHAD", "COTE d'IVOIRE",
      "DEMOCRATIC REPUBLIC OF CONGO", "DJIBOUTI",
      "EQUATORIAL GUINEA", "ERITREA", "GABON", "GAMBIA",
      "GHANA", "GUINEA", "GUINEA-BISSAU", "SUDAN", "SWAZILAND", "TOGO", "UGANDA",
      "TANZANIA", "ZAMBIA", "SENEGAL", "NIGERIA", "ETHIOPIA", "KENYA", "BOTSWANA",
      "MALI", "MOZAMBIQUE", "SOUTH AFRICA", "ZIMBABWE"
    ) ~ "Sub-Sahar Afr",

    .$COUNTRY_OF_BIRTH %in% c("UNITED STATES OF AMERICA", "BERMUDA", "CANADA") ~ "NorthAmerica"
  ))

# dat$MONTH2 <- stringr::str_pad(dat$MONTH, 2, side = "left", pad = 0) # add duratio
# dat$YEAR_MONTH = paste0(as.character(dat$YEAR), "-", dat$MONTH2)

table(dat$ENTIRE_TERM)
skimr::n_missing(dat$ENTIRE_TERM)
skimr::n_unique(dat$SPECIAL_COUNTRY)

# Which unit of pay
dat %>%
  dplyr::group_by(PW_UNIT_OF_PAY_9089) %>%
  dplyr::summarise(count = n(), percentage = 100 * count / (dim(dat)[1])) # ~ /nrow(dat)

# custom function
pw_unit_to_yearly <- function(prevailing_wage, pw_unit_of_pay) {
  return(ifelse(pw_unit_of_pay == "Year",
    prevailing_wage,
    ifelse(pw_unit_of_pay == "Hour",
      2080 * prevailing_wage,
      ifelse(pw_unit_of_pay == "Week",
        52 * prevailing_wage,
        ifelse(pw_unit_of_pay == "Month",
          12 * prevailing_wage,
          26 * prevailing_wage
        )
      )
    )
  ))
}

# change unit of pay
dat <- dat %>%
  dplyr::filter(!is.na(PW_UNIT_OF_PAY_9089)) %>%
  dplyr::mutate(PREVAILING_WAGE = as.numeric(PW_AMOUNT_9089)) %>%
  dplyr::mutate(PREVAILING_WAGE = pw_unit_to_yearly(PW_AMOUNT_9089, PW_UNIT_OF_PAY_9089)) %>%
  # select(YEAR:PW_UNIT_OF_PAY_9089, PREVAILING_WAGE, JOB_INFO_JOB_TITLE:FW_INFO_POSTAL_CODE ) %>%
  dplyr::select(-PW_AMOUNT_9089, -PW_UNIT_OF_PAY_9089)


# # check
# dat2[ 15000:150010, c("CASE_NUMBER","PW_AMOUNT_9089", "PW_UNIT_OF_PAY_9089", "PREVAILING_WAGE" )]

# Then, I merge WORKSITE_STATE_FULL with the WORKSITE_CITY to form a new feature WORKSITE.!!!!
site_merge <- function(x, y) {
  return(paste0(x, ", ", y))
}

dat$WORKSITE <- mapply(site_merge, dat$EMPLOYER_CITY, dat$EMPLOYER_STATE)


# Factor levels -------------------------------------------------------------------------------
table(dat$FOREIGN_WORKER_INFO_EDUCATION)
dat$FOREIGN_WORKER_INFO_EDUCATION <- factor(dat$FOREIGN_WORKER_INFO_EDUCATION,
  levels = c(
    "None", "High School",
    "Associate's", "Bachelor's",
    "Master's",
    "Doctorate",
    "Other"
  )
)

# Collapse educ {base}
dat$EDUC_LEVEL <- dat$FOREIGN_WORKER_INFO_EDUCATION
levels(dat$EDUC_LEVEL)
table(dat$EDUC_LEVEL, useNA = "ifany")
## [1] "None"        "High School" "Associate's" "Bachelor's"  "Master's"    "Doctorate"   "Other"
levels(dat$EDUC_LEVEL) <- c(levels(dat$EDUC_LEVEL), "None", "HighSchool_Other", "Graduate", "PostGraduate") # Collapse the current levels into the new levels
dat$EDUC_LEVEL[dat$EDUC_LEVEL %in% c("Associate's", "Bachelor's")] <- "Graduate"
dat$EDUC_LEVEL[dat$EDUC_LEVEL %in% c("Master's", "Doctorate")] <- "PostGraduate"
dat$EDUC_LEVEL[dat$EDUC_LEVEL %in% c("High School", "Other", NA)] <- "HighSchool_Other"
# Remove the now-unwanted old levels
summary(dat$EDUC_LEVEL)
dat$EDUC_LEVEL <- droplevels(dat$EDUC_LEVEL) # Drop the unwanted levels
dat$EDUC_LEVEL <- factor(dat$EDUC_LEVEL, levels = c(
  "None", "HighSchool_Other",
  "Graduate", "PostGraduate"
))
summary(dat$EDUC_LEVEL)

# NAICS_US_TITLE redux --------------------------------------------------------------------------
skimr::n_unique(dat$NAICS_US_TITLE)
skimr::n_complete(dat$NAICS_US_TITLE)

dat <- dat %>%
  dplyr::mutate(NAICS_sect = case_when(
    str_starts(NAICS_US_CODE, "11") ~ "Agric_Forest_Fish",
    str_starts(NAICS_US_CODE, "21") ~ "Mining_Oil_Gas",
    str_starts(NAICS_US_CODE, "22") ~ "Utilities",
    str_starts(NAICS_US_CODE, "23") ~ "Construction",
    str_starts(NAICS_US_CODE, "31") | str_starts(NAICS_US_CODE, "33") ~ "Manufacturing",
    str_starts(NAICS_US_CODE, "42") ~ "WholesaleTrade",
    str_starts(NAICS_US_CODE, "44") | str_starts(NAICS_US_CODE, "45") ~ "RetailTrade",
    str_starts(NAICS_US_CODE, "48") | str_starts(NAICS_US_CODE, "49") ~ "Transp_Logist",
    str_starts(NAICS_US_CODE, "51") ~ "Information",
    str_starts(NAICS_US_CODE, "52") ~ "Finance_Insurance",
    str_starts(NAICS_US_CODE, "53") ~ "Real Estate",
    str_starts(NAICS_US_CODE, "54") ~ "Prof_Scient_TechServ",
    str_starts(NAICS_US_CODE, "55") ~ "Management",
    str_starts(NAICS_US_CODE, "56") ~ "WasteMngmt_Remed",
    str_starts(NAICS_US_CODE, "61") ~ "Educational",
    str_starts(NAICS_US_CODE, "62") ~ "HealthCare_Social",
    str_starts(NAICS_US_CODE, "71") ~ "Arts_Entert",
    str_starts(NAICS_US_CODE, "72") ~ "Hotel_Food",
    str_starts(NAICS_US_CODE, "81") ~ "OtherServ",
    str_starts(NAICS_US_CODE, "92") ~ "PublicAdmin",
    TRUE ~ "NA"
  ))

skimr::n_unique(dat$NAICS_US_TITLE)
skimr::n_complete(dat$NAICS_US_TITLE)
skimr::n_unique(dat$NAICS_sect)
skimr::n_complete(dat$NAICS_sect)


# PW_LEVEL ------------------------------------------------------------------------------------
dat$PW_LEVEL_9089[dat$PW_LEVEL_9089 %in% c("N/A", "NULL", "<NA>", NA)] <- "na"

# Top 20... -----------------------------------------------------------------------------------
table(dat$EDUC_LEVEL, useNA = "ifany")
table(dat$NAICS_sect, useNA = "ifany")
table(dat$PW_LEVEL_9089, useNA = "ifany")

(country <- dat %>%
  group_by(COUNTRY_OF_BIRTH) %>%
  tally() %>% arrange(desc(n)) %>% .[1:20, ] %>% .$COUNTRY_OF_BIRTH)

(NAICS_sect <- dat %>%
  group_by(NAICS_sect) %>%
  tally() %>% arrange(desc(n)) %>% .[1:20, ] %>% .$NAICS_sect)

(JOB_TITLE <- dat %>%
  group_by(JOB_INFO_JOB_TITLE) %>%
  tally() %>% arrange(desc(n)) %>% .[1:50, ] %>% .$JOB_INFO_JOB_TITLE)

# All technical except for
#  [7] "Meat, Poultry, and Fish Cutters and Trimmers"     # 7/
# [21] "Cooks, Restaurant"
# [28] "Chefs and Head Cooks"
# [33] "Home Health Aides"
# [35] "Janitors and Cleaners, Except Maids and Housekeeping Cleaners"
# [43] "Heavy and Tractor-Trailer Truck Drivers"

(PW_SOC_TITLE <- dat %>%
  group_by(PW_SOC_TITLE) %>%
  tally() %>% arrange(desc(n)) %>% .[1:50, ] %>% .$PW_SOC_TITLE)

(EDUC_LEVEL <- dat %>%
  group_by(EDUC_LEVEL) %>%
  tally() %>% arrange(desc(n)) %>% .[1:8, ] %>% .$EDUC_LEVEL)

# with(dat,table(COUNTRY_OF_BIRTH, PW_LEVEL_9089)/sum(table(COUNTRY_OF_BIRTH, PW_LEVEL_9089)))
# freq <- with(dat,table(COUNTRY_OF_BIRTH, FOREIGN_WORKER_INFO_EDUCATION) )





skimr::n_missing(dat$TIMEm)
dat2 <- dat %>%
  filter(!is.na(TIMEm))

# save clean ds compressed --------------------------------------------------------------------
saveRDS(dat2,
  file = "dat2.rds",
  ascii = FALSE, version = NULL, compress = TRUE
)

# readRDS(file, refhook = NULL)
# readRDS (file = "FY_15_19_s.rds")
