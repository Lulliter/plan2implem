# Pckgs ---------------------------------------------------------------------------------------
if (!require("pacman")) {
  install.packages("pacman")
}
library(pacman) # for loading packages
p_load(
  tidyverse, readxl, writexl, here, lubridate, janitor,
  ggmap, ggrepel, lazyeval, magrittr
)


# Loading -------------------------------------------------------------------------------------
from_dir <- here::here("rawdata/")

from_dir %>%
  list.files() %>%
  .[str_detect(., "Data_FY")] -> files_xls # selecting my ones
files_xls

# Load everything into the Global Environment
files_xls %>%
  purrr::map(function(file_name) { # iterate through each file name
    assign(
      x = str_remove(file_name, ".xlsx"), # Remove file extension ".csv"
      value = read_xlsx(paste0(from_dir, file_name)),
      envir = .GlobalEnv
    )
  })


# Checking variables across FY ----------------------------------------------------------------
names(Data_FY15)
names(Data_FY16)
names(Data_FY17)
names(Data_FY18)
glimpse(Data_FY19)


FY19 <- Data_FY19 %>%
  dplyr::select(
    CASE_NUMBER, COUNTRY_OF_CITIZENSHIP,
    DECISION_DATE, CASE_RECEIVED_DATE, CASE_STATUS,
    FW_INFO_BIRTH_COUNTRY, CLASS_OF_ADMISSION, # immigration visa  held at the time permanent labor certification application was submitted
    FOREIGN_WORKER_INFO_EDUCATION, FOREIGN_WORKER_INFO_INST,
    # REFILE, ORIG_FILE_DATE, ORIG_CASE_NO ,
    EMPLOYER_NAME, EMPLOYER_CITY, EMPLOYER_STATE, EMPLOYER_COUNTRY, EMPLOYER_POSTAL_CODE, EMPLOYER_NUM_EMPLOYEES, EMPLOYER_YR_ESTAB,
    PW_SOC_CODE, PW_SOC_TITLE, PW_LEVEL_9089, PW_AMOUNT_9089, PW_UNIT_OF_PAY_9089,
    JOB_INFO_JOB_TITLE, EMPLOYER_DECL_INFO_TITLE, NAICS_US_CODE, NAICS_US_TITLE, # PW_JOB_TITLE_9089,
    FOREIGN_WORKER_INFO_CITY, FOREIGN_WORKER_INFO_STATE, FW_INFO_POSTAL_CODE # now in the US ??
  )



FY18 <- Data_FY18 %>%
  dplyr::select(
    CASE_NUMBER, COUNTRY_OF_CITIZENSHIP,
    DECISION_DATE, CASE_RECEIVED_DATE, CASE_STATUS,
    FW_INFO_BIRTH_COUNTRY, CLASS_OF_ADMISSION, # immigration visa  held at the time permanent labor certification application was submitted
    FOREIGN_WORKER_INFO_EDUCATION, FOREIGN_WORKER_INFO_INST,
    # REFILE, ORIG_FILE_DATE, ORIG_CASE_NO ,
    EMPLOYER_NAME, EMPLOYER_CITY, EMPLOYER_STATE, EMPLOYER_COUNTRY, EMPLOYER_POSTAL_CODE, EMPLOYER_NUM_EMPLOYEES, EMPLOYER_YR_ESTAB,
    PW_SOC_CODE, PW_SOC_TITLE, PW_LEVEL_9089, PW_AMOUNT_9089, PW_UNIT_OF_PAY_9089,
    JOB_INFO_JOB_TITLE, EMPLOYER_DECL_INFO_TITLE, NAICS_US_CODE, NAICS_US_TITLE, # PW_JOB_TITLE_9089,
    FOREIGN_WORKER_INFO_CITY, FOREIGN_WORKER_INFO_STATE, FW_INFO_POSTAL_CODE # now in the US ??
  )

FY17 <- Data_FY17 %>%
  dplyr::select(
    CASE_NUMBER, COUNTRY_OF_CITIZENSHIP,
    DECISION_DATE, CASE_RECEIVED_DATE, CASE_STATUS,
    FW_INFO_BIRTH_COUNTRY, CLASS_OF_ADMISSION, # immigration visa  held at the time permanent labor certification application was submitted
    FOREIGN_WORKER_INFO_EDUCATION, FOREIGN_WORKER_INFO_INST,
    # REFILE, ORIG_FILE_DATE, ORIG_CASE_NO ,
    EMPLOYER_NAME, EMPLOYER_CITY, EMPLOYER_STATE, EMPLOYER_COUNTRY, EMPLOYER_POSTAL_CODE, EMPLOYER_NUM_EMPLOYEES, EMPLOYER_YR_ESTAB,
    PW_SOC_CODE, PW_SOC_TITLE, PW_LEVEL_9089, PW_AMOUNT_9089, PW_UNIT_OF_PAY_9089,
    JOB_INFO_JOB_TITLE, EMPLOYER_DECL_INFO_TITLE, NAICS_US_CODE, NAICS_US_TITLE, # PW_JOB_TITLE_9089,
    FOREIGN_WORKER_INFO_CITY, FOREIGN_WORKER_INFO_STATE, FW_INFO_POSTAL_CODE # now in the US ??
  )

FY16 <- Data_FY16 %>%
  dplyr::select(
    CASE_NUMBER, COUNTRY_OF_CITIZENSHIP,
    DECISION_DATE, CASE_RECEIVED_DATE, CASE_STATUS,
    FW_INFO_BIRTH_COUNTRY, CLASS_OF_ADMISSION, # immigration visa  held at the time permanent labor certification application was submitted
    FOREIGN_WORKER_INFO_EDUCATION, FOREIGN_WORKER_INFO_INST,
    # REFILE, ORIG_FILE_DATE, ORIG_CASE_NO ,
    EMPLOYER_NAME, EMPLOYER_CITY, EMPLOYER_STATE, EMPLOYER_COUNTRY, EMPLOYER_POSTAL_CODE, EMPLOYER_NUM_EMPLOYEES, EMPLOYER_YR_ESTAB,
    PW_SOC_CODE, PW_SOC_TITLE, PW_LEVEL_9089, PW_AMOUNT_9089, PW_UNIT_OF_PAY_9089,
    JOB_INFO_JOB_TITLE, EMPLOYER_DECL_INFO_TITLE, NAICS_US_CODE, NAICS_US_TITLE, # PW_JOB_TITLE_9089 = PW_Job_Title_9089, # change name
    FOREIGN_WORKER_INFO_CITY, FOREIGN_WORKER_INFO_STATE, FW_INFO_POSTAL_CODE
  ) # now in the US ??


FY15 <- Data_FY15 %>%
  dplyr::select(
    CASE_NUMBER, COUNTRY_OF_CITIZENSHIP,
    DECISION_DATE, CASE_RECEIVED_DATE, CASE_STATUS,
    FW_INFO_BIRTH_COUNTRY, CLASS_OF_ADMISSION, # immigration visa  held at the time permanent labor certification application was submitted
    FOREIGN_WORKER_INFO_EDUCATION, FOREIGN_WORKER_INFO_INST,
    # REFILE, ORIG_FILE_DATE, ORIG_CASE_NO ,
    EMPLOYER_NAME, EMPLOYER_CITY, EMPLOYER_STATE, EMPLOYER_COUNTRY, EMPLOYER_POSTAL_CODE, EMPLOYER_NUM_EMPLOYEES, EMPLOYER_YR_ESTAB,
    PW_SOC_CODE, PW_SOC_TITLE, PW_LEVEL_9089, PW_AMOUNT_9089, PW_UNIT_OF_PAY_9089,
    JOB_INFO_JOB_TITLE, EMPLOYER_DECL_INFO_TITLE, NAICS_US_CODE, NAICS_US_TITLE, # PW_JOB_TITLE_9089 = PW_Job_Title_9089,
    FOREIGN_WORKER_INFO_CITY, FOREIGN_WORKER_INFO_STATE, FW_INFO_POSTAL_CODE
  ) # now in the US ??
FY17$PW_AMOUNT_9089 <- as.numeric(FY17$PW_AMOUNT_9089)
FY18$PW_AMOUNT_9089 <- as.numeric(FY18$PW_AMOUNT_9089)


# Append all 5 FY -----------------------------------------------------------------------------
FY_15_19 <- bind_rows(FY15, FY16, FY17, FY18, FY19)
str(FY_15_19) # 474,359 obs. of  28 variables:

skimr::n_unique(FY_15_19$CASE_NUMBER)
skimr::n_missing(FY_15_19$CASE_NUMBER)


# Select Representative countries -------------------------------------------------------------
table(FY_15_19$COUNTRY_OF_CITIZENSHIP)

countries <- c(
  "VENEZUELA", "SYRIA", "NORTH KOREA", "YEMEN", "LIBYA", "IRAN", "SOMALIA", # ban
  "ISRAEL", "SAUDI ARABIA", "LEBANON", "PALESTINE",
  "AUSTRIA", "LATVIA", "ITALY", "UNITED KINGDOM", "GERMANY", "POLAND", "DENMARK", "KAZAKHSTAN", "NORWAY",
  "AUSTRALIA", "SOUTH KOREA", "VIETNAM", "BANGLADESH", "CHINA", "PHILIPPINES", "INDIA",
  "BRAZIL", "CHILE", "MEXICO", "PARAGUAY", "BOLIVIA", "HONDURAS", "EL SALVADOR", "GUATEMALA",
  "DOMINICAN REPUBLIC", "BAHAMAS", "TRINIDAD AND TOBAGO",
  "SENEGAL", "NIGERIA", "ETHIOPIA", "KENYA", "BOTSWANA", "MALI", "MOZAMBIQUE", "SOUTH AFRICA", "ZIMBABWE",
  "BERMUDA", "CANADA"
)


FY_15_19 <- FY_15_19 %>%
  # dplyr::filter(FW_INFO_BIRTH_COUNTRY %in% countries) %>%
  dplyr::rename(COUNTRY_OF_BIRTH = FW_INFO_BIRTH_COUNTRY) %>%
  dplyr::mutate(REGION_BIRTH = case_when(
    COUNTRY_OF_BIRTH %in% c(
      "BURMA (MYANMAR)", "INDONESIA",
      "NEW ZEALAND", "PAPUA NEW GUINEA",
      "TAIWAN", "THAILAND",
      "MACAU", "WESTERN SAMOA", "BRUNEI", "CAMBODIA", "FIJI", "HONG KONG", "JAPAN",
      "KIRIBATI", "LAOS", "MALAYSIA", "MICRONESIA", "PALAU", "SAMOA", "SINGAPORE",
      "NORTH KOREA", "VIETNAM", "PHILIPPINES", "AUSTRALIA", "SOUTH KOREA", "MONGOLIA",
      "CHINA"
    ) ~ "EastAsia",

    COUNTRY_OF_BIRTH %in% c(
      "BHUTAN", "SRI LANKA", "MALDIVES", "NEPAL", "PAKISTAN",
      "AFGHANISTAN", "BANGLADESH", "INDIA"
    ) ~ "SouthAsia",

    COUNTRY_OF_BIRTH %in% c(
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

    COUNTRY_OF_BIRTH %in% c(
      "MONTSERRAT", "NETHERLANDS ANTILLES", "SINT MAARTEN", "ST VINCENT", "SURINAME", "NICARAGUA", "PANAMA",
      "ANGUILLA", "BARBADOS", "BELIZE", "BRITISH VIRGIN ISLANDS",
      "CAYMAN ISLANDS", "CUBA", "CURACAO", "DOMINICA", "GRENADA", "GUYANA", "HAITI", "JAMAICA", "PERU",
      "SAINT VINCENT AND THE GRENADINES", "URUGUAY", "ANTIGUA AND BARBUDA", "ARGENTINA", "ARUBA",
      "COLOMBIA", "COSTA RICA", "ECUADOR", "ST KITTS AND NEVIS", "ST LUCIA", "VENEZUELA",
      "BRAZIL", "CHILE", "MEXICO", "PARAGUAY", "BOLIVIA",
      "DOMINICAN REPUBLIC", "HONDURAS", "EL SALVADOR", "GUATEMALA", "BAHAMAS",
      "TRINIDAD AND TOBAGO"
    ) ~ "LatAm & Caribb",

    COUNTRY_OF_BIRTH %in% c(
      "EGYPT", "JORDAN", "KUWAIT", "PALESTINIAN TERRITORIES", "QATAR", "BAHRAIN",
      "IRAQ", "MOROCCO", "OMAN", "UNITED ARAB EMIRATES", "TUNISIA", "ALGERIA", "ISRAEL",
      "SYRIA", "YEMEN", "LIBYA", "IRAN", "SAUDI ARABIA",
      "LEBANON", "PALESTINE"
    ) ~ "MiddleEast & NorthAfr",

    COUNTRY_OF_BIRTH %in% c(
      "BENIN", "IVORY COAST", "MAURITIUS",
      "LESOTHO", "LIBERIA", "MADAGASCAR", "MALAWI",
      "MAURITANIA", "NAMIBIA", "NIGER", "REPUBLIC OF CONGO",
      "RWANDA", "SAO TOME AND PRINCIPE", "SEYCHELLES", "SIERRA LEONE",
      "SOUTH SUDAN", "ANGOLA", "BURKINA FASO", "BURUNDI",
      "CAMEROON", "CAPE VERDE", "CHAD", "COTE d'IVOIRE",
      "DEMOCRATIC REPUBLIC OF CONGO", "DJIBOUTI",
      "EQUATORIAL GUINEA", "ERITREA", "GABON", "GAMBIA",
      "GHANA", "GUINEA", "GUINEA-BISSAU", "SUDAN", "SWAZILAND", "TOGO", "UGANDA",
      "TANZANIA", "ZAMBIA", "SOMALIA", "SENEGAL", "NIGERIA", "ETHIOPIA", "KENYA", "BOTSWANA",
      "MALI", "MOZAMBIQUE", "SOUTH AFRICA", "ZIMBABWE"
    ) ~ "Sub-Sahar Afr",

    COUNTRY_OF_BIRTH %in% c("UNITED STATES OF AMERICA", "BERMUDA", "CANADA") ~ "NorthAmerica"
  ))


table(FY_15_19$COUNTRY_OF_BIRTH, useNA = "ifany")
table(FY_15_19$REGION_BIRTH, useNA = "ifany")

FY_15_19 <- FY_15_19 %>% filter(!is.na(COUNTRY_OF_BIRTH))

recap <- FY_15_19 %>%
  group_by(COUNTRY_OF_BIRTH) %>%
  count()


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
