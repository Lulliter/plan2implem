
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

## 0) ESIF_2014_20 Finance PLANNED  Details --------------------------------------------
#https://cohesiondata.ec.europa.eu/2014-2020/ESIF-2014-2020-FINANCES-PLANNED-DETAILS/e4v6-qrrq
#This dataset provides information on planned total and EU financing under the different ESI Funds (2014-2020) in current #prices. The data is taken from the adopted financial tables and is broken down by fund, programme, priority axis, thematic #objective and category of region (more developed, less developed, etc. where available). It is updated daily to reflect any #modifications (i.e; thematic reallocations) agreed between the Member States and the Commission.
#
#The data cover the more than 530 programmes and includes the EU and national co-financing covered by the adoption decision. #Financial allocations in the programme financial table may change over time (i.e. transfers between themes, between funds).

ESIF_2014_20_plan <- read.socrata(
    # "https://cohesiondata.ec.europa.eu/resource/99js-gm52.json"#, JSON restituisce datatype chr
    "https://cohesiondata.ec.europa.eu/2014-2020/ESIF-2014-2020-FINANCES-PLANNED-DETAILS/e4v6-qrrq"#,   just copy/paste the URL from address bar, RSocrata defaults to download from CSV
)

str(ESIF_2014_20_plan)

# ESIF_2014_20_plan$ms <- as.factor(ESIF_2014_20_plan$ms)
# ESIF_2014_20_plan$fund <- as.factor(ESIF_2014_20_plan$fund)
# ESIF_2014_20_plan$title <- as.factor(ESIF_2014_20_plan$title)
# ESIF_2014_20_plan$to <- as.factor(ESIF_2014_20_plan$to)
# ESIF_2014_20_plan$to_short <- as.factor(ESIF_2014_20_plan$to_short)
# ESIF_2014_20_plan$to_long <- as.factor(ESIF_2014_20_plan$to_long)
ESIF_2014_20_plan <- ESIF_2014_20_plan %>%
    modify_at( c("ms", "fund", "title", "to", "to_short", "to_long", "category_of_region" ), as.factor)

ESIF_2014_20_plan <- ESIF_2014_20_plan %>% arrange(ms, fund, title) # evita che i dati dei menu vengano caricati non in ordine

## 1) ESIF_2014_20 Finance Implementation Details --------------------------------------------

# https://dev.socrata.com/foundry/cohesiondata.ec.europa.eu/99js-gm52
# Dataset Identifier: 99js-gm52
# Total Rows: 67571
# Source Domain: cohesiondata.ec.europa.eu
# Created: 12/14/2016, 1:42:06 PM
# Last Updated: 10/27/2022, 7:55:48 AM
# Category: 2014 / 2020 Finances
# Attribution: European Commission
# License: Creative Commons 1.0 Universal (Public Domain Dedication)
#
# This data set provides time series information on the financial implementation on the ground of the 530+ ESI Funded programmes.
# The data is cumulative, i.e. 2016 values included the finances implemented for 2015. Therefore the value for different years MUST NOT BE AGGREGATED

# d <- read.socrata("https://cohesiondata.ec.europa.eu/2014-2020-Finances/ESIF-2014-2020-Finance-Implementation-Details/99js-gm52")
# str(d)

ESIF_2014_20_fin <- read.socrata(
   # "https://cohesiondata.ec.europa.eu/resource/99js-gm52.json"#, JSON restituisce datatype chr
    "https://cohesiondata.ec.europa.eu/2014-2020-Finances/ESIF-2014-2020-Finance-Implementation-Details/99js-gm52"#,   just copy/paste the URL from address bar, RSocrata defaults to download from CSV

)

str(ESIF_2014_20_fin)

# ESIF_2014_20_fin$ms <- as.factor(ESIF_2014_20_fin$ms)
# ESIF_2014_20_fin$fund <- as.factor(ESIF_2014_20_fin$fund)
# ESIF_2014_20_fin$title <- as.factor(ESIF_2014_20_fin$title)
ESIF_2014_20_fin <- ESIF_2014_20_fin %>%
    modify_at( c("ms", "fund", "title", "to", "to_short", "to_long", "category_of_region", "year" ), as.factor)

ESIF_2014_20_fin <- ESIF_2014_20_fin %>% arrange(ms, fund, title)

# different TOs label system for the two sources
ESIF_2014_20_fin$to <- ifelse(nchar(as.character(ESIF_2014_20_fin$to))==1,
                              paste0("0",as.character(ESIF_2014_20_fin$to)),
                              as.character(ESIF_2014_20_fin$to))
ESIF_2014_20_fin$to <- as.factor(ESIF_2014_20_fin$to)
table(ESIF_2014_20_fin$to)

## 2) ESIF_2014_20_plan2imp ESIF 2014-2020 categorisation ERDF-ESF-CF planned vs implemente--------------------------------------------
# https://dev.socrata.com/foundry/cohesiondata.ec.europa.eu/3kkx-ekfq
# Dataset Identifier: 3kkx-ekfq
# Total Rows: 337281
# Source Domain: cohesiondata.ec.europa.eu
# Created: 9/29/2017, 9:35:47 AM
# Last Updated: 5/11/2022, 11:40:28 AM
# Category: 2014 / 2020 Categorisation
# Attribution: DG REGIO
# License: Public Domain
# Owner: REGIO IT Reporting
#
# This file contains categorization data from the ERDF/ESF/Cohesion Fund programmes and in particular it compares PLANNED AMOUNTS TO IMPLEMENTED INVESTMENTS.
# This dataset currently contains data to end-2016 and end- 2017. As the data is cumulative the **ANNUAL VALUES SHOULD NOT BE AGGREGATED *** but may be compared to see progress.

ESIF_2014_20_plan2imp <- read.socrata(
  # "https://cohesiondata.ec.europa.eu/resource/3kkx-ekfq.json"#, imprecise datatype
  "https://cohesiondata.ec.europa.eu/2014-2020-Categorisation/ESIF-2014-2020-categorisation-ERDF-ESF-CF-planned-/3kkx-ekfq"#, better datatype
  # app_token =  ...,
  # email     = "...",
  # password  = "..."
)
str(ESIF_2014_20_plan2imp)

#
# different TOs label system for the two sources
ESIF_2014_20_plan2imp$to <- ifelse(nchar(as.character(ESIF_2014_20_plan2imp$to))==1,
                                   paste0("0",as.character(ESIF_2014_20_plan2imp$to)),
                                   as.character(ESIF_2014_20_plan2imp$to))

#ESIF_2014_20_plan2imp$to <- as.factor(ESIF_2014_20_plan2imp$to)
#ESIF_2014_20_plan2imp$ms <- as.factor(ESIF_2014_20_plan2imp$ms)
# ESIF_2014_20_plan2imp$fund <- as.factor(ESIF_2014_20_plan2imp$fund)
# ESIF_2014_20_plan2imp$title <- as.factor(ESIF_2014_20_plan2imp$title)
ESIF_2014_20_plan2imp <- ESIF_2014_20_plan2imp %>%
    modify_at( c("ms", "fund", "title", "to", "to_short", "to_long", "category_of_region", "year" ), as.factor)

ESIF_2014_20_plan2imp <- ESIF_2014_20_plan2imp %>% arrange(ms, fund, to, dimension_code)

## 3) ESIF_2014_20_categ lookup categorization --------------------------------------------
# https://dev.socrata.com/foundry/cohesiondata.ec.europa.eu/xns4-t7ym
ESIF_2014_20_categ <- read.socrata("https://cohesiondata.ec.europa.eu/2014-2020-Categorisation/ESIF-2014-2020-Categorisation-Crosscutting-Themes-/xns4-t7ym")

## 4) Indicators --------------------------------------------
urlInd <- "https://cohesiondata.ec.europa.eu/resource/2q3n-nr7n.json"
dfR <- read.socrata(urlInd)

str(dfR)
# different TOs label system for the two sources
dfR$to <- ifelse(nchar(as.character(dfR$to))==1,paste0("0",as.character(dfR$to)),as.character(dfR$to))

dfR <- dfR %>%
    modify_at( c("ms", "ms_name",  "title", "to", "to_short",  "category_of_region", "year" ), as.factor)

dfR <- dfR %>%
    modify_at( c("implemented_nominator", "implemented_denominator",  "target_value", "forecast_value"),
               as.numeric)

dfR <- dfR %>% arrange(ms, fund, to, ind_code)


#########################################################
################## Load Meta from API ###################  --------------------------------------------------------
#########################################################

urlMetadata <- "http://cohesiondata.ec.europa.eu/api/views/metadata/v1/f6wa-fhmb"
update <- substr(readLines(urlMetadata)[8],22,31)

urlMetadata <- "http://cohesiondata.ec.europa.eu/api/views/metadata/v1/2q3n-nr7n"
update_ind <- substr(readLines(urlMetadata)[8],22,31)

#########################################################
# Filter ERDF & ITALY only --------------------------------------------------------
#########################################################

# select only ERDF  - bc I'm interested in infrastructure (ESF is for social issue)
ERDF_plan  <- ESIF_2014_20_plan %>%
    filter(., fund == 'ERDF') %>% # n 12678 -> 16732
    filter(., ms == 'IT')

ERDF_imp  <- ESIF_2014_20_fin %>%
    filter(., fund == 'ERDF') %>% # n 12678 -> 16732
    filter(., ms == 'IT')

# also for
ERDF_plan2imp  <- ESIF_2014_20_plan2imp %>%
    filter(., fund == 'ERDF') %>% # n 12678
    filter(., ms == 'IT')

# save data  --------------------------------------------------------------

# # 1/2 using for loop
# list_objects1  <-  ls( pattern =  "ESIF*") #, pattern =  "ERDF*") ) # list newly created objects
# list_objects2  <-  ls( pattern =  "ERDF*") #, pattern =  "ERDF*") ) # list newly created objects
#
# list_objects <- c(list_objects1, list_objects2)
# list_objects
# rdsFilesFolder <- fs::path("./data/rawdata_oct2022")
# saveRDSobjects <- paste0(rdsFilesFolder, "/", list_objects, ".Rds")
#
# # [function] write as .Rds
# for (i in seq_along(list_objects)) {
#     base::saveRDS(get(list_objects[i]), file = saveRDSobjects[i])
# }

# 2/2 using purrr
list_objects_pu <- list( categ = ESIF_2014_20_categ,
                         indic = dfR,

                         ESIF_2014_20_plan = ESIF_2014_20_plan,
                         ESIF_2014_20_plan2imp = ESIF_2014_20_plan2imp,
                         ESIF_2014_20_fin = ESIF_2014_20_fin,

                         ERDF_plan = ERDF_plan,
                         ERDF_imp = ERDF_imp,
                         ERDF_plan2imp = ERDF_plan2imp

                         )

purrr::imap(list_objects_pu, ~saveRDS(.x,
                                      # rdsFilesFolder NO bc lacks final "/"
                                      file = paste0("./data/rawdata_oct2022/", .y, ".Rds")))
