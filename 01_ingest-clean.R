
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


## 1) ESIF_2014_20 Finance Implementation Details --------------------------------------------

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
## 2) ESIF_2014_20_plan2imp ESIF 2014-2020 categorisation ERDF-ESF-CF planned vs implemente--------------------------------------------
# https://dev.socrata.com/foundry/cohesiondata.ec.europa.eu/3kkx-ekfq
# This file contains categorization data from the ERDF/ESF/Cohesion Fund programmes and in particular it compares PLANNED AMOUNTS TO IMPLEMENTED INVESTMENTS.
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
ESIF_2014_20$reference_date <- lubridate::as_date(ESIF_2014_20$reference_date)


# save data  --------------------------------------------------------------

# 1/2 using for loop
list_objects  <-  ls(pattern = "ESIF*" ) # list newly created objects
list_objects
rdsFilesFolder <- fs::path("./data/rawdata_oct2022")
saveRDSobjects <- paste0(rdsFilesFolder, "/", list_objects, ".Rds")

# [function] write as .Rds
for (i in seq_along(list_objects)) {
    base::saveRDS(get(list_objects[i]), file = saveRDSobjects[i])
}
