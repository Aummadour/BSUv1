# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# IMPORTING DATA INTO R ----


# 1.0 Load libraries ----

# Contains readr
library(tidyverse)   

# Excel Connection
library(readxl)
library(writexl)

# Database Connection
library(odbc)
library(RSQLite)



# 2.0 readr ----

# 2.1 CSV ----
bike_orderlines_wrangled_tbl 

readr::read_csv("00_data/bike_sales/data_wrangled/bike_orderlines.csv")


# 2.2 RDS ----

readr::read_rds("00_data/bike_sales/data_wrangled/bike_orderlines.rds")


# 3.0 Excel ----

readr:read_xlsx("00_data/bike_sales/data_wrangled/bike_orderlines.xlsx")


# 4.0 Databases  ----


