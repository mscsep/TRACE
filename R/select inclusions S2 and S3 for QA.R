# select inclusions S2 and S3 (based on data extraction)

rm(list=ls())
library(readxl)
library(dplyr)


S2.animal <- read_excel("data/TRACE data_collection_search2.xlsx", sheet = "animal_inclusions_s2") # some question marks in inclusion.final
S2.human <- read_excel("data/TRACE data_collection_search2.xlsx", sheet = "human_inclusions_s2")

str(S2.animal)

S2.animal %>% filter(!(inclusion.final %in% c("0","1"))) %>% select(`MetaData_FINAL incl/excl_comment`)
S2.human %>% filter(!(inclusion.final %in% c("0","1"))) %>% select(`MetaData_FINAL incl/excl_comment`)

S2.animal$inclusion.final
S2.human$inclusion.final

S3.animal <- read_excel("data/TRACE_data_collection_search3.xlsx", sheet = "animal.inclusions.s3")   # 4 missing values in inclusion.final?
S3.human <- read_excel("data/TRACE_data_collection_search3.xlsx", sheet = "human.inclusions.s3")  # final inclusions missing?

S3.animal %>% filter(!(inclusion.final %in% c("0","1"))) %>% select(`MetaData_FINAL incl/excl_comment`)
S3.human %>% filter(!(inclusion.final %in% c("0","1"))) %>% select(`MetaData_FINAL incl/excl_comment`)

S3.animal$inclusion.final
S3.human$inclusion.final
