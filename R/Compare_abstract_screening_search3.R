# compare abstract screening search 3
# written by Milou Sep (30.9.20)

rm(list=ls())
library(osfr)
library(readxl)
library(dplyr)

# Authenticate to OSF (see: http://centerforopenscience.github.io/osfr/articles/auth.html). Via osf_auth("PAT") in the commentline. (note PAT can be derived from OSF)

#load data

S3.animal <- read_excel("Checked_TRACE_screening_search3_MS_EG.xlsx", sheet = "full.text.screening.s3.animal")#, col_types = "numeric")
S3.human <- read_excel("Checked_TRACE_screening_search3_MS_EG.xlsx", sheet = "full.text.screening.s3.human")#, col_types = "numeric")

S3.animal %>% filter(inclusion.screening != "0") %>% write.csv2(.,file='animal.inclusions.s3.csv')
S3.human %>% filter(inclusion.screening != "0") %>% write.csv2(.,file='human.inclusions.s3.csv')

# order animal data by PMID 
# animal.inclusions[order(animal.inclusions$PMID),]

# upload files to OSF
osf_upload(osf_retrieve_node("e9wmt"),'animal.inclusions.s3.csv')
osf_upload(osf_retrieve_node("e9wmt"),'human.inclusions.s3.csv')