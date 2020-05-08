# compare abstract screening search 2
# written by Milou Sep (8.5.20)

rm(list=ls())
library(osfr)
library(readxl)

# Authenticate to OSF (see: http://centerforopenscience.github.io/osfr/articles/auth.html). Via osf_auth("PAT") in the commentline. (note PAT can be derived from OSF)

# load data
osf_retrieve_file("pu6bf") %>% osf_download() #abstract screening search2 SH
osf_retrieve_file("56fyu") %>% osf_download() #abstract screening search2 MS

SH_animal <- read_excel("TRACE_screening_search2_SH.xlsx", sheet = "screening search 2 animal")#, col_types = "numeric")
MS_animal <- read_excel("TRACE_screening_search2_MS.xlsx", sheet = "screening search 2 animal")#, col_types = "numeric")

SH_human <- read_excel("TRACE_screening_search2_SH.xlsx", sheet = "screening search 2 human")#, col_types = "numeric")
MS_human <- read_excel("TRACE_screening_search2_MS.xlsx", sheet = "screening search 2 human")#, col_types = "numeric")

#file.remove(c("TRACE_screening_search2_SH.xlsx","TRACE_screening_search2_MS.xlsx"))

# remove rows with all NA's in screening SH
SH_animal[rowSums(is.na(SH_animal)) != ncol(SH_animal), ]->SH_animal
SH_human[rowSums(is.na(SH_human)) != ncol(SH_human), ]->SH_human

# make unique colnames
colnames(SH_animal) <- paste(colnames(SH_animal),"SH", sep = "_")
colnames(MS_animal) <- paste(colnames(MS_animal),"MS", sep = "_")

colnames(SH_human) <- paste(colnames(SH_human),"SH", sep = "_")
colnames(MS_human) <- paste(colnames(MS_human),"MS", sep = "_")

# compare human
library(dplyr)
SH_human %>% select(Reference_PMID_SH, inclusion_SH)
