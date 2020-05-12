# compare abstract screening search 2
# written by Milou Sep (8.5.20)

rm(list=ls())
library(osfr)
library(readxl)
library(dplyr)

# Authenticate to OSF (see: http://centerforopenscience.github.io/osfr/articles/auth.html). Via osf_auth("PAT") in the commentline. (note PAT can be derived from OSF)

# load data
osf_retrieve_file("pu6bf") %>% osf_download() #abstract screening search2 SH
osf_retrieve_file("56fyu") %>% osf_download() #abstract screening search2 MS

SH_animal <- read_excel("TRACE_screening_search2_SH.xlsx", sheet = "screening search 2 animal")#, col_types = "numeric")
MS_animal <- read_excel("TRACE_screening_search2_MS.xlsx", sheet = "screening search 2 animal")#, col_types = "numeric")

SH_human <- read_excel("TRACE_screening_search2_SH.xlsx", sheet = "screening search 2 human")#, col_types = "numeric")
MS_human <- read_excel("TRACE_screening_search2_MS.xlsx", sheet = "screening search 2 human")#, col_types = "numeric")


# remove rows with all NA's in screening SH
SH_animal[rowSums(is.na(SH_animal)) != ncol(SH_animal), ]->SH_animal
SH_human[rowSums(is.na(SH_human)) != ncol(SH_human), ]->SH_human

# make unique colnames
colnames(SH_animal) <- paste(colnames(SH_animal),"SH", sep = "_")
colnames(MS_animal) <- paste(colnames(MS_animal),"MS", sep = "_")
# 
colnames(SH_human) <- paste(colnames(SH_human),"SH", sep = "_")
colnames(MS_human) <- paste(colnames(MS_human),"MS", sep = "_")


# order animal data by PMID 
SH_animal[order(SH_animal$Reference_PMID_SH),]->SH_animal2
MS_animal[order(MS_animal$Reference_PMID_MS),]->MS_animal2
# check if ordering is the same:
all_equal(SH_animal2$Reference_PMID_SH,MS_animal2$Reference_PMID_MS)

# order human data by PMID 
SH_human[order(SH_human$Reference_PMID_SH),]->SH_human2
MS_human[order(MS_human$Reference_PMID_MS),]->MS_human2
# check if ordering is the same:
all_equal(SH_human2$Reference_PMID_SH,MS_human2$Reference_PMID_MS)

# merge data
cbind(SH_animal2, MS_animal2)->animal
cbind(SH_human2, MS_human2)->human

# select only PMID & ref.
# human %>% select(Reference_PMID_SH, inclusion_SH, Reference_PMID_MS, inclusion_MS) ->df.h
# animal %>% select(Reference_PMID_SH, Inclusion_SH, Reference_PMID_MS, inclusion_MS) ->df.a

# recode to same scale: 1=inclusion; 0=exclusion; ?=full text check needed
human %>% mutate(
  inclusion_MS=
    case_when(
      inclusion_MS==1 ~"yes",
      inclusion_MS==0 ~"no",
      grepl("?",inclusion_MS, fixed = T) ~ '?')
)->human_recoded

animal %>% mutate(
  inclusion_MS=
    case_when(
      inclusion_MS==1 ~"yes",
      inclusion_MS==0 ~"no",
      grepl("?",inclusion_MS, fixed = T) ~ '?')
) ->animal_recoded


# select equal rows
# df.a2 %>% filter((Reference_PMID_SH==Reference_PMID_MS) & (Inclusion_SH == inclusion_MS)) %>% nrow()
animal_recoded %>% mutate(
  conclusion1= case_when(
    # Inclusion_SH == '?' ~ 'full_text_check',
    # Inclusion_MS == '?' ~ 'full_text_check',
    (Reference_PMID_SH!=Reference_PMID_MS) ~'PMIDincor',
    # (Reference_PMID_SH==Reference_PMID_MS) & Inclusion_SH != '?' & (Inclusion_SH == inclusion_MS) ~ 'equal',
    (Reference_PMID_SH==Reference_PMID_MS) & (Inclusion_SH == inclusion_MS) ~ 'same',
    (Reference_PMID_SH==Reference_PMID_MS) & (Inclusion_SH != inclusion_MS) ~ 'discuss'
  )
)->animal_recoded2 

human_recoded %>% mutate(
  conclusion1= case_when(
    (Reference_PMID_SH!=Reference_PMID_MS) ~'PMIDincor',
    (Reference_PMID_SH==Reference_PMID_MS) &  (inclusion_SH == inclusion_MS) ~ 'same',
    # (Reference_PMID_SH==Reference_PMID_MS) & (inclusion_SH != '?' | inclusion_MS!="?") & (inclusion_SH == inclusion_MS) ~ 'same',
    # (Reference_PMID_SH==Reference_PMID_MS) & (inclusion_SH == '?' | inclusion_MS=="?") ~ 'full_text_check',
    (Reference_PMID_SH==Reference_PMID_MS) & (inclusion_SH != inclusion_MS) ~ 'discuss'
 )
) ->human_recoded2 

# select papers to discuss
# df.h2 %>% filter(conclusion=='discuss') %>% write.csv2(.,file='inconsistencies_human.csv')
# df.a2 %>% filter(conclusion=='discuss') %>% write.csv2(.,file='inconsistencies_animal.csv')

# create vectors with PMIDs that need to be discussed:
human_recoded2$Reference_PMID_MS[which(human_recoded2$conclusion=='discuss')]->pmid.h
animal_recoded2$Reference_PMID_MS[which(animal_recoded2$conclusion=='discuss')]->pmid.a

# select original entries for these PMIDs
human_recoded2 %>% filter(Reference_PMID_MS %in% pmid.h) %>% write.csv2(.,file='inconsistencies_human_s2.csv')
animal_recoded2 %>% filter(Reference_PMID_MS %in% pmid.a) %>% write.csv2(.,file='inconsistencies_animal_s2.csv')

# upload files to OSF
  osf_upload(osf_retrieve_node("awkn6"),'inconsistencies_human_s2.csv')
  osf_upload(osf_retrieve_node("awkn6"),'inconsistencies_animal_s2.csv')
  
#remove downloaded files
file.remove(c("TRACE_screening_search2_SH.xlsx","TRACE_screening_search2_MS.xlsx","inconsistencies_human_s2.csv", "inconsistencies_animal_s2.csv"))


# Merge data after discussion inconsitencies ------------------------------
# 12.5.20

osf_retrieve_file("ugyrp") %>% osf_download() # discussed animal
osf_retrieve_file("kve78") %>% osf_download() # discussed human data

animal_discussed <- read.csv2("inconsistencies_animal_s2_SH.csv")
human_discussed <- read.csv2("inconsistencies_human_s2_SH.csv")
#remove colums with only na's
human_discussed <- human_discussed[,colSums(is.na(human_discussed))<nrow(human_discussed)]

# human_discussed <- human_discussed[rowSums(is.na(human_discussed))<nrow(human_discussed),]
# animal_discussed <- animal_discussed[rowSums(is.na(animal_discussed))<nrow(animal_discussed),]


# merge colums from discussion to original screening data
animal_discussed %>% select(Reference_PMID_SH,!grep("_SH|_MS", colnames(animal_discussed)))->animal_discussed2
human_discussed %>% select(Reference_PMID_SH,!grep("_SH|_MS", colnames(human_discussed)))->human_discussed2

# merge data
full_join(human_recoded2,human_discussed2, by="Reference_PMID_SH")->human_new
full_join(animal_recoded2,animal_discussed2, by="Reference_PMID_SH")->animal_new


# Create colum with new descisions:

# rename "Second.screening..yes...inclusie..no..exlusie....discussieren" to more convenient colname
 colnames(human_new)[40]<-"second.screening"
 colnames(animal_new)[43]<-"second.screening"
 
# new conclusion colum human
human_new %>% mutate(
  conclusion2=
    case_when(
      conclusion1 == "same"  ~ inclusion_MS,
      (conclusion1 == "discuss" & (second.screening =="yes"| second.screening =="no")) ~ as.character(second.screening),
      (conclusion1 == "discuss" & (second.screening !="yes"| second.screening !="no")) ~ as.character(Final),
     # (conclusion1 == "discuss" & !is.na(second.screening) & is.na(Final) ) ~ as.character(second.screening),
     # (conclusion1 == "discuss" & !is.na(second.screening) )  ~ as.character(Final))
      )
)->human_new2
# check, there should be no na's
is.na(human_new2$conclusion2) %>% any()

# new conclusion colum human
animal_new %>% mutate(
  conclusion2=
    case_when(
      conclusion1 == "same"  ~ inclusion_MS,
      (conclusion1 == "discuss" & (second.screening =="yes"| second.screening =="no")) ~ as.character(second.screening),
      (conclusion1 == "discuss" & (second.screening !="yes"| second.screening !="no")) ~ as.character(Final),
      # (conclusion1 == "discuss" & !is.na(second.screening) & is.na(Final) ) ~ as.character(second.screening),
      # (conclusion1 == "discuss" & !is.na(second.screening) )  ~ as.character(Final))
    )
)->animal_new2
# check, there should be no na's
is.na(animal_new2$conclusion2) %>% any()

# export new datasets
human_new2 %>% filter(conclusion2 == "?") %>% write.csv2(.,file='required_full_checks_human_s2.csv')
animal_new2 %>% filter(conclusion2 == "?") %>% write.csv2(.,file='required_full_checks_animal_s2.csv')

# upload files to OSF
 osf_upload(osf_retrieve_node("awkn6"),'required_full_checks_human_s2.csv')
 osf_upload(osf_retrieve_node("awkn6"),'required_full_checks_animal_s2.csv')

#remove downloaded files
file.remove(c("inconsistencies_human_s2_SH.csv", "inconsistencies_animal_s2_SH.csv"))
# file.remove(c('required_full_checks_human_s2.csv', 'required_full_checks_animal_s2.csv'))
 
 