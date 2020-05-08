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
human %>% select(Reference_PMID_SH, inclusion_SH, Reference_PMID_MS, inclusion_MS) ->df.h
animal %>% select(Reference_PMID_SH, Inclusion_SH, Reference_PMID_MS, inclusion_MS) ->df.a

# recode to same scale: 1=inclusion; 0=exclusion; ?=full text check needed
df.h %>% mutate(
  inclusion_MS=
    case_when(
      inclusion_MS==1 ~"yes",
      inclusion_MS==0 ~"no",
      grepl("?",inclusion_MS, fixed = T) ~ '?'
    ),
  inclusion_SH=
    tidyr::replace_na(.$inclusion_SH, "?") # is this correct?
)->df.h2

df.a %>% mutate(
  inclusion_MS=
    case_when(
      inclusion_MS==1 ~"yes",
      inclusion_MS==0 ~"no",
      grepl("?",inclusion_MS, fixed = T) ~ '?'
    ),
  Inclusion_SH=
    tidyr::replace_na(.$Inclusion_SH, "?") # is this correct?
) ->df.a2


# select equal rows
# df.a2 %>% filter((Reference_PMID_SH==Reference_PMID_MS) & (Inclusion_SH == inclusion_MS)) %>% nrow()
df.a2 %>% mutate(
  conclusion= case_when(
    # Inclusion_SH == '?' ~ 'full_text_check',
    # Inclusion_MS == '?' ~ 'full_text_check',
    (Reference_PMID_SH!=Reference_PMID_MS) ~'PMIDincor',
    # (Reference_PMID_SH==Reference_PMID_MS) & Inclusion_SH != '?' & (Inclusion_SH == inclusion_MS) ~ 'equal',
    (Reference_PMID_SH==Reference_PMID_MS) & (Inclusion_SH == inclusion_MS) ~ 'same',
    (Reference_PMID_SH==Reference_PMID_MS) & (Inclusion_SH != inclusion_MS) ~ 'discuss'
  )
)->df.a2 

df.h2 %>% mutate(
  conclusion= case_when(
    (Reference_PMID_SH!=Reference_PMID_MS) ~'PMIDincor',
    (Reference_PMID_SH==Reference_PMID_MS) &  (inclusion_SH == inclusion_MS) ~ 'same',
    # (Reference_PMID_SH==Reference_PMID_MS) & (inclusion_SH != '?' | inclusion_MS!="?") & (inclusion_SH == inclusion_MS) ~ 'same',
    # (Reference_PMID_SH==Reference_PMID_MS) & (inclusion_SH == '?' | inclusion_MS=="?") ~ 'full_text_check',
    (Reference_PMID_SH==Reference_PMID_MS) & (inclusion_SH != inclusion_MS) ~ 'discuss'
 )
) ->df.h2 

# select papers to discuss
# df.h2 %>% filter(conclusion=='discuss') %>% write.csv2(.,file='inconsistencies_human.csv')
# df.a2 %>% filter(conclusion=='discuss') %>% write.csv2(.,file='inconsistencies_animal.csv')

# create vectors with PMIDs that need to be discussed:
df.h2$Reference_PMID_MS[which(df.h2$conclusion=='discuss')]->pmid.h
df.a2$Reference_PMID_MS[which(df.a2$conclusion=='discuss')]->pmid.a

# select original entries for these PMIDs
human %>% filter(Reference_PMID_MS %in% pmid.h) %>% write.csv2(.,file='inconsistencies_human_s2.csv')
animal %>% filter(Reference_PMID_MS %in% pmid.a) %>% write.csv2(.,file='inconsistencies_animal_s2.csv')

# upload files to OSF
  osf_upload(osf_retrieve_node("awkn6"),'inconsistencies_human_s2.csv')
  osf_upload(osf_retrieve_node("awkn6"),'inconsistencies_animal_s2.csv')
  
#remove downloaded files
file.remove(c("TRACE_screening_search2_SH.xlsx","TRACE_screening_search2_MS.xlsx","inconsistencies_human.csv", "inconsistencies_animal.csv"))
