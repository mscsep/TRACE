# Compare PMIDs in search 1 and 2 of trace & remove duplicates
# code to remove duplicates from search 2 in TRACE project
# written by Milou Sep

# set-up ------------------------------------------------------------------
# install.packages("remotes")
# remotes::install_github("centerforopenscience/osfr")
rm(list=ls())
library(osfr) # to interact with Open Science Framework
# instructions: https://github.com/CenterForOpenScience/osfr
# Authenticate to OSF (see: http://centerforopenscience.github.io/osfr/articles/auth.html). Via osf_auth("PAT") in the commentline. (note PAT can be derived from OSF)
library(readxl)

# retrieve OSF files ------------------------------------------------------
osf_retrieve_file("k3xqw") %>% osf_download() # trace dataset search 1
osf_retrieve_file("7kdmu") %>% osf_download() # human search 2
osf_retrieve_file("fs2gq") %>% osf_download() # animal search 2

osf_retrieve_file("42jr8") %>% osf_download() # human search 3
osf_retrieve_file("ajb8h") %>% osf_download() # animal search 3

# load data search 1 ------------------------------------------------------
# load data search1 animal hits
search1_animal <- read_excel("TRACE Dataset v28.2.19.xlsx", sheet = "Animal hits n=178 SSv18.10.2016", col_types = "numeric")
search1_animal_uPMID <- unique(search1_animal$PMID)
rm(search1_animal)

# load data search1 human hits
search1_human <- read_excel("TRACE Dataset v28.2.19.xlsx",  sheet = "Human Hits n=292 SSv12.10.2016", col_types = "numeric")
search1_human_uPMID <- unique(search1_human$`PMID (op alfabet)`)
rm(search1_human)

# remove last 2 items in vector (these are not PMIDs, but countings/notes)
# search1_animal <- data.frame(as.numeric(search1_animal_uPMID[1:(length(search1_animal_uPMID)-3)]))
search1_animal <- search1_animal_uPMID[1:(length(search1_animal_uPMID)-2)]
search1_human <- search1_human_uPMID[1:(length(search1_human_uPMID)-2)]

# load data search 2 ------------------------------------------------------
# load PMIDs new search
animal_search2 <- read.table("pubmed_result_search2_animal 6.1.20.txt",  quote="\"", comment.char="")
human_search2<- read.table("pubmed_result_search2_human 6.1.20.txt", quote="\"", comment.char="")

# compare search 1 and 2 --------------------------------------------------

# Compare animal hits & and save unique id's to csv
animal_search2[!animal_search2$V1 %in% search1_animal_uPMID,] -> animal_search2_uniq
length(animal_search2_uniq)
# write.table(animal_search2_uniq, file = "animal_search2_unique.csv", sep = ';', row.names = F)

# Compare human hits & and save unique id's to csv
human_search2$V1 %in% search1_human_uPMID #vgl search 2 met search1 "welke hits van search 2 zitten in search 1?"
human_search2[!human_search2$V1 %in% search1_human_uPMID,] -> human_search2_uniq # "select the hits of search 2 that are NOT in search 1"
# write.table(human_search2_uniq, file = "human_search2_unique.csv", sep = ';', row.names = F)

# load search 3 -----------------------------------------------------------
# animal_search3 <- read.table("pmid-search3_animal_18.5.20.txt",  quote="\"", comment.char="")
# human_search3<- read.table("pmid-search3_human_18.5.20.txt", quote="\"", comment.char="")


animal_search3 <- read.table("pmid-search3.learn.animal.txt",  quote="\"", comment.char="")
human_search3<- read.table("pmid-search3.learn.human.txt", quote="\"", comment.char="")

# compare search 1,2,3 ----------------------------------------------------
# After search 2, SC updated (task & behavioral part), script to extract unique id's from new search (=not checked in search 1 or 2)
# 18.5.20 Milou Sep

# Compare human hits 
any(!search1_human %in% human_search2$V1) # All items in search 1 are also in search 2
human_search3[!human_search3$V1 %in% human_search2$V1,]  -> unique.search3.human # therefore search 3 compared to search 2
length(unique.search3.human) # (met cognitie) 412 -> met alleen learning / memory: 286
# write.table(unique.search3.human, file = "human_search3_unique.csv", sep = ';', row.names = F)

# Compare animal hits 
any(!search1_animal %in% animal_search2$V1) # All items in search 1 are also in search 2
animal_search3[!animal_search3$V1 %in% animal_search2$V1,]  -> unique.search3.animal # therefore search 3 compared to search 2
length(unique.search3.animal) # (met cognitie) 318 -> met alleen learning/memory: 291
# write.table(unique.search3.animal, file = "animal_search3_unique.csv", sep = ';', row.names = F)

# remove local copies of downloaded OSF files
file.remove(c("TRACE Dataset v28.2.19.xlsx","pubmed_result_search2_animal 6.1.20.txt","pubmed_result_search2_human 6.1.20.txt",
              "pmid-search3_animal_18.5.20.txt", "pmid-search3_human_18.5.20.txt",
              "pmid-search3.learn.human.txt", "pmid-search3.learn.animal.txt"))
