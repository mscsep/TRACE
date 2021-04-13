rm(list = ls()) #clean environment
# load files from OSF
library(osfr)
# Authenticate to OSF (see: http://centerforopenscience.github.io/osfr/articles/auth.html). Via osf_auth("PAT") in the commentline. (note PAT can be derived from OSF)

library(readxl)
library(dplyr)

# new data
osf_retrieve_file("tqmwb") %>% osf_download(path = "data", conflicts="overwrite") # search 1
osf_retrieve_file("xqpm3") %>% osf_download(path = "data", conflicts="overwrite") # search 2
osf_retrieve_file("wbrcm") %>% osf_download(path = "data", conflicts="overwrite") # search 3



s1 <- read_excel("data/TRACE data_collection_search1.xlsx", na = c(" ", "-", "not reported"), col_types = "text")#s1
s2a <- read_excel("data/TRACE data_collection_search2.xlsx", sheet = "animal_inclusions_s2", na = c(" ", "-", "not reported"), col_types = "text")#s2
s2h <- read_excel("data/TRACE data_collection_search2.xlsx", sheet = "human_inclusions_s2", na = c(" ", "-", "not reported"), col_types = "text")#s2
s3a <- read_excel("data/TRACE_data_collection_search3.xlsx", sheet = "animal.inclusions.s3", na = c(" ", "-", "not reported"), col_types = "text")#s3
s3h <- read_excel("data/TRACE_data_collection_search3.xlsx", sheet = "human.inclusions.s3", na = c(" ", "-", "not reported"), col_types = "text")#s3

# old data (for comparison)

s1_old <- read_excel("data/s1_old.xlsx", na = c(" ", "-", "not reported"), col_types = "text")#s1
s2a_old <- read_excel("data/s2_old.xlsx", sheet = "animal_inclusions_s2", na = c(" ", "-", "not reported"), col_types = "text")#s2
s2h_old <- read_excel("data/s2_old.xlsx", sheet = "human_inclusions_s2", na = c(" ", "-", "not reported"), col_types = "text")#s2
s3a_old <- read_excel("data/s3_old.xlsx", sheet = "animal.inclusions.s3", na = c(" ", "-", "not reported"), col_types = "text")#s3
s3h_old <- read_excel("data/s3_old.xlsx", sheet = "human.inclusions.s3", na = c(" ", "-", "not reported"), col_types = "text")#s3



# S1 animal & human -------------------------------------------------------
# s1 comparison (human & animal together)
s1$Data_Subjects_PTSDtypeSHORT2
s1_old$Data_Subjects_PTSDtypeSHORT2
str(s1)

s1 %>% select(inclusion, Reference_PMID, ID_Experimental_group, ID_Control_group, Data_Subjects_PTSDtypeSHORT2, Data_Subjects_Strain_or_Population)->s1.1
s1_old %>% select(Reference_PMID,ID_Experimental_group, ID_Control_group, Data_Subjects_PTSDtypeSHORT2, Data_Subjects_Strain_or_Population)->s1.old.1

# only included papers checked!
full_join(s1.1, s1.old.1, by=c("Reference_PMID","ID_Experimental_group", "ID_Control_group") ) %>% filter(inclusion == 1) ->df1
str(df1)

df1 %>% filter(Data_Subjects_PTSDtypeSHORT2.x != Data_Subjects_PTSDtypeSHORT2.y) %>% View() # differences correct different
df1 %>% filter(Data_Subjects_Strain_or_Population.x != Data_Subjects_Strain_or_Population.y) %>% View() # after 1 correction all correct


# S2 animal ---------------------------------------------------------------
str(s2a)

s2a %>% select(inclusion.final, Reference_PMID_SH, ID_Experimental_group, ID_Control_group, Data_Subjects_PTSDtypeSHORT2, Data_Subjects_Strain_or_Population)->s2a.1
s2a_old %>% select(Reference_PMID_SH,ID_Experimental_group, ID_Control_group, Data_Subjects_PTSDtypeSHORT2, Data_Subjects_Strain_or_Population)->s2a.old.1

# only included papers checked!
full_join(s2a.1, s2a.old.1, by=c("Reference_PMID_SH","ID_Experimental_group", "ID_Control_group") ) %>% filter(inclusion.final == 1) ->df2a
str(df2a)

df2a %>% filter(Data_Subjects_PTSDtypeSHORT2.x != Data_Subjects_PTSDtypeSHORT2.y) %>% View() # differences correct different
df2a %>% filter(Data_Subjects_Strain_or_Population.x != Data_Subjects_Strain_or_Population.y) %>% View() # all were correct


# S2 human ----------------------------------------------------------------
s2h %>% select(inclusion.final, Reference_PMID_SH, ID_Experimental_group, ID_Control_group, Data_Subjects_PTSDtypeSHORT2, Data_Subjects_Strain_or_Population)->s2h.1
s2h_old %>% select(Reference_PMID_SH,ID_Experimental_group, ID_Control_group, Data_Subjects_PTSDtypeSHORT2, Data_Subjects_Strain_or_Population)->s2h.old.1

# only included papers checked!
full_join(s2h.1, s2h.old.1, by=c("Reference_PMID_SH","ID_Experimental_group", "ID_Control_group") ) %>% filter(inclusion.final == 1) ->df2h

str(df2h)
df2h %>% filter(Data_Subjects_PTSDtypeSHORT2.x != Data_Subjects_PTSDtypeSHORT2.y) %>% View() # 0 (so all correct)
df2h %>% filter(Data_Subjects_Strain_or_Population.x != Data_Subjects_Strain_or_Population.y) %>% View() # 0 (so all correct)




# S3 animal ---------------------------------------------------------------

s3a %>% select(inclusion.final, PMID, ID_Experimental_group, ID_Control_group, Data_Subjects_PTSDtypeSHORT2, Data_Subjects_Strain_or_Population)->s3a.1
s3a_old %>% select(PMID,ID_Experimental_group, ID_Control_group, Data_Subjects_PTSDtypeSHORT2, Data_Subjects_Strain_or_Population)->s3a.old.1

str(s3a)
# only included papers checked!
full_join(s3a.1, s3a.old.1, by=c("PMID","ID_Experimental_group", "ID_Control_group") ) %>% filter(inclusion.final == 1) ->df3a

df3a %>% filter(Data_Subjects_PTSDtypeSHORT2.x != Data_Subjects_PTSDtypeSHORT2.y) %>% View() # differences correct different
df3a %>% filter(Data_Subjects_Strain_or_Population.x != Data_Subjects_Strain_or_Population.y) %>% View() # 1 difference.. was correct in new file



# S3 human ----------------------------------------------------------------

s3h %>% select(inclusion.final, PMID, ID_Experimental_group, ID_Control_group, Data_Subjects_PTSDtypeSHORT2, Data_Subjects_Strain_or_Population)->s3h.1
s3h_old %>% select(PMID,ID_Experimental_group, ID_Control_group, Data_Subjects_PTSDtypeSHORT2, Data_Subjects_Strain_or_Population)->s3h.old.1


# only included papers checked!
full_join(s3h.1, s3h.old.1, by=c("PMID","ID_Experimental_group", "ID_Control_group") ) %>% filter(inclusion.final == 1) ->df3h

df3h %>% filter(Data_Subjects_PTSDtypeSHORT2.x != Data_Subjects_PTSDtypeSHORT2.y) %>% View() # differences correct different
df3h %>% filter(Data_Subjects_Strain_or_Population.x != Data_Subjects_Strain_or_Population.y) %>% View() # correct
