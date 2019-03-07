# Environment preparation -------------------------------------------------
rm(list = ls()) #clean environment

#Libraries
library(metafor) #for effect size estimation
library(dplyr) #for general functions

#import full dataset 
data <- read.csv("TRACE_Dataset.csv", sep = ";", na.strings = c(" ", "-"), dec = c(",", "."))

# Select relevant data parts of dataset ----------------------------------------
##prepare your dataset
dat <- select(data, 
              "Reference_PMID",
              "Reference_First.Author",
              "Reference_Publication.Year",
              "inclusion",
              "subject",
              "valence",
              "MetaData_Learning.MemoryPhase",
              "MetaData_CueContext.",
              "recode",
              
              "Comparison",
              
              "ID_Experimental_group",
              "ID_Control_group",
              
              "Data_Subjects_n_ptsd",
              "Data_Outcome1_M",
              "Data_Outcome1_SD",
              "Data_Outcome1_SEM",
              
              "Data_Subjects_n_control",
              "Data_Outcome2_M",
              "Data_Outcome2_SD",
              "Data_Outcome2_SEM")

# Rename
names(dat) <- c("id", "author", "year", "include", "subject", "valence", "phase", "cuectx", "recode", "comparisonControl", "idExp", "idControl",
                "nE", "meanE", "sdE", "semE", "nC", "meanC", "sdC", "semC")

# Select included rows
dat <- dat %>% filter(include == 1) %>% droplevels()

# Recode subjecttype
dat$subject <- ifelse(dat$subject <= 3, "Human", "Animal")

# check missing of all variables of interst
which(is.na(dat$subject))
which(is.na(dat$id))
which(is.na(dat$comparisonControl))


# Set variable properties -------------------------------------------------
# Change factors to numbers
stat.vars <- c("nE", "meanE", "sdE", "semE", "nC", "meanC", "sdC", "semC", "recode")
for(i in 1:length(stat.vars)){
  dat[,stat.vars[i]]<- sub("-", "-", as.character(dat[,stat.vars[i]]), fixed = TRUE) # Correction of - signs
  dat[,stat.vars[i]]<- as.numeric(sub(",", ".", as.character(dat[,stat.vars[i]]), fixed = TRUE))
}
# Create factors from character/numeric
factor.vars<-c("id", "include", "subject", "valence","phase", "cuectx","comparisonControl", "idExp", "idControl")
dat<-mutate_each(dat, as.factor, factor.vars)
# Create reference var as character
dat<-dat %>% mutate(reference = as.character(paste(author, year, sep=" "))) %>% select(-c(author, year)) # Merge year & author & dropvars.

# Check
str(dat)
# all.equal(dat$id, dat1$id) # test differences
# all.equal(dat$subject, dat1$subject)

# max(dat$nC) # --> hier al probleem met 923  # Dit zijn human papers met code 1
# dat[which(dat$nC == "923"),]   # en 339 veel te hoog...
# dat %>% filter(nC == 923)


# Corrections to statistical measurements -------------------------------------------------------------

# Correction for multiple use control group -------------------------------
# Merge ID's of experimental and control groups.
dat <- dat %>% mutate(id_combination = as.factor(paste(idExp, idControl, sep=".")))
str(dat)

# Check which control groups are used multiple times
reused_controls <- dat %>%
  select(idExp, idControl, id_combination)%>%
  group_by(idControl) %>% # for each control groups
  summarise (used=length(unique(id_combination)))%>% # count the amount of unique id combinations
  filter(used>1) # show control groups in which a unique combination is precent more than once

reused_controls # 10 control groups are used in multiple unique id_combinations
# Check results
dat %>% filter(idControl%in%reused_controls$idControl)

# merge to dataframe
dat1<-merge(dat,reused_controls, by="idControl", all.x = T)
# Recalculate n's
dat1$nC_corrected<-ifelse(!is.na(dat1$used), dat1$nC/dat1$used, dat1$nC )
# Check if the effects are correct.. yes
dat1 %>% 
  filter(idControl %in% reused_controls$idControl)  %>% 
  select(nC, nC_corrected, used) %>% 
  filter(!is.na(used)) 
#head()

dat$nC<-round(dat1$nC_corrected)







# Calculate SD from SEM ---------------------------------------------------
dat$sdE <- ifelse(is.na(dat$sdE), (dat$semE * sqrt(dat$nE)), dat$sdE)
dat$sdC <- ifelse(is.na(dat$sdC), (dat$semC * sqrt(dat$nC)), dat$sdC)


# Missing Values ----------------------------------------------------------
# Check Missing values
which(is.na(dat$sdE))
which(is.na(dat$nE))
which(is.na(dat$nC))
which(is.na(dat$sdC)) # erblijven missing na's over...
# Missing values SD? Original datafile checked: 5 papers don't report sem or sd: PMID: 7654154, 8731522, 9821567, 17392739, 27297027
unique(dat[which(is.na(dat$sdC)),"id"]) # Check of dit overeenkomt.. Klopt.
# exclude missing values
dat <- dat[-which(is.na(dat$sdC)),]

which(is.na(dat))


# # old code valeria
# ##correction for qualitative interpretation direction (for systematic review graphs)
# data$directionQual <- (as.numeric(factor(data$directionGrouped, 
#                                          levels = c("decrease", "ns", "increase"))) - 2) #convert direction reported by studies to numeric
# 
# data$directionQual <- data$directionQual * data$multiply #correct direction of effects reported by studies according to categorization rules
# 
# data$directionGrouped <- ifelse(is.na(data$directionQual), "notRetrievable",
#                                 ifelse(data$directionQual == -1, "decrease",
#                                        ifelse(data$directionQual == 1, "increase", "ns"))) #convert numeric to interpretation
# 
# #convert decrease with increase and viceversa for nsLearning and social
# data$directionGrouped <- ifelse(data$domain %in% c("nsLearning", "social") & data$directionGrouped == "decrease", 
#                                 "increase",
#                                 ifelse(data$domain %in% c("nsLearning", "social") & data$directionGrouped == "increase", 
#                                        "decrease", data$directionGrouped))
# 
# data$directionGrouped <- as.factor(data$directionGrouped)


# Calculation effect size and checks --------------------------------------
##calculate effect size
dat <- escalc(m1i = meanE, sd1i = sdE, n1i = nE, 
              m2i = meanC, sd2i = sdC, n2i = nC, 
              measure = "SMD", method = "HE",  # calc hedge's G
              data = dat)

#dat$yi <- ifelse(data$each %% 2 == 0, dat$yi * -1, dat$yi) ##for blinding
dat$yi <- dat$yi * dat$recode #give all effect sizes the correct direction  (higher score, better performance)

# INTERPRETATIE
# Viechtbauer, W. (2010). Conducting Meta-Analyses in R with the metafor Package. Journal of Statistical Software, 1–48. Page 7
# "SMD": The standardized mean difference is equal to (m1i m2i)/spi
# 1 = ptsd en 2 = control
# DUS positive effect size is PTSD meer; Neg effect size is HC meer


# Save resulting dataset --------------------------------------------------
dat <- dat %>% select(-c(id_combination)) %>% droplevels() #drop missing levels & Remove 'unique id combination' variable (not needed anymore)

str(dat)

save(dat, file = "data.RData") #save
