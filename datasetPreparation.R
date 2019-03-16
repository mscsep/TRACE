# Environment preparation -------------------------------------------------
rm(list = ls()) #clean environment

#Libraries
library(metafor) #for effect size estimation
library(dplyr) #for general functions

#import full dataset 
#data <- read.csv("TRACE_Dataset.csv", sep = ";", na.strings = c(" ", "-"), dec = c(",", "."))
data <- read.csv("TRACE Dataset v16.3.19.csv", sep = ";", na.strings = c(" ", "-"), dec = c(",", "."))

# get variable codes
factors <- read.csv("TRACE_factors.csv", sep = ";", na.strings = c(" ", "-"), dec = c(",", "."))
factors %>% select(task, measure, type, phase, valence, recode, cuectx) -> factors 
 which(is.na(factors)) # check, should be no missing.

 
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
              
              "Data_Method_TaskSHORT",
              "Data_Method_MeasureSHORT",
              
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
names(dat) <- c("id", "author", "year", "include", "subject", "valence", "phase", "cuectx", "recode", "task", "measure","comparisonControl", "idExp", "idControl",
                "nE", "meanE", "sdE", "semE", "nC", "meanC", "sdC", "semC")







# Select included rows
dat <- dat %>% filter(include == 1) %>% droplevels()


# add factor vars ---------------------------------------------------------

str(factors)
head(factors)

# merge task & measure in a code --> this code is needed to merge 'factors' to data.
dat %>% mutate(measureID = as.factor(paste(task, measure, sep="."))) -> dat
factors %>% mutate(measureID = as.factor(paste(task, measure, sep="."))) -> factors

unique(dat$task)

# factors$measureID
# dat$measureID

which(is.na(dat$task))
which(is.na(dat$measure))

dat<- merge(dat, factors, by="measureID", all=T)





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
# add to dataframe
dat$used<- ifelse(dat$idControl%in%reused_controls$idControl, reused_controls$used, NA )
str(dat)
# Recalculate n's
dat$nC_corrected<-ifelse(!is.na(dat$used), dat$nC/dat$used, dat$nC)
# Check if the effects are correct.. yes
dat %>% 
  filter(idControl %in% reused_controls$idControl)  %>% 
  select(nC, nC_corrected, used) %>% 
#  filter(!is.na(used)) 
head()
# round
dat$nC<-round(dat$nC_corrected)

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
dat %>% filter(!is.na(sdC)) -> dat

dat %>% select(-c(semE, semC, used, nC_corrected)) -> dat # Remove unnessesary colums
which(is.na(dat))

head(is.na(dat))  # -> er zijn nog cuectx missings (Als je die eruit haalt, is niets missing! voor nu zo gelaten, later correcten in coding.)




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
