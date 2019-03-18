#' ---
#' title: "Datapreparation TRACE dataset"
#' author: "Valeria Bonapersona & Milou Sep"
#' date: "March 2019"
#' ---


#' **Preparation of the environment**
#+ include=F
rm(list = ls()) #clean environment
# libraries
library(metafor) # for effect size estimation
library(dplyr) #for general functions
# import .csv files 
data <- read.csv("TRACE_data.csv", sep = ";", na.strings = c(" ", "-"), dec = c(",", ".")) # Data
method <- read.csv("TRACE_method_codes.csv", sep = ";", na.strings = c(" ", "-"), dec = c(",", ".")) # Method codes
# which(is.na(method)) # check, should be no missing.


#' **Select relevant colums dataset**
method %>% select(task, measure, type, phase, valence, recode, cuectx) -> method 

dat <- select(data, 
              "Reference_PMID",
              "Reference_First.Author",
              "Reference_Publication.Year",
              "inclusion",
              "subject",
              "task",
              "measure",
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
names(dat) <- c("id", "author", "year", "include", "subject", "task", "measure","comparisonControl", "idExp", "idControl",
                "nE", "meanE", "sdE", "semE", "nC", "meanC", "sdC", "semC")
# Create reference var as character
dat<-dat %>% mutate(reference = as.character(paste(author, year, sep=" "))) %>% select(-c(author, year)) # Merge year & author & dropvars.


#' **Select only data that is included in the meta-analysis**
dat <- dat %>% filter(include == 1) %>% droplevels()


#' **Add method codes to data**
#' NB the combination of task and measure indicates which method is used to measure behavior. Those values are combined into a new variable ..
dat %>% mutate(measureID = as.factor(paste(task, measure, sep="."))) -> dat
method %>% mutate(measureID = as.factor(paste(task, measure, sep="."))) -> method
#' ... that is used to merge data and method codes
dat<- merge(dat, method, by="measureID", all=T, suffixes = c("_d", "_m"))

#' Quality check after merging.. (should be identical)
 identical(as.character(dat$task.d), as.character(dat$task.m ))
 identical(as.character(dat$measure.d), as.character(dat$measure.m ))
# dat %>% select(-c(task_m, measure_m)) -> dat #remove duplicate colums (niet echt nodig...)


#' **Recode subjecttype**
dat$subject <- ifelse(dat$subject <= 3, "Human", "Animal")

# check missing of all variables of interst
which(is.na(dat$subject))
which(is.na(dat$id))
 which(is.na(dat$comparisonControl))


#' **Set variable properties**
#' Change factors to numbers:
stat.vars <- c("nE", "meanE", "sdE", "semE", "nC", "meanC", "sdC", "semC", "recode")
for(i in 1:length(stat.vars)){
  dat[,stat.vars[i]]<- sub("-", "-", as.character(dat[,stat.vars[i]]), fixed = TRUE) # Correction of - signs
  dat[,stat.vars[i]]<- as.numeric(sub(",", ".", as.character(dat[,stat.vars[i]]), fixed = TRUE))
}
#' and create factors from character/numeric:
factor.vars<-c("id", "include", "subject", "comparisonControl", "idExp", "idControl")
dat<-mutate_each(dat, as.factor, factor.vars)



#' **Correction for multiple use control group**
#' Merge ID's of experimental and control groups.
dat <- dat %>% mutate(id_combination = as.factor(paste(idExp, idControl, sep=".")))
#str(dat)

#' Check which control groups are used multiple times
reused_controls <- dat %>%
  select(idExp, idControl, id_combination)%>%
  group_by(idControl) %>% # for each control groups
  summarise (used=length(unique(id_combination)))%>% # count the amount of unique id combinations
  filter(used>1) # show control groups in which a unique combination is precent more than once
reused_controls # 10 control groups are used in multiple unique id_combinations # 18.3.19 11 control grousp used multiple times
#' Check results
#+ eval=F
dat %>% filter(idControl%in%reused_controls$idControl) %>% tibble
#' add to dataframe
dat$used<- ifelse(dat$idControl%in%reused_controls$idControl, reused_controls$used, NA )
#' Recalculate n's
dat$nC_corrected<-ifelse(!is.na(dat$used), dat$nC/dat$used, dat$nC)
#' Check if the effects are correct.. yes
dat %>% 
  filter(idControl %in% reused_controls$idControl)  %>% 
  select(nC, nC_corrected, used) %>% head()
# round
dat$nC<-round(dat$nC_corrected)


#' **Calculate SD from SEM**
dat$sdE <- ifelse(is.na(dat$sdE), (dat$semE * sqrt(dat$nE)), dat$sdE)
dat$sdC <- ifelse(is.na(dat$sdC), (dat$semC * sqrt(dat$nC)), dat$sdC)


#' **Missing values**
# Check Missing values
which(is.na(dat$sdE))
which(is.na(dat$nE))
which(is.na(dat$nC))
which(is.na(dat$sdC)) # erblijven missing na's over...
#' Missing values SD? Original datafile checked: 5 papers don't report sem or sd: PMID: 7654154, 8731522, 9821567, 17392739, 27297027
unique(dat[which(is.na(dat$sdC)),"id"]) # Check of dit overeenkomt.. Klopt.
#' exclude missing values
dat %>% filter(!is.na(sdC)) -> dat

dat %>% select(-c(semE, semC, used, nC_corrected)) -> dat # Remove unnessesary colums
which(is.na(dat))

head(is.na(dat)) 



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


#' **Calculation of effect sizes and checks**
dat <- escalc(m1i = meanE, sd1i = sdE, n1i = nE, 
              m2i = meanC, sd2i = sdC, n2i = nC, 
              measure = "SMD", method = "HE",  # calc hedge's G
              data = dat)

#' recode variables (effect sizes right direction)
#dat$yi <- ifelse(data$each %% 2 == 0, dat$yi * -1, dat$yi) ##for blinding
dat$yi <- dat$yi * dat$recode #give all effect sizes the correct direction  (higher score, better performance)


#' **INTERPRETATIE**
#' Viechtbauer, W. (2010). Conducting Meta-Analyses in R with the metafor Package. Journal of Statistical Software, 1–48. Page 7
#' "SMD": The standardized mean difference is equal to (m1i m2i)/spi
#' 1 = ptsd en 2 = control
#' DUS positive effect size is PTSD meer; Neg effect size is HC meer



#' **add unique id's**
dat$each <- c(1:nrow(dat)) 



#' **Explore frequencies of potential moderators** 
#' Potential moderators in dataset: (NB valence, phase, cuectx follow from task & measure in dataset (see code above)):
#' - comparisionControl: CODES: A="ptsd_nontrauma_H", B="trauma_nontrauma_H", C="ptsd_trauma_H", 
#' D="trauma_nontrauma_A", E="ptsd_nontrauma_A",F="ptsd_trauma_A"
#' - valence # NB codes: # T=trauma; N=non_trauma_neutral; E=non_trauma_emotional; F=non-trauma_fearfull
#' - phase: L=learning or M=memory (or E=extinction) (THINK:--> extinction droppen / indelen bij learing?)
#' - cuectx: cue or context

#' NB for the analysis 2 moderator possible.


# # Check frequenties per measure 
# sum_tasks <-dat %>%
#   group_by(type, phase, valence, comparisonControl, cuectx) %>%
#   summarise(length(unique(id)))
# 
# data.frame(sum_tasks)


#'*Comparision and subject as moderators?*
dat %>% group_by(subject,  comparisonControl) %>%
  summarize(papers=length(unique(id)), comparisons=length(each)) %>% data.frame() 
#' Not enough datapoints in some categories --> not possible

#' NB If comparisontypes is no longer included in analyses, comparision B needs to be excluded 
#' (in which healhty trauma exposed humans are compared to heathy non-trauma exposed humans; so NO PTSD patients)
dat %>% filter(comparisonControl != c("B")) %>% droplevels() -> dat


#' *Valence and subject as moderators?*
dat %>% group_by(subject, valence) %>%
summarize(papers=length(unique(id)), comparisons=length(each)) %>% data.frame() 
#' Problems with distribution valence categories in animal & human data, not al valence categories are in human & animal.
#' Decided to group stressful vs non-stressful learning
dat$Valence_Grouped <- ifelse(dat$valence %in% c("T","F","E"), "stress", "neutral") #NB nu trauma ook bij stressed!
dat$Valence_Grouped <-as.factor(dat$Valence_Grouped)
 dat %>% select(task_d, Valence_Grouped, valence) # to check. 

 #' NB trauma is niet helemaal een eerlijke categorie om toe te voegen aan 'stressed', omdat het nooit gemeten is humaan....
 dat <- dat %>% filter(valence != "T")%>% droplevels() ## Use as 'sensitivity check?'
 

#' *Phase and subject as moderators?*
# Explore distribution learning and memory data per subject & valence type
dat %>% 
  group_by(subject, Valence_Grouped, phase, cuectx) %>%
  summarize(papers=length(unique(id)), comparisons=length(each))
#' extinction data not in all groups --> Dropped for now.
dat<- dat %>% filter(phase != "E")%>% droplevels()


#' *cuectx and Valence as moderators, Animal & Human separate?*
dat %>% 
  group_by(subject, Valence_Grouped, cuectx) %>%
  summarize(papers=length(unique(id)), comparisons=length(each)) 


#' *Phase and Valence, in animal human separately?*
dat %>% 
  group_by(subject, Valence_Grouped, phase) %>%
  summarize(papers=length(unique(id)), comparisons=length(each)) 


dat %>% group_by(subject, phase, valence) %>%
  summarize(papers=length(unique(id)), comparisons=length(each)) %>% data.frame()

dat %>% filter(subject == "Animal", Valence_Grouped == "neutral", phase=="L")
dat %>% filter(subject == "Animal", Valence_Grouped == "neutral", phase=="M")

# Save resulting dataset --------------------------------------------------
dat <- dat %>% select(-c(id_combination)) %>% droplevels() #drop missing levels & Remove 'unique id combination' variable (not needed anymore)

#str(dat)

saveRDS(dat, file = "data.RData") #save

