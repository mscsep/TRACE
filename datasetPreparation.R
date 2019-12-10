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
which(is.na(method)) # check, should be no missing.

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
              
              "Data_Subjects_PTSDtypeSHORT2", 
              
              "MetaData_KeyFindingSHORT..PTSDmore..PTSDminder.0NoDiff.",
              
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
names(dat) <- c("id", "author", "year", "include", "subject", "task", "measure","comparisonControl", "ptsd", "keyfinding",  "idExp", "idControl",
                "nE", "meanE", "sdE", "semE", "nC", "meanC", "sdC", "semC")
# Create reference var as character
dat<-dat %>% mutate(reference = as.character(paste(author, year, sep=" "))) %>% select(-c(author, year)) # Merge year & author & dropvars.

# data$Data_Subjects_PTSDtypeSHORT2

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
# identical(ifelse(dat$subject %in% c(0,1,2,3), "Human", "Animal"),ifelse(dat$subject <= 3, "Human", "Animal")  ) # To check, gives same results


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
#' round
dat$nC<-round(dat$nC_corrected)


#' **Calculate SD from SEM**
dat$sdE <- ifelse(is.na(dat$sdE), (dat$semE * sqrt(dat$nE)), dat$sdE)
dat$sdC <- ifelse(is.na(dat$sdC), (dat$semC * sqrt(dat$nC)), dat$sdC)


#' **Check Missing values (should be none)**
#' Sample sizes ok
which(is.na(dat$nE))
which(is.na(dat$nC))
#' sd not..
which(is.na(dat$sdE))
which(is.na(dat$sdC))
#' Missing values SD? Original datafile checked: 5 papers don't report sem or sd: PMID: 7654154, 8731522, 9821567, 17392739, 27297027
unique(dat[which(is.na(dat$sdC)),"id"]) 
#' Theses values correspond with original datafile, and are exlcuded from further analysis
dat %>% filter(!is.na(sdC)) -> dat
dat %>% select(-c(semE, semC, used, nC_corrected)) -> dat # Remove unnessesary colums
#' Missingvalues are checked again, should be non..
which(is.na(dat)) 
#' update 19.3.19: na toevoeging "keyfinding", sommmige na, anders niet.
head(is.na(dat)) 


#' **Calculation of effect sizes and checks**
dat <- escalc(m1i = meanE, sd1i = sdE, n1i = nE, 
              m2i = meanC, sd2i = sdC, n2i = nC, 
              measure = "SMD", method = "HE",  # calc hedge's G
              data = dat)

#' **Recode Effect sizes in same direction (higher score = better performance)**
#' Based on recode variable, which is specified in "Trace_methode_codes.csv"
dat$yi <- dat$yi * dat$recode
#' INTERPRETATION: Viechtbauer, W. (2010). Conducting Meta-Analyses in R with the metafor Package. Journal of Statistical Software, 1–48. Page 7
#' "SMD": The standardized mean difference is equal to (m1i m2i)/spi
#' In the calulations above 1 = ptsd en 2 = control; therefore positive effect size = PTSD more, negative effectsize is HC more 


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


#'*Comparision and subject as moderators?*
dat %>% group_by(subject, valence, comparisonControl) %>%
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
#' NB Learning & memory of "Trauma" information are never measured in human (with behavioral tasks), therefor not an 'fair' comparison between animal & human..
#' Consider exclusion in 'analysis script'

 
#' *Phase and subject as moderators?*
# Explore distribution learning and memory data per subject & valence type
dat %>% 
  group_by(subject, Valence_Grouped, phase, cuectx) %>%
  summarize(papers=length(unique(id)), comparisons=length(each))
#' NB extinction data is not available for all groups. Besides it is an specific for of fear learning..
#' Consider exclusion in 'analysis script'


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

# dat %>% filter(subject == "Animal", Valence_Grouped == "neutral", phase=="L")
# dat %>% filter(subject == "Animal", Valence_Grouped == "neutral", phase=="M")



# compare keyfinding & effectsize -----------------------------------------
#' soort van quality check

# dat %>% filter(keyfinding %in% c("+", "-", "0")) %>% select(id, keyfinding, yi, phase, measureID, recode) -> qc
# + lijkt aardig te kloppen (keyfinding is niet zo secure ingevuld tijdens dataextractie.)
# Het is moeilijk om 0 te controleren.. (omdat het aan de ffect sizes niet te zien is of ze sig zijn.) 
# op een of andere manier is - niet in de data




# Save resulting dataset --------------------------------------------------
dat <- dat %>% droplevels() #drop missing levels & Remove 'unique id combination' variable (not needed anymore)

#str(dat)

saveRDS(dat, file = "data.RData") #save

