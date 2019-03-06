# Explore dataset

rm(list = ls()) 
library(dplyr)


# Explore measures to check & create 'factor' file ------------------------
#import full dataset 
data <- read.csv("TRACE_Dataset.csv", sep = ";", na.strings = c(" ", "-"), dec = c(",", "."))

methods<-select(data,
                Reference_PMID,
                inclusion,
                subject,
                valence,
                Data_Subjects_PTSDtypeSHORT2,
                Data_Method_TaskSHORT,
                Data_Method_MeasureSHORT,
                MetaData_CueContext.,
                MetaData_Learning.MemoryPhase,
                recode
)
names(methods) <- c("id","include", "subject", "valence", "ptsd", "task", "measure", "cuectx","phase","recode" )

methods <- methods %>% filter(include == 1) %>% droplevels()
methods$subject <- ifelse(methods$subject <= 3, "Human", "Animal")
str(methods)

# Check frequenties per measure 
sum_tasks<-methods %>%
  group_by(task, measure, phase, valence,recode, cuectx) %>%
  summarise(length(unique(id)))

data.frame(sum_tasks)


write.table(sum_tasks,"factors.txt", sep="\t", row.names = F) # --> save file to inspect, correct errors & create facors.






# Find possible moderators for analyses in dataset -------------------------
# Import prepared data for analyses.
load("data.RData")
str(dat)

## SOME INFO: Potential moderators in dataset: (NB valence, phase, cuectx follow from task & measure in dataset (see code above)):
# - comparisionControl: CODES: A="ptsd_nontrauma_H", B="trauma_nontrauma_H", C="ptsd_trauma_H", 
                            # D="trauma_nontrauma_A", E="ptsd_nontrauma_A",F="ptsd_trauma_A"
# - valence # NB codes: # 1=trauma; 2=non_trauma_neutral; 3=non_trauma_emotional; 4=non-trauma_fearfull
# - phase: L=learning or M=memory (or E=extinction) (THINK:--> extinction droppen / indelen bij learing?)
# - cuectx: cue or context (nb recode below neded, adjust later in factorfile)
                  # Recode wrong cuectx codes
                  data.frame(unique(dat$cuectx))
                  dat$cuectx<-  ifelse(dat$cuectx %in% c("Cue (in context) -", "Cue (in context) +", "Cue & Context", "Cue", "cue"), 
                                       "cue", "context")
                  
# add unique id's
 dat$each <- c(1:nrow(dat)) 



# Comparison types --------------------------------------------------------

 ## Plan was to compare PTSD vs Trauma-exposure (per subject type & valence type) --> not possible, due to insufficient datapoints in some comparisiontypes
 # dat$comparison_control_grouped <- ifelse(dat$comparison_control %in% c("B", "D"), "trauma", "ptsd")
 #dat %>% filter(comparison_control == c("A", "D")) ->  dat2#use E as a sensitivity/qualitative description
 # data %>%
 #   filter(!comparison_control %in% c("C", "F")) %>%
 #   droplevels() -> dat
  ## check frequencies
  dat %>% 
   group_by(subject, valence,  comparisonControl) %>%
   summarize(papers=length(unique(id)), comparisons=length(each)) %>% data.frame()
 
# If comparisontypes is no longer included in analyses 
  # --> drop comparision B (in which healhty trauma exposed humans are compared to heathy non-trauma exposed humans; so NO PTSD patients)
  dat %>% filter(comparisonControl != c("B")) -> dat1
  

# Valence -----------------------------------------------------------------
  
# Also problems with distribution valence categories in animal & human data --> decided to group stressful vs non-stressful learning
  # Problem with "mods   = ~subject:valence - 1" in model --> not al valence categories are in human & animal..
  # naast valence ook een verschil spatial/verbal/visual/ memory maken voor neutral?
  
# Check frequencies
dat1 %>% 
  group_by(subject, valence) %>%
  summarize(papers=length(unique(id)), comparisons=length(each))


# Group valence. # NB codes: # 1=trauma; 2=non_trauma_neutral; 3=non_trauma_emotional; 4=non-trauma_fearfull
dat1$Valence_Grouped <- ifelse(dat1$valence %in% c(1,3,4), "stress", "neutral") #NB nu trauma ook bij stressed!
dat1$Valence_Grouped <-as.factor(dat1$Valence_Grouped)

str(dat1)

# NB trauma is niet helemaal een eerlijke categorie om toe te voegen aan 'stressed', omdat het nooit gemeten is humaan....
dat2 <- dat1 %>% filter(valence != "1") ## Use as 'sensitivity check?'


# Learning Memory ---------------------------------------------------------

# Explore distribution learning and memory data per subject & valence type
dat1 %>% 
  group_by(subject, Valence_Grouped, phase, cuectx) %>%
  summarize(papers=length(unique(id)), comparisons=length(each))

# extinction data not in all groups # Dropped for now.
dat1<- dat1 %>% filter(phase != "E")


# Split human animal in seperate analyses? --------------------------------

# Check:

# Possible to invstigate valence * cuectx in animal human separately?
dat1 %>% 
  group_by(subject, Valence_Grouped, cuectx) %>%
  summarize(papers=length(unique(id)), comparisons=length(each)) 
# --> Combination valence cuectx (split over human/animal), in some groups 1 paper..


# Possible to invstigate valence * phase in animal human separately?
dat1 %>% 
  group_by(subject, Valence_Grouped, phase) %>%
  summarize(papers=length(unique(id)), comparisons=length(each)) 
## Animal: only 5 learning papers, with 23 comparisons, not enough? 
## Human 5 papers for learning & memory stressed..

# for now...  split learning / memory not ideal...?


# # Code to split:
# dat1 %>% filter(subject == "Animal") %>% droplevels() -> dat.a
# dat1 %>% filter(subject == "Human") %>% droplevels() -> dat.h
# 
# str(dat.a)
# str(dat.h)


# Save datasets -----------------------------------------------------------

saveRDS(list(dat1, dat2), "data.analysis.RData")


