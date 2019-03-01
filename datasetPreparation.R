# Environment preparation -------------------------------------------------
rm(list = ls()) #clean environment

#Libraries
library(metafor) #for effect size estimation
library(dplyr) #for general functions
library(ggplot2) #for function cutnumber


#import full dataset 
data <- read.csv("TRACE_v1_3_19.csv", sep = ";", na.strings = c(" ", "-"), dec = c(",", "."))


# Select relevant parts of dataset ----------------------------------------
##prepare your dataset
var <- c("Reference_PMID",
         "MetaData_FINALinclORexcl..0..checked.excluded..1...included.MemoryFC...2...other_Domein.",
         "Data_Subjects_background..0.humanmixed.Civiel.Military...1.ActiveDutyMilitary..2.Veteran..3.Civilian..4.Rat..5.Mice.",
         "MetaData_MemoryValence.1.trauma..2.non_trauma_neutral..3.non_trauma_emotional..4.non.trauma_fearfull.",
         "OmpoolFactor_zodatHogereScoresISBeterePerformance",
         "Comparison",
         "ID_Experimental.group",
         "ID_Control.group",
         "Data_Subjects_n_ptsd",
         "Data_Outcome1_M",
         "Data_Outcome1_SD",
         "Data_Outcome1_SEM",
         "Data_Subjects_n_control",
         "Data_Outcome2_M",
         "Data_Outcome2_SD",
         "Data_Outcome2_SEM")

dat <- data[, var]

names(dat) <- c("id", "include", "subject", "valence", "recode", "comparison_control", "id_exp", "id_control",
                "n_e", "mean_e", "sd_e", "sem_e", "n_c", "mean_c", "sd_c", "sem_c")

dat <- dat %>% filter(include == 1) %>% droplevels()

# Recode animals
dat$subject <- ifelse(dat$subject <= 3, "Human", "Animal")

# check missing of all variables of interst
which(is.na(dat$subject))
which(is.na(dat$id))
which(is.na(dat$comparison_control))


# change stats from factors to numbers
stat.vars <- c("n_e", "mean_e", "sd_e", "sem_e", "n_c", "mean_c", "sd_c", "sem_c")

str(dat)

dat[,c("n_e")]<- as.numeric(sub(",", ".", as.character(dat[,"n_e"]), fixed = TRUE))
dat[,c("n_c")]<- as.numeric(sub(",", ".", as.character(dat[,"n_c"]), fixed = TRUE))

dat[,c("mean_c")]<- sub("-", "-", as.character(dat[,"mean_c"]), fixed = TRUE) # Error -
dat[,c("mean_c")]<- sub("%", "", dat[,"mean_c"], fixed = TRUE)
dat[,c("mean_c")]<- as.numeric(sub(",", ".", dat[,"mean_c"], fixed = TRUE)) # Error -

dat[,c("mean_e")]<- sub("-", "-", as.character(dat[,"mean_e"]), fixed = TRUE) # Error -
dat[,c("mean_e")]<- sub("%", "", dat[,"mean_e"], fixed = TRUE)
dat[,c("mean_e")]<- as.numeric(sub(",", ".", dat[,"mean_e"], fixed = TRUE)) # Error -


dat[,c("sd_c")]<- as.numeric(sub(",", ".", as.character(dat[,"sd_c"]), fixed = TRUE))
dat[,c("sd_e")]<- as.numeric(sub(",", ".", as.character(dat[,"sd_e"]), fixed = TRUE))

dat[,c("sem_c")]<- as.numeric(sub(",", ".", as.character(dat[,"sem_c"]), fixed = TRUE))
dat[,c("sem_e")]<- as.numeric(sub(",", ".", as.character(dat[,"sem_e"]), fixed = TRUE))





#factor.vars<-c("id", "include", "subject", "valence", "recode", "comparison_control", "id_exp", "id_control")
#dat[,factor.vars] = lapply(dat[factor.vars], as.factor)

#which(is.na(dat$sd_e))
str(dat)


# Calculate sd from sem
dat$sd_e <- ifelse(is.na(dat$sd_e), (dat$sem_e * sqrt(dat$n_e)), dat$sd_e)
dat$sd_c <- ifelse(is.na(dat$sd_c), (dat$sem_c * sqrt(dat$n_c)), dat$sd_c)

# Check Missing values
which(is.na(dat$sd_e))
which(is.na(dat$n_e))
which(is.na(dat$sd_c)) # erblijven missing na's over...

# Missing values SD? Original datafile checked: 5 papers don't report sem or sd: PMID: 7654154, 8731522, 9821567, 17392739, 27297027
unique(dat[which(is.na(dat$sd_c)),"id"]) # Check of dit overeenkomt.. Klopt.

# exclude missing values
dat <- dat[-which(is.na(dat$sd_c)),]



# Check frequenties -------------------------------------------------------
summarise(dat)
summarise(dat, subject, valence, comparison_control, group_by(id))
group_by(dat, valence)
         
         
# Corrections to statistical measurements -------------------------------------------------------------
#N correction
data$nC <- ifelse(!is.na(data$cut_nC), data$nC/2, data$nC) ##cut N.C in half if same control used by two experimental groups

##papers in which N not reported >> mean of other papers
data$nC[is.na(data$nC)] <- round(mean(data$nC, na.rm = TRUE))
data$nE[is.na(data$nE)] <- round(mean(data$nE, na.rm = TRUE))

#calculate SD for all comparisons -> necessary for calculation effect size
data$sdC <- ifelse(!is.na(data$effectSizeCorrection), data$seC/6, #correction for IQ range
                   ifelse(is.na(data$sdC), (data$seC * sqrt(data$nC)), data$sdC)) #transform se in sd

data$sdE <- ifelse(!is.na(data$effectSizeCorrection), data$seE/6, #correction for IQ range
                   ifelse(is.na(data$sdE), (data$seE * sqrt(data$nE)), data$sdE)) #transform se in sd

data$seC <- ifelse(!is.na(data$effectSizeCorrection), (data$sdC / sqrt(data$nC)), data$seC) #change IQR to se
data$seC <- ifelse(is.na(data$seC), (data$sdC / sqrt(data$nC)), data$seC) #compute missing se from sd
data$seE <- ifelse(!is.na(data$effectSizeCorrection), (data$sdE / sqrt(data$nE)), data$seE) #change IQR to se
data$seE <- ifelse(is.na(data$seE), (data$sdE / sqrt(data$nE)), data$seE) #compute missing se from sd


##correction for qualitative interpretation direction (for systematic review graphs)
data$directionQual <- (as.numeric(factor(data$directionGrouped, 
                                         levels = c("decrease", "ns", "increase"))) - 2) #convert direction reported by studies to numeric

data$directionQual <- data$directionQual * data$multiply #correct direction of effects reported by studies according to categorization rules

data$directionGrouped <- ifelse(is.na(data$directionQual), "notRetrievable",
                                ifelse(data$directionQual == -1, "decrease",
                                       ifelse(data$directionQual == 1, "increase", "ns"))) #convert numeric to interpretation

#convert decrease with increase and viceversa for nsLearning and social
data$directionGrouped <- ifelse(data$domain %in% c("nsLearning", "social") & data$directionGrouped == "decrease", 
                                "increase",
                                ifelse(data$domain %in% c("nsLearning", "social") & data$directionGrouped == "increase", 
                                       "decrease", data$directionGrouped))

data$directionGrouped <- as.factor(data$directionGrouped)


# Calculation effect size and checks --------------------------------------
##calculate effect size
dat <- escalc(m1i = mean_e, sd1i = sd_e, n1i = n_e, 
               m2i = mean_c, sd2i = sd_c, n2i = n_c, 
               measure = "SMD", method = "HE",
               data = dat)

#dat$yi <- ifelse(data$each %% 2 == 0, dat$yi * -1, dat$yi) ##for blinding

dat$yi <- dat$yi * dat$recode #give all effect sizes the correct direction



# Save resulting dataset --------------------------------------------------

data <- data %>% droplevels() #drop missing levels

save(dat, file = "data.RData") #save


