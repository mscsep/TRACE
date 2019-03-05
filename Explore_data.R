# Explore dataset

rm(list = ls()) 
library(dplyr)


#import full dataset 
data <- read.csv("TRACE_Dataset.csv", sep = ";", na.strings = c(" ", "-"), dec = c(",", "."))


# Explore dataset ---------------------------------------------------------

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




