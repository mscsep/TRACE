# script to merge pubmed information with screened data

readxl::read_xlsx("TRACE_screening_search3_MS.xlsx", sheet="abstract.screening.s3.human")->screen.human
readxl::read_xlsx("TRACE_screening_search3_MS.xlsx", sheet="abstract.screening.s3.animal")->screen.animal
readxl::read_xlsx("pmid.info.hs3.xlsx")->info.human
readxl::read_xlsx("pmid.info.as3.xlsx")->info.animal

# there is one more hit in animal screen, same paper twice.. wrong pmid
which(!screen.animal$Reference_PMID %in% info.animal$PMID)
screen.animal[30,] # same paper
which(!screen.human$Reference_PMID %in% info.human$PMID)

# rename
rename(screen.animal, PMID=Reference_PMID)->screen.animal
rename(screen.human, PMID=Reference_PMID)->screen.human

library(dplyr)
full_join(info.animal, screen.animal, by= "PMID")->animal.joined
full_join(info.human, screen.human, by= "PMID")->human.joined

#export
write.csv2(animal.joined,"animal.abstract.screening.s3.joined.csv")
write.csv2(human.joined,"human.abstract.screening.s3.joined.csv")
