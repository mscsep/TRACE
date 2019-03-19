# Check influential points
# Based on analysis code valeria!



influentials_valence_phase <- function(dataset){
# Quality check Preclinical, Neutral learning lijkt heel extreem. Data in paper opgenieuw bekeken. staat correct in dataset.

hist(dataset$yi, breaks = 50) 

  #identification influential cases following Viechtbauer & Cheung(2010)'s definition: "the studentized deleted residual > 1.96"
  #available in rma.uni model of metafor package
univarMOD <- rma.uni(yi, vi,
                     #random = list(~1 | each, ~1 | idExp),
                     mods   = ~phase:Valence_Grouped - 1,  # NB only interaction term
                     method = "REML",
                     data = dataset) # Similar effects with dat2 (trauma learning excluded)
summary(univarMOD)

dataset$potInf <- 0
dataset$potOut <- 0 

y<- rstudent(univarMOD)
inf<- influence(univarMOD)

dataset[which(inf$is.infl),]$potInf <- 1 # Influential cases
dataset[which(abs(y$z) > 1.96),]$potOut <- 1 # Values that are potential outliers

dataset %>% 
  group_by(potInf) %>% 
  summarize(length(each), 
            length(unique(idExp)),
            length(unique(id)))

dataset %>% 
  group_by(potOut) %>% 
  summarize(length(each), 
            length(unique(idExp)),
            length(unique(id)))


# Cases that are potential outliers and influential
dataset$outInf <- ifelse(dataset$potOut == 1 & dataset$potInf == 1, 1, 0) 

dataset %>% 
  group_by(outInf) %>% 
  summarize(length(each), 
            length(unique(idExp)),
            length(unique(id)))


return(dataset)

}


