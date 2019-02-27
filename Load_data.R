# Load data from OSF

# READ file from OSF
# https://rdrr.io/github/SachaEpskamp/OSF2R/man/readOSF.html


library(rJava)
#install.packages("devtools")
library(devtools)
#install_github("SachaEpskamp/OSF2R") # error heeft iets met rJava te maken.. WERKT NIET


# https://centerforopenscience.github.io/osfr/
# install.packages("remotes")
remotes::install_github("centerforopenscience/osfr") # werkt wel

library(osfr)

# authentication OSF private repositories.
# http://centerforopenscience.github.io/osfr/articles/auth.html

# osf_auth() # Paste in console met PAT


# Retrieve OSF file(s)
TRACE<- osf_retrieve_file("9uryb")

library 




