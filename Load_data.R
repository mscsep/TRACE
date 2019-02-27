# Load data from OSF

# READ file from OSF
# https://centerforopenscience.github.io/osfr/
# OSFR package https://www.rdocumentation.org/packages/osfr/versions/0.1.0.9000

# install.packages("remotes")
# remotes::install_github("centerforopenscience/osfr") # werkt wel
library(osfr)

# authentication OSF private repositories.
# http://centerforopenscience.github.io/osfr/articles/auth.html
# Paste in console met PAT (handmatig toevoegen)
 osf_auth()

# Retrieve OSF file(s)
TRACE<- osf_retrieve_file("9uryb")
# Open dataset with browser
osf_open(TRACE) 

# Download local copy of OSF data.
osf_download(TRACE) 

# Load file in envirionment.
Trace_OSF_Test <- read_delim("Trace_OSF_Test.csv", ";", escape_double = FALSE, trim_ws = TRUE)




