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
TRACE<- osf_retrieve_file("zwbxg")
# Open dataset with browser
osf_open(TRACE) 

# Download local copy of OSF data.
osf_download(TRACE) 

# Load file in envirionment.
library(readr)

TRACE_dataset <- read_delim("TRACE_dataset.csv", ";", escape_double = FALSE, trim_ws = TRUE)