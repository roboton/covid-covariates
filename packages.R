## library() calls go here
library(conflicted)
library(dotenv)
library(drake)
library(rmarkdown)

# Prediction
library(SuperLearner)
library(BoomSpikeSlab)
library(tidymodels)
library(ranger)
library(pdp)

# Time series 
library(zoo)
library(EnvCpt)
library(CausalImpact)

# Read Census FIPS xlsx
library(httr)
library(readxl)

# Presentation
library(plotly)
library(DT)

# Data munging
library(countrycode)
library(jsonlite)
library(lubridate)
library(tidyverse)

# prioritizing namespaces
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("first", "dplyr")
conflict_prefer("last", "dplyr")
conflict_prefer("gather", "tidyr")
conflict_prefer("discard", "purrr")
conflict_prefer("partial", "pdp")
conflict_prefer("lag", "dplyr")
