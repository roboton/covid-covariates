## library() calls go here
library(conflicted)
library(dotenv)
library(drake)
library(rmarkdown)

# added
library(zoo)
library(EnvCpt)
library(CausalImpact)
library(httr)
library(readxl)
library(plotly)
library(DT)
library(rvest)
library(jsonlite)

# always last
library(tidyverse)
library(lubridate)

# prioritizing namespaces
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("first", "dplyr")
conflict_prefer("last", "dplyr")
conflict_prefer("gather", "tidyr")
