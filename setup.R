
# In this section we will install the required packages, set the
# parallel plan and progress bar




#_______________________INSTALL_ONLY_MISSING_PACKAGES___________________________

packages <- c("jsonlite", "dplyr", "purrr", "data.table", "tidyquant", 
              "lubridate", "httr", "hms", "ggplot2", "future.apply", "stringr",
              "data.table", "here", "transport", "future", "progressr",
              "future", "rvest", "tidyr", "tibble", "MASS", "glmnet")

to_install <- packages[!(packages %in% installed.packages()[, "Package"])]
if (length(to_install) > 0) install.packages(to_install)

invisible(lapply(packages, library, character.only = TRUE))



#___________________________DEFINE_PARALLEL_PLAN________________________________

future::plan(future::multisession,
             workers = max(parallel::detectCores() - 1, 1))



#___________________________DEFINE_PROGRESS_BAR_________________________________

progressr::handlers(global = TRUE)
progressr::handlers("txtprogressbar")