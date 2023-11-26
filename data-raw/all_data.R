# path<- "S:/data/databased/"
#
# library(templaforms)
# library(tidyverse)
# library(gplyr)
# library(devtools)
#
# file_tibble(path) %>%
#   pull(path) %>%
#   walk(load, envir = parent.frame(2))
#
# remove(path)
#
#
# walk(mget(ls(globalenv()), envir = globalenv()),
#      use_data,
#      overwrite = TRUE)
#
# usethis::use_data(all_data, overwrite = TRUE)
#
#

