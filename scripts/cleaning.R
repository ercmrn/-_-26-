library(tidyverse)
library(readxl)

datapath <- './data/001212602.xls'

(sheets <- 
        readxl::excel_sheets(datapath) %>% 
        cbind(datapath) %>% 
        as.data.frame() %>% 
        rename('sheet' = '.') %>% 
        mutate_if(.predicate = is.factor, .funs = as.character))

raws <- 
    sheets %>% 
    mutate(files = purrr::map2(.x = datapath, .y = sheet, .f = read_xls))

raws[3][[1]][[2]] %>% View()
