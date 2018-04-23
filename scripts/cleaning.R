library(tidyverse)
library(readxl)

datapath <- './data/001212602.xls'

raws <- 
    readxl::excel_sheets(datapath) %>% 
    cbind(datapath) %>% 
    as.data.frame() %>% 
    rename('sheet' = '.') %>% 
    mutate_if(.predicate = is.factor, .funs = as.character) %>% 
    mutate(sheets = purrr::map2(.x = datapath, .y = sheet, .f = read_xls))


# instead, let's read in subsets as ranges, assign columns, then gather the data

visitordata <- 
    c('1', '2', '3') %>% 
    cbind(datapath) %>% 
    as.data.frame() %>% 
    rename('sheet' = '.') %>% 
    mutate_if(.predicate = is.factor, .funs = as.character) %>% 
    mutate(data = pmap(.l = list(datapath, sheet, 'B8:M54', FALSE), 
                       .f = read_xls),
           headers = pmap(.l = list(datapath, sheet, 'B4:M7'), 
                          .f = read_xls))


eventdata <- 
    c('4', '5') %>% 
    cbind(datapath) %>% 
    as.data.frame() %>% 
    rename('sheet' = '.') %>% 
    mutate_if(.predicate = is.factor, .funs = as.character) %>% 
    mutate(data = purrr::pmap(.l = list(datapath, sheet, 'B7:I53', FALSE), 
                              .f = read_xls),
           headers = purrr::pmap(.l = list(datapath, sheet, 'B4:I6'), 
                                 .f = read_xls))

rows <- read_xls(datapath, '1', 'A8:A54', c('都道府県'))


chunks <- split(1:12, ceiling(seq_along(1:12)/4))

extract_headers <- function(section){
    measure <- section[[1, 1]]
    location <- c(rep(section[[2, 1]], 2), rep(section[[2, 3]], 2))
    combinations <- 
        rep(measure, 4) %>% 
        cbind(location) %>% 
        cbind(as.character(section[3,])) %>% 
        as.data.frame() %>% 
        `colnames<-`(c('measure', 'location', 'type')) %>% 
        mutate(output = paste0(measure, ', ', location, ', ', type))
    
    combinations$output
}

testheaders <- map2(rep(chunks, 3), rep(1:3, each=3), 
                    ~ extract_headers(visitordata$headers[.y][[1]][, .x]))


values <- c('senninkai', 'rate', 'hyakumanen')

columns_ab <- c('kennai - jukuhaku', 'kennai - higaeri', 
             'kengai - jukuhaku', 'kengai - higaeri')

columns_c <- c('kankou - jukuhaku', 'kankou - higaeri', 
             'bijinesu - jukuhaku', 'bijinesu - higaeri')

columns_de <- c()



visitordata$data[1][[1]] %>% select(X__1, X__2) %>% gather(key = 'staytype', value = 'visitors')


# for each header in visitordata$headers, for each chunk in header, extract_headers
# it should output a 12-element vector for each header
# then, each dataset in visitordata$data should have colnames(data) <- respective header vector

# the final output of the first three tabs should be a six-column dataframe:
# year, prefecture, in/out prefecture, type of visit, visitors, spend/visitor, spend

# the final output of the last two tabs should be a 
# year, prefecture, type of location, count
# note that column b is actually a total of columns c:h