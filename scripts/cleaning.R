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


testheaders <- map2(rep(chunks, 3), rep(1:3, each=3), 
                    ~ extract_headers(visitordata$headers[.y][[1]][, .x]))

visitordata$data[1][[1]] %>% select(X__1, X__2) %>% gather(key = 'staytype', value = 'visitors')


# for each header in visitordata$headers, for each chunk in header, extract_headers
# it should output a 12-element vector for each header
# then, each dataset in visitordata$data should have colnames(data) <- respective header vector

# the final output of the first three tabs should be a six-column dataframe:
# year, prefecture, in/out prefecture, type of visit, visitors, spend/visitor, spend

# the final output of the last two tabs should be a 
# year, prefecture, type of location, count
# note that column b is actually a total of columns c:h



visitor_cols <- function(descriptions){
    measurements <- c('観光入込客数（千人回）',
                      '観光消費額単価（円/人回）',
                      '観光消費額（百万円）') 
    visitor_origins <- c('県内', '県外')
    visit_purposes <- c('観光目的', 'ビジネス目的')
    visit_length <- c('宿泊', '日帰り')
    
    cols <- 
        cbind(rep(measurements, each = 4), 
              rep(descriptions, each = 2), 
              visit_length) %>% 
        as.data.frame() %>% 
        unite('columns', colnames(.), sep='、')
    
    c('都道府県', cols[[1]])
}


define_cols <- function(sheet, description){
    if (sheet %in% as.character(1:3)) {
        input = eval(parse(text=description))
        enc2utf8(visitor_cols(input))
    }
    else {
        attractions_cols <- c('都道府県',
                              '観光地点',
                              '自然',
                              '歴史・文化',
                              '温泉・健康',
                              'ｽﾎﾟｰﾂ・ ﾚｸﾘｴｰｼｮﾝ',
                              '都市型観光',
                              'その他',
                              '行祭事・ イベント')
        
        enc2utf8(attractions_cols)
    }
}


tidy_data <- function(sheet, type = c('visitor_origins', 'visit_purposes', 'attractions')){
    type = match.arg(type)
    if (type == 'attractions') {
        sheet %>% 
            separate(都道府県, c('index', '都道府県')) %>% 
            select(-観光地点, -index) %>% 
            gather(key = 'attraction_type', value = 'count', -都道府県)
    }
    else {
        sheet %>% 
            separate(都道府県, c('index', '都道府県')) %>% 
            select(-index) %>% 
            gather(key = 'key', value = 'measures', -都道府県) %>% 
            separate(key, 
                     c('measure_type', 'visitor_description', 'visit_length'), 
                     enc2utf8('、')) %>% 
            spread(measure_type, measures)
    }
}


ranges <- c(rep('A8:M54', 3), rep('A7:I53', 2))
sheets <- as.character(1:5)
descriptions = c(rep('visitor_origins', 2), 
                 'visit_purposes', 
                 rep("attractions", 2))



headers <- 
    map2(.x = sheets,
         .y = descriptions,
         .f = define_cols)


data <- 
    pmap(.l = list(datapath, sheets, ranges, headers),
         .f = read_xls)


clean_data <- map2(.x = data, 
                   .y = descriptions, 
                   .f = tidy_data)
