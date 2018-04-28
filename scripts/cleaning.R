library(tidyverse)
library(readxl)

datapath <- './data/001212602.xls'

# readxl::excel_sheets(datapath)
# note that column b is actually a total of columns c:h

visitor_cols <- function(descriptions) {
    measurements <- c('観光入込客数（千人回）',
                      '観光消費額単価（円/人回）',
                      '観光消費額（百万円）') 
    visit_length <- c('宿泊', '日帰り')
    
    cols <- 
        cbind(rep(measurements, each = 4), 
              rep(descriptions, each = 2), 
              visit_length) %>% 
        as.data.frame() %>% 
        unite('columns', colnames(.), sep = '、')
    
    c('都道府県', cols[[1]])
}


define_cols <- function(sheet, description) {
    if (sheet %in% as.character(1:3)) {
        input = eval(parse(text = description))
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


tidy_data <- function(sheet, type = c('visitor_origins', 'visit_purposes', 'attractions')) {
    type = match.arg(type)
    if (type == 'attractions') {
        sheet %>% 
            separate(都道府県, c('index', '都道府県')) %>% 
            select(-観光地点, -index) %>% 
            gather(key = 'attraction_type', value = 'num_attractions', -都道府県)
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


clean_data <- function(sheet, type = c('visitor_origins', 'visit_purposes', 'attractions')){
    type = match.arg(type)
    if (type == 'attractions') {
        sheet %>% 
            mutate(num_attractions = ifelse(is.na(num_attractions), 0, num_attractions))
    }
    else {
        sheet %>% 
            mutate_at(vars(matches('（')),
                      .funs = as.numeric) %>% 
            mutate_at(vars(matches('（')),
                      .funs = function(x) ifelse(is.na(x), 0, x))
    }
}


visitor_origins <- c('県内', '県外')
visit_purposes <- c('観光目的', 'ビジネス目的')
ranges <- c(rep('A8:M54', 3), rep('A7:I53', 2))
sheets <- as.character(1:5)
descriptions <- c(rep('visitor_origins', 2), 
                  'visit_purposes', 
                  rep("attractions", 2))



#headers <- 
#    map2(.x = sheets,
#         .y = descriptions,
#         .f = define_cols)
#
#
#data <- 
#    pmap(.l = list(datapath, sheets, ranges, headers),
#         .f = read_xls)
#
#
#reshaped_data <- 
#    map2(.x = data, 
#         .y = descriptions, 
#         .f = tidy_data)
#
#
#cleaned_data <- 
#    map2(.x = reshaped_data,
#         .y = descriptions,
#         .f = clean_data)


cleaned_data <- 
    pmap(.l = list(datapath, 
                   sheets, 
                   ranges, 
                   map2(.x = sheets,
                        .y = descriptions,
                        .f = define_cols)),
         .f = read_xls) %>% 
    map2(.y = descriptions, 
         .f = tidy_data) %>% 
    map2(.y = descriptions,
         .f = clean_data)