# used a single example to prototype the data reshaping and cleaning process
# looking at other examples, it holds up pretty well, but quarterly data
# has two extra sheets per file.

# TODO: increase functionality to handle sheets 6:7 on quarterly data
# TODO: functionalized cleaning, applied to all of the sheets
# TODO: combine cleaned data with metadata, somehow (perhaps as a list of dfs + mutate?)


library(tidyverse)
library(readxl)

#datapath <- './data/001212602.xls'
datapath <- './data/001127121.xls'


quarterly <- 
    excel_sheets(datapath) %>% 
    map2(.x = datapath, .y = ., read_xls)

# maybe change the column naming strategy to a list of column name vectors

# page 1 is kankou - nihonjin
# page 2 is business - nihonjin
# page 3 is kankou / business - gaikokujin
# page 4 is # of locations/events
# page 5 is # of visitors for locations/events
# page 6 is # of parameter locations?
# page 7 is # of sampled visitors to parameter locations?





# readxl::excel_sheets(datapath)
# note that column b is actually a total of columns c:h
# further, appears that Okinawa and Hokkaido might have some separately collected data

visitor_cols <- function(sheet) {
    visit_length <- c('宿泊', '日帰り')
    measurements <- c('観光入込客数（千人回）',
                      '観光消費額単価（円/人回）',
                      '観光消費額（百万円）') 
    
    if (sheet == '1') {
        cols <- 
            cbind(rep(c('県内', '県外'), each = 2), 
                  '観光目的',
                  visit_length,
                  rep(measurements, each = 4)) %>% 
            as.data.frame() %>% 
            unite('columns', colnames(.), sep = '、')
    }
        
    else if (sheet == '2') {
        cols <- 
            cbind(rep(c('県内', '県外'), each = 2), 
                  'ビジネス目的',
                  visit_length,
                  rep(measurements, each = 4)) %>% 
            as.data.frame() %>% 
            unite('columns', colnames(.), sep = '、')
    }

    else (sheet == '3') {
        cols <- 
            cbind('訪日外国人', 
                  rep(c('観光目的', 'ビジネス目的'), each = 2),
                  visit_length,
                  rep(measurements, each = 4)) %>% 
            as.data.frame() %>% 
            unite('columns', colnames(.), sep = '、')
    }
    
    c('都道府県', cols[[1]])
}


define_cols <- function(sheet) {

    if (sheet %in% as.character(1:3)) {
        cols <- visitor_cols(sheet)
    }
    
    else if (sheet %in% as.character(4:5)) {
        cols <- c('都道府県', '観光地点', '自然', '歴史・文化', '温泉・健康', 
                  'ｽﾎﾟｰﾂ・ ﾚｸﾘｴｰｼｮﾝ','都市型観光','その他','行祭事・ イベント')
    }
    
    else if (sheet == '6') {
        cols <- c('都道府県', 'パラメータ地点総数', '自然', '歴史・文化',
                  '温泉・健康', 'ｽﾎﾟｰﾂ・ ﾚｸﾘｴｰｼｮﾝ', '都市型観光', 'その他')
    }
    
    else if (sheet == '7') {
        cols <- c('都道府県', 'サンプル数（人）', '平均同行者数（人）', 
                  '1人当たり 平均訪問地点数', '1人当たり 平均消費額（円）',
                  '1人当たり平均訪問 都道府県数')
    }
    else {
        cols <- ''
    }
    
    enc2utf8(cols)
    
}


tidy_data <- function(sheet, index = as.character(1:7)) {
    index = match.arg(index)
    
    if (index %in% as.character(1:3)) {
        sheet %>% 
            separate(都道府県, c('index', '都道府県')) %>% 
            select(-index) %>% 
            gather(key = 'key', value = 'values', -都道府県) %>% 
            separate(key, 
                     c('visitor_description', 'visit_purpose', 
                       'visit_length', 'value_type'), 
                     enc2utf8('、')) %>% 
            spread(value_type, values)
    }
    else if (index == '4') {
        sheet %>% 
            separate(都道府県, c('index', '都道府県')) %>% 
            select(-観光地点, -index) %>% 
            gather(key = 'attraction_type', value = 'attractions', -都道府県)
    }
    else if (index == '5') {
        sheet %>% 
            separate(都道府県, c('index', '都道府県')) %>% 
            select(-観光地点, -index) %>% 
            gather(key = 'attraction_type', value = '1K_visitors', -都道府県)
    }
    else if (index == '6') {
        sheet %>% 
            separate(都道府県, c('index', '都道府県')) %>% 
            select(-パラメータ地点総数, -index) %>% 
            gather(key = 'attraction_type', value = 'attractions', -都道府県)
    }
    else if (index == '7') {
        sheet %>% 
            separate(都道府県, c('index', '都道府県')) %>% 
            select(-index)
    }
}


clean_data <- function(sheet, index = as.character(1:7)) {
    index = match.arg(index)
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


#visitor_origins <- c('県内', '県外')
#visit_purposes <- c('観光目的', 'ビジネス目的')
#sheets <- as.character(1:7)


#descriptions <- c(rep('visitor_origins', 2), 
#                  'visit_purposes', 
#                  rep("attractions", 2))


#ranges <- c(rep('A8:M54', 3), rep('A7:I53', 2))

# read in everything, then find the range by hokkaido - okinawa

# find the headers. This really sucks, though.
othercols <- function(sheet, headerrange) {
    map(sheet[headerrange[1]:headerrange[2],], function(x) x[!is.na(x)]) %>% 
        map(function(x) gsub('\n', '', x)) %>% 
        as.character()
}

findranges <- function(sheet) {
    c(match('都道府県', sheet[[1]]), 
      match('01 北海道', sheet[[1]]) - 1,
      match('01 北海道', sheet[[1]]),
      match('47 沖縄県', sheet[[1]]))
}

#findsheetranges <- function(sheet) {
#    ranges <- c()
#    
#    ranges[[1]] <- paste0('A', match('都道府県', sheet[[1]]), ':',
#                          'A', match('01 北海道', sheet[[1]]))
#    
#    ranges[[2]] <- paste0('A', match('01 北海道', sheet[[1]]), ':',
#                          LETTERS[ncol(sheet)], match('47 沖縄県', sheet[[1]]))
#    
#    ranges
#}



sheets <- excel_sheets(datapath)

cleaned_data <- 
    pmap(.l = list(datapath, 
                   sheets, 
                   ranges, 
                   map(.x = sheets,
                       .f = define_cols)),
         .f = read_xls) %>% 
    map2(.y = sheets, 
         .f = tidy_data) %>% 
    map2(.y = sheets,
         .f = clean_data)



