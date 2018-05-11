# used a single example to prototype the data reshaping and cleaning process
# looking at other examples, it holds up pretty well, but quarterly data
# has two extra sheets per file.

# TODO: combine cleaned data with metadata, somehow (perhaps as a list of dfs + mutate?)
# TODO: refactor to run loop over one file, adding extracted data to a set of
# output dfs

library(tidyverse)
library(readxl)
library(data.table)

# page 1 is kankou - nihonjin
# page 2 is business - nihonjin
# page 3 is kankou / business - gaikokujin
# page 4 is # of locations/events
# page 5 is # of visitors for locations/events
# page 6 is # of parameter locations?
# page 7 is # of sampled visitors to parameter locations?


visitor_cols <- function(sheetname) {
    
    # TODO: fill the columns with a different approach?
    
    visit_length <- c('宿泊', '日帰り')
    measurements <- c('観光入込客数（千人回）',
                      '観光消費額単価（円/人回）',
                      '観光消費額（百万円）')
    visitor_type_a <- c('県内', '県外')
    visitor_type_b <- '訪日外国人'
    visitor_purpose_a <- '観光目的'
    visitor_purpose_b <- 'ビジネス目的'
    
    combine_keys <- function(visitor_type, visitor_purpose) {
        cbind(rep(c('県内', '県外'), each = 2), 
              '観光目的',
              visit_length,
              rep(measurements, each = 4)) %>% 
            as.data.frame() %>% 
            unite('columns', colnames(.), sep = '、')
    }
    
    if (sheetname == '1') {
        cols <- 
            cbind(rep(c('県内', '県外'), each = 2), 
                  '観光目的',
                  visit_length,
                  rep(measurements, each = 4)) %>% 
            as.data.frame() %>% 
            unite('columns', colnames(.), sep = '、')
    }
        
    else if (sheetname == '2') {
        cols <- 
            cbind(rep(c('県内', '県外'), each = 2), 
                  'ビジネス目的',
                  visit_length,
                  rep(measurements, each = 4)) %>% 
            as.data.frame() %>% 
            unite('columns', colnames(.), sep = '、')
    }

    else {
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


othercols <- function(sheet) {
    # TODO: refactor to accept/find dynamic ranges?
    
    headerrange <- c(match('都道府県', sheet[[1]]), 
                     match('01 北海道', sheet[[1]]) - 1)
    
    map(sheet[headerrange[1]:headerrange[2],], function(x) x[!is.na(x)]) %>% 
        map(function(x) gsub('\n', '', x)) %>% 
        as.character() %>% 
        "["(. != "character(0)") # I can't believe this works
}


define_cols <- function(sheet, sheetname) {
    
    if (sheetname %in% as.character(1:3)) {
        cols <- visitor_cols(sheetname)
    }
    
    else {
        cols <- othercols(sheet)
    }
    
    enc2utf8(cols)
    
}

extract_data <- function(sheet, sheetname) {
    # TODO: refactor to accept/find dynamic ranges?
    datarange <- c(match('01 北海道', sheet[[1]]),
                   match('47 沖縄県', sheet[[1]]))

    columns <- define_cols(sheet, sheetname)
    
    sheet[datarange[1]:datarange[2], 1:length(columns)] %>% 
        `colnames<-`(columns)
    
}


tidy_data <- function(sheet, sheetname) {

    if (sheetname %in% as.character(1:3)) {
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
    else if (sheetname == '4') {
        sheet %>% 
            separate(都道府県, c('index', '都道府県')) %>% 
            select(-index) %>% 
            gather(key = 'attraction_type', value = 'attractions', -都道府県)
    }
    else if (sheetname == '5') {
        sheet %>% 
            separate(都道府県, c('index', '都道府県')) %>% 
            select(-index) %>% 
            gather(key = 'attraction_type', value = '1K_visitors', -都道府県)
    }
    else if (sheetname == '6') {
        sheet %>% 
            separate(都道府県, c('index', '都道府県')) %>% 
            select(-index) %>% 
            gather(key = 'attraction_type', value = 'attractions', -都道府県)
    }
    else {
        sheet %>% 
            separate(都道府県, c('index', '都道府県')) %>% 
            select(-index)
    }
}

#-観光地点計, -パラメータ地点総数, 

clean_data <- function(sheet, sheetname, datapath) {

    sheet %>% 
        mutate_if(.predicate = is.character,
                  .funs = enc2native) %>% 
        mutate_at(vars(matches('（|数|attractions|1K_visitors')),
                  .funs = function(x) as.numeric(ifelse(is.na(x), 0, x))) %>% 
        mutate(filename = gsub('./data/', '', datapath))
}

# match() function gives indices within an object
# using match() and ncol() to define ranges in a spreadsheet can be very useful
# sheet names can be extremely misleading...

#sheets <- excel_sheets(datapath)[-1]
#
#cleaned_data <- 
#    pmap(.l = list(datapath, 
#                   sheets),
#         .f = ~ read_xls(path = ..1,
#                         sheet = ..2)) %>% 
#    map2(.y = sheets,
#         .f = extract_data) %>% 
#    map2(.y = sheets, 
#         .f = tidy_data) %>% 
#    map2(.y = sheets,
#         .f = clean_data)


join_metadata <- function(sheet) {
    sheet %>% 
        inner_join(extracted_data, by = 'filename')
}


wrangle <- function(datapath){
    sheets <- excel_sheets(datapath)[-1]
    
    pmap(.l = list(datapath, 
                   sheets),
         .f = ~ read_xls(path = ..1,
                         sheet = ..2)) %>% 
        map2(.y = sheets,
             .f = extract_data) %>% 
        map2(.y = sheets, 
             .f = tidy_data) %>% 
        pmap(.l = list(., sheets, datapath),
             .f = ~ clean_data(..1, ..2, ..3)) %>% 
        map(join_metadata)
}


filelist <- paste0("./data/", list.files('data/', '.xls'))
all_data <- map(filelist, wrangle)

firstsheets <- lapply(all_data, function(x) {
    x[[1]]
})

testbinding <- bind_rows(firstsheets)
testbinding <- rbindlist(firstsheets)

