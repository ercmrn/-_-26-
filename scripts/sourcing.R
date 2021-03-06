library(rvest)
library(httr)
library(tidyverse)


extract_links <- function(source_url) {

    page <- read_html(source_url)
    
    linkattr <- 
        page %>% 
        html_nodes("a") %>% 
        html_attr('href')
    
    
    all_li <- 
        page %>% 
        html_nodes("li") %>% 
        html_text()

    # get all links that include Excel in the text, and .xls in the file
    data.frame(all_li[grepl('Excel', all_li)] %>% str_trim(), 
               url_parse(source_url)[c('scheme', 'server')],
               linkattr[grepl('.xls', linkattr)], 
               stringsAsFactors = FALSE) %>% 
        `colnames<-`(c('li_text', 'scheme', 'server', 'li_filepath')) %>% 
        mutate(timerange = ifelse(grepl('年間値', li_text), 
                                  'yearly', 
                                  'quarterly'),
               li_text = gsub(pattern = '全',
                              replacement = '0/', 
                              x = li_text))
}


parse_metadata <- function(string) {
    
    regex_list <- c('（集計済：([[:digit:]]+)/',
                    '([[:digit:]]+)',
                    '都道府県 ※([[:alpha:]]+)',
                    '([[:digit:]]+\\.[[:digit:]]+\\.[[:digit:]]+)更新）',
                    '［(Excel)：([[:digit:]]+[[:alpha:]]+)］')
    
    if (grepl('年間値', string)) {
    
        first_regexes <- c('^【年間値：([[:alpha:]]+)',
                            '([[:digit:]]+)[[:alpha:]]+】')
        
        matches <- str_match_all(enc2utf8(string),
                                 enc2utf8(paste0(c(first_regexes, regex_list), 
                                                 collapse = '')))
        append(matches[[1]], "1-12", 3)
    }
    
    else {
        first_regexes <- c('^([[:alpha:]]+)',
                           '([[:digit:]]+)年',
                           '([[:digit:]]+-[[:digit:]]+)月期')
        
        str_match_all(enc2utf8(string),
                      enc2utf8(paste0(c(first_regexes, regex_list), 
                                      collapse = '')))[[1]][1,]
    }
}


extract_metadata <- function(files) {
    metadata_cols <- c('li_text', 'period', 'year', 'months', 
                       'included_prefectures', 'total_prefectures', 
                       'alt_period', 'alt_date_jpn',
                       'filetype_name', 'filesize')
    
    data.frame(map(files$li_text, parse_metadata), 
               stringsAsFactors = FALSE) %>% 
        t() %>% 
        as.data.frame() %>% 
        `rownames<-`(NULL) %>% 
        `colnames<-`(metadata_cols)
    
}


GET_func <- function(link, filename) {
    GET(link, write_disk(paste0('./data/', filename),
                         overwrite = TRUE))
}


calculate_year <- function(year, months) {
    paste0(as.character(1988 + as.numeric(as.character(year))),
           '-',
           as.numeric(str_match(months, '[[:digit:]]+$')) %/% 3)
}


source_url <- 'http://www.mlit.go.jp/kankocho/siryou/toukei/irikomi.html'

extracted_data <- extract_links(source_url)

extracted_data <- 
    extracted_data %>% 
    inner_join(extract_metadata(extracted_data), by = 'li_text') %>% 
    mutate(data_link = paste0(scheme, '://', server, li_filepath),
           filename = gsub('/common/', '', li_filepath),
           year_quarter = ifelse(timerange == 'yearly', 
                                 1988 + as.numeric(as.character(year)), 
                                 calculate_year(year, months)))

files <- 
    map2(.x = extracted_data$data_link,
         .y = extracted_data$filename,
         .f = GET_func)

