library(rvest)
library(tidy)


source_url <- 'http://www.mlit.go.jp/kankocho/siryou/toukei/irikomi.html'

tourism_page <- read_html(source_url)

linktext <- 
    tourism_page %>% 
    html_nodes(".linkList") %>% 
    html_text()

linkattr <- 
    tourism_page %>% 
    html_nodes("a") %>% 
    html_attr('href')


all_li <- 
    tourism_page %>% 
    html_nodes("li") %>% 
    html_text()


linkattr[grepl('.xls', linkattr)]

all_li[grepl('Excel', all_li)] %>% str_trim()

url_parse(source_url)[c('scheme', 'server')]

raw_urldata <- 
    data.frame(all_li[grepl('Excel', all_li)] %>% str_trim(), 
                     url_parse(source_url)[c('scheme', 'server')],
                     linkattr[grepl('.xls', linkattr)], 
               stringsAsFactors = FALSE) %>% 
    `colnames<-`(c('filename', 'li_text', 'scheme', 'server')) %>% 
    mutate(timerange = ifelse(grepl('年間値', filename), 'yearly', 'quarterly'),
           filename = gsub(pattern = '全', replacement = '0/', x = filename))

yearly_regexes <- c('^【年間値：([[:alpha:]]+)',
                    '([[:digit:]]+)[[:alpha:]]+】')
quarterly_regexes <- c('^([[:alpha:]]+)',
                       '([[:digit:]]+)年',
                       '([[:digit:]]+-[[:digit:]]+)月期')
                       
regex_list <- c('（集計済：([[:digit:]]+)/',
                '([[:digit:]]+)',
                '都道府県 ※([[:alpha:]]+)',
                '([[:digit:]]+\\.[[:digit:]]+\\.[[:digit:]]+)更新）',
                '［(Excel)：([[:digit:]]+[[:alpha:]]+)］')

metadata_cols <- c('filename', 'period', 'year', 'months', 
                   'included_prefectures', 'total_prefectures', 
                   'alt_period', 'alt_date_jpn',
                   'filetype_name', 'filesize')

str_match_all(enc2utf8('平成29年1-3月期（集計済：20/46都道府県 ※H30.4.27更新）［Excel：131KB］'), 
            enc2utf8(paste0(c(quarterly_regexes, regex_list), collapse='')))[[1]][1,]

str_match_all(enc2utf8('【年間値：平成29年】（集計済：1/46都道府県 ※H30.4.27更新）［Excel：94KB］'), 
              enc2utf8(paste0(c(yearly_regexes, regex_list), collapse='')))



outputs <- 
    data.frame(map(raw_urldata$filename, extract_url_metadata), 
               stringsAsFactors = FALSE) %>% 
    t() %>% 
    as.data.frame() %>% 
    `rownames<-`(NULL) %>% 
    `colnames<-`(metadata_cols) %>% 
    mutate(alt_date_iso8601 = )

extract_url_metadata <- function(string) {
    if (grepl('年間値', string)) {
        matches <- str_match_all(enc2utf8(string),
                                 enc2utf8(paste0(c(yearly_regexes, regex_list), 
                                                 collapse = '')))
        append(matches[[1]], "1-12", 3)
    }
    
    else {
        str_match_all(enc2utf8(string),
                      enc2utf8(paste0(c(quarterly_regexes, regex_list), 
                                      collapse = '')))[[1]][1,]
    }
}
