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

cbind(linkattr[grepl('.xls', linkattr)], 
      all_li[grepl('Excel', all_li)] %>% str_trim(), 
      url_parse(source_url)[c('scheme', 'server')])
