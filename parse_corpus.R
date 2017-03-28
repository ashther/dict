
library(rvest)
library(magrittr)
library(RSelenium)

href_need <- 1000

baike_url <- 'http://baike.baidu.com/wikitag/taglist?tagId=75953'

pjs <- phantom('phantomjs-2.1.1-windows/bin/phantomjs.exe', port = 5555)
ecap <- list(phantomjs.page.settings.userAgent = 
               'Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/42.0.2311.154 Safari/537.36 LBBROWSER')
remDr <- remoteDriver(browserName = 'phantomjs', port = 5555, extraCapabilities = ecap)
remDr$open()
remDr$navigate(baike_url)

i <- 1
href_list <- 0
while(i <= 50 & length(href_list) <= href_need) {
  remDr$executeScript("window.scrollTo(0,document.body.scrollHeight);")
  href_list <- remDr$findElements('css selector', 'div.waterFall_item a') %>% 
    sapply(function(x)unlist(x$getElementAttribute('href')))
  cat(sprintf("%s: %s times  %s", Sys.time(), i, length(href_list)), '\n')
  i <- i + 1
  Sys.sleep(runif(1, 0.5, 1.5))
}

remDr$close()
pjs$stop()

itemSearch <- function(url) {
  require(rvest)
  require(magrittr)
  
  basicInfo <- c()
  para <- c()
  
  tryCatch({
    page_content <- read_html(url)
    
    name <- page_content %>% 
      html_node('dd.lemmaWgt-lemmaTitle-title h1') %>% 
      html_text()
    
    basicInfo <- page_content %>% 
      html_nodes('.basicInfo-block dd') %>% 
      html_text() %>% 
      gsub('\\n', '', .)
    basicInfoNames <- page_content %>% 
      html_nodes('.basicInfo-block dt') %>% 
      html_text() %>% 
      gsub('\\n', '', .)
    names(basicInfo) <- basicInfoNames
    
    para <- page_content %>% 
      html_nodes('div[class^="para"]') %>% 
      html_text() %>% 
      gsub('\\n', '', .)
  }, error = function(e)e)
  
  return(list(basicInfo = basicInfo, 
              name = name, 
              url = url, 
              para = para))
}

baike_items <- pbapply::pblapply(href_list, function(url) {
  Sys.sleep(runif(1, 0, 0.5))
  itemSearch(url)
})

nameIdc <- c('别    称', '别    名', '又    称', '中医病名') # 无法识别空格?
name_2Idc <- c('英文名称', '英文别称', '英文别名', '西医学名')
roomIdc <- c('就诊科室', '所属科室')
reasonIdc <- c('常见病因', '主要病因')
bodyPartIdc <- c('常见发病部位', '发病部位')
symptomIdc <- c('常见症状', '典型症状', '主要症状')
isInfectiousIdc <- c('传染性')
infectiousWayIdc <- c('传播途径')
peopleIdc <- c('多发群体')

baike_items_df <- lapply(baike_items, function(item) {

  result <- c()
  result['url'] <- item$url
  result['name'] <- item$name
  result['name_2'] <- paste0(item$basicInfo[names(item$basicInfo) %in% name_2Idc], collapse = ',')
  result['room'] <- paste0(item$basicInfo[names(item$basicInfo) %in% roomIdc], collapse = ',')
  result['reason'] <- paste0(item$basicInfo[names(item$basicInfo) %in% reasonIdc], collapse = ',')
  result['bodyPart'] <- paste0(item$basicInfo[names(item$basicInfo) %in% bodyPartIdc], collapse = ',')
  result['symptom'] <- paste0(item$basicInfo[names(item$basicInfo) %in% symptomIdc], collapse = ',')
  result['isInfectious'] <- paste0(item$basicInfo[names(item$basicInfo) %in% isInfectiousIdc], collapse = ',')
  result['infectiousWay'] <- paste0(item$basicInfo[names(item$basicInfo) %in% infectiousWayIdc], collapse = ',')
  result['people'] <- paste0(item$basicInfo[names(item$basicInfo) %in% peopleIdc], collapse = ',')
  result['para'] <- paste0(item$para, collapse = '')
  
  return(result)
}) %>% 
  do.call(rbind, .)


# itemNames_from_baikeBaidu <- pbapply::pbsapply(href_list, function(x) {
#   tryCatch({
#     Sys.sleep(0.1)
#     read_html(x) %>% 
#       html_node('dd.lemmaWgt-lemmaTitle-title h1') %>% 
#       html_text()
#   }, 
#     error = function(e)cat(e$message)
#   )
# })
# 
# baike_items_df$name_0 <- itemNames_from_baikeBaidu