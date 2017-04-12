
itemSearch <- function(url) {
  suppressPackageStartupMessages(require(rvest))
  suppressPackageStartupMessages(require(magrittr))
  
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

# pjs_path <- 'phantomjs-2.1.1-windows/bin/phantomjs.exe'
crawler <- function(n_urls, n_scroll_down, max_sleep_time,
                    baike_all_path, baike_para_path, 
                    pjs_path) {
  cat(n_urls, '\n')
  cat(n_scroll_down, '\n')
  cat(max_sleep_time, '\n')
  cat(baike_all_path, '\n')
  cat(baike_para_path, '\n')
  cat(pjs_path, '\n')
  suppressPackageStartupMessages(require(rvest))
  suppressPackageStartupMessages(require(magrittr))
  suppressPackageStartupMessages(require(RSelenium))
  suppressPackageStartupMessages(require(pbapply))
  
  baike_url <- 'http://baike.baidu.com/wikitag/taglist?tagId=75953'
  
  pjs <- phantom(pjs_path, port = 5555)
  ecap <- list(phantomjs.page.settings.userAgent = 
                 'Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/42.0.2311.154 Safari/537.36 LBBROWSER')
  remDr <- remoteDriver(browserName = 'phantomjs', port = 5555, extraCapabilities = ecap)
  remDr$open()
  remDr$navigate(baike_url)
  
  cat(sprintf('%s get urls list\n', Sys.time()))
  i <- 1
  href_list <- 0
  pb <- startpb(0, n_urls)
  while(i <= n_scroll_down & length(href_list) <= n_urls) {
    remDr$executeScript("window.scrollTo(0,document.body.scrollHeight);")
    href_list <- remDr$findElements('css selector', 'div.waterFall_item a') %>% 
      sapply(function(x)unlist(x$getElementAttribute('href')))
    i <- i + 1
    Sys.sleep(runif(1, max = max_sleep_time))
    setpb(pb, length(href_list))
  }
  closepb(pb)
  
  remDr$close()
  pjs$stop()
  
  cat(sprintf('%s get content from url of urls list\n', Sys.time()))
  baike_items <- pbapply::pblapply(href_list, function(url) {
    Sys.sleep(runif(1, max = max_sleep_time))
    itemSearch(url)
  })
  
  nameIdc <- c('别    称', '别    名', '又    称', '中医病名')
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
  
  save(baike_items_df, file = baike_all_path)
  cat(sprintf('%s write baike items dataframe to %s\n', Sys.time(), baike_all_path))
  writeLines(enc2utf8(baike_items_df[, 'para']), baike_para_path, useBytes = TRUE)
  cat(sprintf('%s write baike para text to %s\n', Sys.time(), baike_para_path))
}

main <- function() {
  suppressPackageStartupMessages(require(optparse))
  option_list <- list(
    make_option('--n_urls', type = 'integer', default = 25), 
    make_option('--n_scroll_down', type = 'integer', default = 1), 
    make_option('--max_sleep_time', type = 'numeric', default = 0.2), 
    make_option('--baike_all_path', type = 'character', default = 'data/baike_all.rda'), 
    make_option('--baike_para_path', type = 'character', default = 'data/baike_para.txt'), 
    make_option('--pjs_path', type = 'character', default = 'phantomjs-2.1.1-windows/bin/phantomjs.exe')
  )
  opt <- parse_args(OptionParser(option_list = option_list))
  crawler(opt$n_urls, opt$n_scroll_down, opt$max_sleep_time, 
          opt$baike_all_path, opt$baike_para_path, opt$pjs_path)
}

# test <- apply(baike_items_df, 1, function(x) {
#   idx <- labels %in% unlist(strsplit(x['room'], '，|、|；|-|,'))
#   if (any(idx)) {
#     return(which(idx)[1])
#   } else {
#     return(0)
#   }
# })
# writeLines(enc2utf8(as.character(test)), 'test.txt', useBytes = TRUE)
