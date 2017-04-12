
library(jiebaR)
library(purrr)
library(magrittr)

singleCompare <- function(x, y) {
  precision_n <- sum(y %in% x)
  recall_n <- sum(x %in% y)
  # if (length(y) > 0) {
  #   precision <- sum(y %in% x) / length(y)
  # }
  # if (length(x) > 0) {
  #   recall <- sum(x %in% y) / length(x)
  # }
  # if (!is.na(precision) && !is.na(recall)) {
  #   f1 <- 2 * precision * recall / (precision + recall)
  # }
  return(c(precision_n = precision_n,
           recall_n = recall_n))
}

compareFunc <- function(label, resultSegment) {
  require(purrr)
  require(magrittr)
  map2(label, resultSegment, singleCompare) %>% 
    do.call(rbind, .) %>% 
    colSums() %>% {
      precision <- extract(., 1) / length(unlist(resultSegment))
      recall <- extract(., 2) / length(unlist(label))
      c(precision = precision, 
        recall = recall)
    } %>% 
    scales::percent() %>% 
    setNames(c('precision', 'recall'))
}

label <- readLines('data/complaint_all_label.utf8') %>% 
  iconv(from = 'utf-8', to = 'gbk') %>% 
  strsplit(' ')
COMPLAINT <- readLines('data/complaint_all.txt')

# ============================================================================

# jiebar without user dict, without stopword
require(jiebaR)
seg <- worker()
jiebar <- map(COMPLAINT, segment, jiebar = seg)
result_jiebar <- compareFunc(label, jiebar)
# precision    recall 
# "6.5%"   "39.0%"

# jiebar with stopword
seg_with_stopword <- worker(stop_word = 'data/stop_words.utf8')
jiebar_stopword <- map(COMPLAINT, segment, jiebar = seg_with_stopword)
result_jiebar_stopword <- compareFunc(label, jiebar_stopword)
# precision    recall 
# "7.6%"   "39.0%" 

# jiebar with user dict
seg_with_userDict <- worker(user = 'dict/user_dict_complaint_all.utf8')
jiebar_userDict <- map(COMPLAINT, segment, jiebar = seg_with_userDict)
result_jiebar_userDict <- compareFunc(label, jiebar_userDict)
# precision    recall 
# "13.4%"   "71.4%" 

# jiebar with user dict and ICD10
addScel <- function(dict_path, scel_path, out_path) {
  require(cidian)
  require(magrittr)
  # require(pbapply)
  tmp <- tempfile('scel', fileext = '.utf8')
  decode_scel(scel_path, tmp, progress = TRUE)
  new_words <- readLines(tmp) %>% 
    iconv(from = 'utf-8', 'gbk') %>%
    lapply(function(x) {
      unlist(strsplit(x, ' '))[1]
    }) %>% 
    unlist() %>% 
    enc2utf8()
  if (!identical(dict_path, '')) {
    before_dict <- load_user_dict(dict_path)
  } else {
    before_dict <- list()
  }
  user_dict_names <- unique(c(names(before_dict), new_words))
  user_dict <- replicate(length(user_dict_names), 'n', simplify = FALSE)
  names(user_dict) <- user_dict_names
  # user_dict <- pbsapply(user_dict_names,
  #                       simplify = FALSE,
  #                       USE.NAMES = TRUE,
  #                       function(x) {
  #                         if (!is.null(before_dict[[x]])) {
  #                           return(before_dict[[x]])
  #                         }
  #                         return('userDefine')
  #                       })
  attr(user_dict, 'class') <- 'user_dict'
  # user_dict <- add_user_words(before_dict, new_words, rep('userDefine', length(new_words)))
  write_dict(user_dict, out_path)
}
# precision    recall 
# "14.2%"   "75.1%" 


# ============================================================================
# Rwordseg
require(Rwordseg)
rwordseg <- map(COMPLAINT, segmentCN)
result_rwordseg <- compareFunc(label, rwordseg)
# precision    recall 
# "5.5%"   "35.1%"

# Rwordseg with user dict
loadDict('dict/user_dict_complaint_all.utf8')
rwordseg_userDict <- map(COMPLAINT, segmentCN)
result_rwordseg_userDict <- compareFunc(label, rwordseg_userDict)
# precision    recall 
# "13.3%"   "68.8%" 

insertWords('皮肤黄染')
rwordseg_userDict_test <- map(COMPLAINT, segmentCN)
result_rwordseg_userDict_test <- compareFunc(label, rwordseg_userDict_test)
# precision    recall 
# "15.5%"   "78.4%" 

# installDict('d:/360极速浏览器下载/ICD-10疾病编码1.scel', 'icd10', 'scel')
# precision    recall 
# "16.0%"   "80.8%" 
# 
# lapply(rwordseg_userDict_test, function(x)x[!names(x) %in% c('m')])
# precision    recall 
# "20.7%"   "80.8%" 

# =============================================================================
# xunfei


xunfeiKey <- ''
xunfeiUrl <- 'http://ltpapi.voicecloud.cn/analysis'

xunfeiSegment <- function(textForSeg = COMPLAINT, api_key, baseUrl) {
  require(httr)
  require(pbapply)
  result_xunfei <- vector('list', length = length(textForSeg))
  pb <- timerProgressBar(style = 5)
  for (i in seq_along(textForSeg)) {
    if (i %% 200 == 0) {
      Sys.sleep(2)
    }
    result_xunfei[[i]] <- tryCatch({
      temp <- GET(
        baseUrl, 
        query = list(
          api_key = api_key,
          text = textForSeg[i],
          pattern = 'ws',
          format = 'plain'
        )
      )
      if (status_code(temp) != 200) {
        ''
      } else {
        unlist(strsplit(content(temp), ' '))
      }
    }, error = function(e) {
      return('')
    })
    
    setTimerProgressBar(pb, value = i / length(textForSeg))
  }
  # close(pb)
  return(result_xunfei)
}
segment_xunfei <- xunfeiSegment(COMPLAINT, xunfeiKey, xunfeiUrl)
result_xunfei <- compareFunc(label, segment_xunfei)
# precision    recall 
# "5.4%"   "40.8%" 

# ===========================================================================
# ltp-cloud

ltpKey <- ''
ltpUrl <- 'http://api.ltp-cloud.com/analysis'
segment_ltp <- xunfeiSegment(COMPLAINT, ltpKey, ltpUrl)

# =============================================================================
#  boson
bosonKey <- ''
bosonUrl <- 'http://api.bosonnlp.com/tag/analysis'
bosonSegment <- function(textForSeg = COMPLAINT, bosonKey, bosonUrl) {
  require(httr)
  require(pbapply)
  result <- vector('list', length = length(textForSeg))
  pb <- timerProgressBar(style = 5)
  i <- 1
  while (i <= length(textForSeg)) {
    j <- ifelse(i + 99 > length(textForSeg), 
                length(textForSeg), 
                i + 99)
    
    temp <- POST(bosonUrl, 
                 add_headers(.headers = c('X-Token' = bosonKey, 
                                          'Content-Type' = 'application/json', 
                                          'Accept' = 'application/json')), 
                 body = jsonlite::toJSON(textForSeg[i:j]))
    
    if (status_code(temp) != 200) {
      cat('\n',sprintf('status_code: %s', status_code(temp)))
      result[i:j] <- ''
    } else {
      result[i:j] <- map(content(temp), function(x)unlist(x$word))
    }
    Sys.sleep(1)
    i <- j + 1
    setTimerProgressBar(pb, value = j / length(textForSeg))
  }
  
  return(result)
}
segment_boson <- bosonSegment(COMPLAINT, bosonKey, bosonUrl)
result_boson <- compareFunc(label, segment_boson)
# precision    recall 
# "3.4%"   "27.4%" 

# =============================================================================
# ltp with user dict
model_exe <- 'ltp/cws_cmdline'
model_path <- paste('--segmentor-model', 'ltp/cws.model')
threads_num <- paste('--threads', '1')
input_path <- paste('--input', 'ltp/complaint.utf8')
seg_lexicon <- paste('--segmentor-lexicon', 'dict/user_dict_complaint_all.utf8')
output_path <- 'ltp/segment_ltp.txt'
output_path_userDict <- 'ltp/segment_ltp_userDict.txt'

writeLines(enc2utf8(tm::removePunctuation(COMPLAINT)), 
           'ltp/complaint.utf8', useBytes = TRUE)
tryCatch(
  system2(model_exe, 
          args = c(input_path, model_path), 
          stdout = output_path), 
  error = function(e)e
)
tryCatch(
  system2(model_exe, 
          args = c(input_path, model_path, seg_lexicon), 
          stdout = output_path_userDict), 
  error = function(e)e
)

segment_ltp <- readLines(output_path) %>% 
  iconv('utf-8', 'gbk') %>% 
  lapply(function(x) {
    unlist(strsplit(x, '\t'))
  })
result_ltp <- compareFunc(label, segment_ltp)
# precision    recall 
# "5.6%"   "34.1%" 

segment_ltp_userDict <- readLines(output_path_userDict) %>% 
  iconv('utf-8', 'gbk') %>% 
  lapply(function(x) {
    unlist(strsplit(x, '\t'))
  })
result_ltp_userDict <- compareFunc(label, segment_ltp_userDict)
# precision    recall 
# "11.7%"   "62.3%" 


