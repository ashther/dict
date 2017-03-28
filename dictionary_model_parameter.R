library(magrittr)
library(jiebaR)
source('dictionary_create.R')

singleCompare <- function(x, y) {
  precision_n <- sum(y %in% x)
  recall_n <- sum(x %in% y)
  return(c(precision_n = precision_n,
           recall_n = recall_n))
}

compareFunc <- function(label, resultSegment) {
  purrr::map2(label, resultSegment, singleCompare) %>% 
    do.call(rbind, .) %>% 
    colSums() %>% {
      precision <- magrittr::extract(., 1) / length(unlist(resultSegment))
      recall <- magrittr::extract(., 2) / length(unlist(label))
      c(precision = precision, 
        recall = recall)
    } %>% 
    scales::percent() %>% 
    setNames(c('precision', 'recall'))
}

addScel <- function(dict_path, scels, out_path) {
  require(cidian)
  require(magrittr)
  
  new_words <- lapply(scels, function(scel) {
    tmp <- tempfile('scel', fileext = '.utf8')
    decode_scel(scel, tmp, progress = FALSE)
    readLines(tmp) %>% 
      iconv(from = 'utf-8', to = 'gbk') %>% 
      lapply(function(x) {
        unlist(strsplit(x, ' '))[[1]]
      })
  }) %>%
    unlist() %>% 
    enc2utf8() %>% 
    unique()
  
  tryCatch(
    file.remove(paste(tempdir(), grep('^scel*', 
                                      list.files(tempdir()), 
                                      value = TRUE), sep = '\\')), 
    error = function(e)e
  )
  
  if (!identical(dict_path, '')) {
    before_dict <- load_user_dict(dict_path, default_tag = 'n')
  } else {
    before_dict <- list()
  }
  user_dict_names <- unique(c(names(before_dict), new_words))
  user_dict <- replicate(length(user_dict_names), 'n', simplify = FALSE)
  names(user_dict) <- user_dict_names
  attr(user_dict, 'class') <- 'user_dict'
  write_dict(user_dict, out_path, cpp_progress = FALSE)
}

label <- readLines('data/complaint_all_label.utf8') %>% 
  iconv(from = 'utf-8', to = 'gbk') %>% 
  strsplit(' ')
COMPLAINT <- readLines('data/complaint_all.txt')

CORPUS_FILE <- 'data/complaint_all.txt'

# PMI_WEIGHT <- 0.692 
# FREE_WEIGHT <- 0.231
# FREQ_WEIGHT <- 0.077
# TERM_MAX_LENGTH <- 5
# THR_P <- 0
# THR_F <- 0.6
# THR_FREQ <- 5
# THR_IDF <- 5
#THR_SCORE <- 0

USER_DICT <- 'dict/user_dict_complaint_all.utf8'
# USER_IDF <- 'user_idf.utf8'

stopwords_cn <- readLines('data/stop_words.utf8') %>%
  iconv(from = 'utf-8', to = 'gbk') %>%
  extract(156:877)
stopwords_cn <- c(stopwords_cn, '分钟', '周', '小时', '天', '克', '次', 
                  '次分') %>%
  paste(collapse = '|')
SCELS <- c('dict/ICD-10疾病编码1.scel', 'dict/ICD10诊断.scel')
OUT_PATH <- 'dict/user_dict_StatAndScel.utf8'

argSearch <- function(corpus_file, user_dict, is_stop, stopwords, 
                      term_max_length, thr_p, thr_f, thr_freq, thr_idf) {
  cat(sprintf('%s: userDictWrite...\n', Sys.time()))
  userDictWrite(corpus_file, is_stop, stopwords, 
                term_max_length, thr_p, thr_f, thr_freq, thr_idf, 
                user_dict)
  
  cat(sprintf('%s: addScel...\n', Sys.time()))
  addScel(user_dict, SCELS, OUT_PATH)
  
  cat(sprintf('%s: segment...\n', Sys.time()))
  require(jiebaR)
  seg <- worker(user = OUT_PATH, stop_word = 'data/stop_words.utf8')
  segs <- purrr::map(COMPLAINT, segment, jiebar = seg)
  compareFunc(label, segs)
}

argGridSearch <- function(arg_df) {
  require(magrittr)
  pbapply::pbsapply(
    seq_len(nrow(arg_df)), 
    function(x) {
      tmp <- tempfile()
      sink(tmp)
      result <- invisible(
        argSearch(
          CORPUS_FILE, USER_DICT, is_stop = TRUE, stopwords_cn, 
          term_max_length = arg_df$TERM_MAX_LENGTH[x],
          thr_p = arg_df$THR_P[x],
          thr_f = arg_df$THR_F[x],
          thr_freq = arg_df$THR_FREQ[x], 
          thr_idf = arg_df$THR_IDF[x]
        )
      )
      sink()
      file.remove(tmp)
      return(result)
    }, 
    # simplify = FALSE, 
    USE.NAMES = TRUE
  ) %>% 
    t() %>% 
    gsub('%', '', .) %>% 
    cbind(arg_df, .) %>% {
      .$precision = as.numeric(as.character(.$precision))
      .$recall = as.numeric(as.character(.$recall))
      .
    } %>% 
    arrange(desc(recall), desc(precision))
}


arg_df <- expand.grid(
  TERM_MAX_LENGTH = 5, # 5
  THR_P = 0:2, # 0
  THR_F = seq(0, 1, 0.075), # 0.6
  THR_FREQ = 5, 
  THR_IDF = 3:6
)

save(result, file = paste0('out/', format(Sys.time(), '%Y%m%d%H%M%S'), '.rda'))

apply(arg_df, 1, function(x) {
  mutate(test, score = x[1] * pmi + x[2] * free + x[3] * freq) %>% 
    select(score) %>% 
    range()
}) %>% 
  t()