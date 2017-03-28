library(jiebaR)
library(dplyr)
library(magrittr)
library(parallel)
library(stringi)
library(tm)
# library(luzlogr)

dicCreate <- function(words_original, 
                      term_max_length, 
                      thr_p, 
                      thr_f, 
                      thr_freq, 
                      thr_idf) {
  require(dplyr)
  require(magrittr)
  require(parallel)
  require(stringi)
  require(tm)
  
  words <- paste0(words_original, collapse = '')
  cl <- makeCluster(4)
  n <- nchar(words)
  st <- 1:n
  
  clusterExport(cl, 'words', environment())
  clusterExport(cl, 'st', environment())
  invisible(clusterEvalQ(cl, require(magrittr)))
  invisible(clusterEvalQ(cl, require(stringi)))
  invisible(clusterEvalQ(cl, entropyCaculate <- function(vec) {
    require(freqCountCpp)
    vec <- unname(freqCountCpp(vec))
    p <- vec / sum(vec)
    sum(-p * log(p))
  }))
  
  # seg_words
  cat(sprintf('%s : seg_words start', Sys.time()), '\n')
  seg_words <- parLapply(cl, 0:(term_max_length - 1), function(i){
    temp <- stri_sub(words, st, i + st)
    return(temp[nchar(temp) == (i + 1)])
  })
  cat(sprintf('%s : seg_words finish', Sys.time()), '\n')
  
  # word_dic
  cat(sprintf('%s : word_freq start', Sys.time()), '\n')
  word_freq <- table(unlist(seg_words))
  rm(seg_words)
  cat(sprintf('%s : word_freq finish', Sys.time()), '\n')
  word_dic <- word_freq / length(word_freq)
  dic_names <- names(word_dic)
  rm(word_freq)
  
  clusterExport(cl, 'dic_names', environment())
  
  # free
  cat(sprintf('%s : free start', Sys.time()), '\n')
  free <- parSapply(cl, dic_names, function(w){
    pos <- stri_locate_all(words, fixed = w)[[1]]
    pre <- stri_sub(words, pos[, 1] - 1, length = 1) %>% entropyCaculate()
    suff <- stri_sub(words, pos[, 2] + 1, length = 1) %>% entropyCaculate()
    return(min(pre, suff))
  })
  cat(sprintf('%s : free finish', Sys.time()), '\n')
  
  stopCluster(cl)
  
  pmi <- word_dic
  
  # ugly but efficient way to caculate pmi
  words_len_2 <- dic_names[nchar(dic_names) == 2]
  pmi[words_len_2] <- word_dic[substr(words_len_2, 1, 1)] * 
    word_dic[substr(words_len_2, 2, 2)]
  
  words_len_3 <- dic_names[nchar(dic_names) == 3]
  pmi[words_len_3] <- pmax(word_dic[substring(words_len_3, 1, 1)] * 
                             word_dic[substring(words_len_3, 2, 3)], 
                           word_dic[substring(words_len_3, 1, 2)] * 
                             word_dic[substring(words_len_3, 3, 3)])
  
  words_len_4 <- dic_names[nchar(dic_names) == 4]
  pmi[words_len_4] <- pmax(word_dic[substring(words_len_4, 1, 1)] * 
                             word_dic[substring(words_len_4, 2, 4)], 
                           word_dic[substring(words_len_4, 1, 2)] * 
                             word_dic[substring(words_len_4, 3, 4)], 
                           word_dic[substring(words_len_4, 1, 3)] * 
                             word_dic[substring(words_len_4, 4, 4)])
  
  words_len_5 <- dic_names[nchar(dic_names) == 5]
  pmi[words_len_5] <- pmax(word_dic[substring(words_len_5, 1, 1)] * 
                             word_dic[substring(words_len_5, 2, 5)], 
                           word_dic[substring(words_len_5, 1, 2)] * 
                             word_dic[substring(words_len_5, 3, 5)], 
                           word_dic[substring(words_len_5, 1, 3)] * 
                             word_dic[substring(words_len_5, 4, 5)], 
                           word_dic[substring(words_len_5, 1, 4)] * 
                             word_dic[substring(words_len_5, 5, 5)])
  
  pmi <- log(word_dic / pmi)
  
  word_df <- data.frame(word = names(pmi), 
                        pmi = as.vector(pmi), 
                        free = unname(free), 
                        row.names = NULL, 
                        stringsAsFactors = FALSE)
  
  result <- word_df %>% 
    mutate(freq = as.vector(word_freq[as.character(word)])) %>%
    filter(free > thr_f & pmi > thr_p & freq > thr_freq) %>% 
    mutate(idf = log(
      length(words_original) /
        (unlist(purrr::map(word, function(x)sum(grepl(x, words_original)))) + 1)
    )) %>%
    filter(idf < thr_idf) %>%
    arrange(desc(freq), idf, desc(pmi), desc(free))
  
  cat(sprintf('%s : end', Sys.time()), '\n')
  return(result)
}

# discover user dictionary based on statistics, create user_dict.txt
userDictWrite <- function(corpus_file, is_stop, stopwords, 
                          term_max_length, thr_p, thr_f, thr_freq, thr_idf, 
                          # pmi_weight = PMI_WEIGHT, free_weight = FREE_WEIGHT, 
                          # freq_weight = FREQ_WEIGHT, 
                          # thr_score = THR_SCORE, 
                          user_dict) {
  require(dplyr)
  require(magrittr)
  require(tm)
  
  # tryCatch({
  #   pmi_weight <- pmi_weight / (pmi_weight + free_weight + freq_weight)
  #   free_weight <- free_weight / (pmi_weight + free_weight + freq_weight)
  #   freq_weight <- 1 - pmi_weight - free_weight
  # }, error = function(e) {
  #   cat(e)
  #   pmi_weight <- 1
  #   free_weight <- 1
  #   freq_weight <- 1
  # })
  
  cat(sprintf('%s : file: %s', Sys.time(), corpus_file), '\n')
  readLines(corpus_file) %>% 
    # paste0(collapse = '') %>% 
    removePunctuation() %>% 
    removeNumbers() %>% # 是否需要？
    stripWhitespace() %>% 
    gsub(' ', '', .) %>% 
    # paste0(collapse = ' ') %>% 
    {  # 病例文本可能不需要停止词
      if (!is_stop) {
        .
      } else {
        gsub(stopwords_cn, '', .)
      }
    } %>% 
    dicCreate(term_max_length = term_max_length, 
              thr_p = thr_p, 
              thr_f = thr_f, 
              thr_freq = thr_freq, 
              thr_idf = thr_idf) %>% 
    # mutate(pmi = as.vector(scale(pmi)), 
    #        free = as.vector(scale(free)), 
    #        freq = as.vector(scale(freq))) %>% 
    # mutate(score = pmi_weight * pmi + 
    #          free_weight * free + 
    #          freq_weight * freq) %>% 
    # filter(score > thr_score) %>% 
    # arrange(desc(score)) %>% 
    select(word) %>% 
    magrittr::extract(, 1) %>% 
    enc2utf8() %>% 
    writeLines(user_dict, useBytes = TRUE)
}
