
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
  suppressPackageStartupMessages(require(cidian))
  suppressPackageStartupMessages(require(magrittr))
  
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

userSegment <- function(user_dict, stopwords_path, scels_path, mix_dict,
                        words_path, out_path, labels_path) {
  suppressPackageStartupMessages(require(jiebaR))
  # cat(sprintf('%s: userDictWrite...\n', Sys.time()))
  # system('python dicCreatePy.py')
  
  cat(sprintf('%s: addScel...\n', Sys.time()))
  addScel(user_dict, scels_path, mix_dict)
  
  words <- iconv(readLines(words_path), 'utf-8', 'gbk', sub = 'utf-8')
  
  cat(sprintf('%s: segment...\n', Sys.time()))
  seg <- worker(user = mix_dict, stop_word = stopwords_path)
  segs <- purrr::map(words, segment, jiebar = seg)
  if (!is.na(labels_path)) {
    labels <- readLines(labels_path) %>% 
      iconv(from = 'utf-8', to = 'gbk') %>% 
      strsplit(' ')
    temp <- compareFunc(labels, segs)
    cat(sprintf('compare segment with labels: %s(precision), %s(recall)\n', 
                temp[1], temp[2]))
  }
  tryCatch({
    writeLines(enc2utf8(sapply(segs, function(x)paste0(x, collapse = ' '))), 
               out_path, 
               useBytes = TRUE)
    cat(sprintf('%s: write segment to file %s\n', Sys.time(), out_path))
  }, error = function(e)e)
}

main <- function() {
  suppressPackageStartupMessages(require(optparse))
  option_list <- list(
    make_option('--user_dict', type = 'character', default = 'dict/user_dict_complaint_all.utf8'), 
    make_option('--scels_path', type = 'character', default = "dict/ICD-10_1.scel, dict/ICD10.scel"), 
    make_option('--mix_dict', type = 'character', default = 'dict/user_dict_StatAndScel.utf8'), 
    make_option('--stopwords_path', type = 'character', default = 'data/stop_words.utf8'), 
    make_option('--words_path', type = 'character', default = 'data/complaint_all.txt'), 
    make_option('--labels_path', type = 'character', default = NA),
    make_option('--out_path', type = 'character', default = 'data/segment.txt')
  )
  opt <- parse_args(OptionParser(option_list = option_list))
  opt$scels_path <- stringr::str_trim(unlist(strsplit(opt$scels_path, ',')))
  userSegment(opt$user_dict, opt$stopwords_path, opt$scels_path, opt$mix_dict, 
              opt$words_path, opt$out_path, opt$labels_path)
}

main()

# argGridSearch <- function(arg_df) {
#   require(magrittr)
#   pbapply::pbsapply(
#     seq_len(nrow(arg_df)), 
#     function(x) {
#       tmp <- tempfile()
#       sink(tmp)
#       result <- invisible(
#         userSegment(
#           CORPUS_FILE, USER_DICT, is_stop = TRUE, stopwords_cn, 
#           term_max_length = arg_df$TERM_MAX_LENGTH[x],
#           thr_p = arg_df$THR_P[x],
#           thr_f = arg_df$THR_F[x],
#           thr_freq = arg_df$THR_FREQ[x], 
#           thr_idf = arg_df$THR_IDF[x]
#         )
#       )
#       sink()
#       file.remove(tmp)
#       return(result)
#     }, 
#     # simplify = FALSE, 
#     USE.NAMES = TRUE
#   ) %>% 
#     t() %>% 
#     gsub('%', '', .) %>% 
#     cbind(arg_df, .) %>% {
#       .$precision = as.numeric(as.character(.$precision))
#       .$recall = as.numeric(as.character(.$recall))
#       .
#     } %>% 
#     arrange(desc(recall), desc(precision))
# }

