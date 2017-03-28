
# ==========================================================================

data_source_files <- grep('^diagnose_\\d', 
                          fixed = FALSE,
                          list.files('../data source/'), 
                          value = TRUE)
# 多字典文件输出
lapply(data_source_files, function(x) {
  file_path <- paste0('../data source/', x)
  out_path <- paste0('user_dict_', unlist(strsplit(x, '.txt')), '.utf8')
  userDictWrite(file_path, user_dict = out_path)
})

# 单字典文件输出
lapply(data_source_files, function(x) {
  file_path <- paste0('../data source/', x)
  readLines(file_path)
}) %>% 
  do.call(c, .) %>% 
  writeLines('data/diagnose_all.txt', useBytes = FALSE)
# ==========================================================================

# make idf file based on user dictionary, create user_idf.txt
seg <- worker(user = USER_DICT)
readLines(CORPUS_FILE) %>% 
  lapply(segment, jiebar = seg) %>% 
  get_idf('data/stop_words.utf8', USER_IDF)

# make keywords based on user idf file
key <- worker('keywords', user = USER_DICT, idf = USER_IDF)
corpus_keywords <- readLines(CORPUS_FILE) %>% 
  removeNumbers() %>% 
  lapply(function(x) {
    tryCatch(
      keywords(x, key), 
      error = function(e) {
        keywords(removePunctuation(x), key) # only punctuation in sentence
      }
    )
  })

# # for comparing
# keyOriginal <- worker('keywords')
# corpus_keywords_original <- readLines(CORPUS_FILE) %>% 
#   lapply(keywords, jiebar = keyOriginal)

uniq_word <- unique(unlist(corpus_keywords))
tdMtx <- matrix(0, length(corpus_keywords), length(uniq_word), 
                dimnames = list(c(), uniq_word))

idx <- do.call('rbind', 
               lapply(seq_along(corpus_keywords), function(x) {
                 temp <- which(uniq_word %in% corpus_keywords[[x]])
                 matrix(c(rep(x, length(temp)), temp), ncol = 2)
               }))

tdMtx[idx] <- 1
corTdMtx <- cor(tdMtx)
corTdMtx[lower.tri(corTdMtx, TRUE)] <- 0
which(corTdMtx > 0.9, arr.ind = TRUE) %>% 
  apply(1, function(x)c(rownames(corTdMtx)[x[1]], 
                        colnames(corTdMtx)[x[2]])) %>% 
  t()
# 高胆红      25  24
# 素血症      24  25
# 缺血缺氧    58  57
# 性脑病      57  58
# 结肠炎      70  68
# 坏死性小肠  68  70
# 溶血       101 100
# RH         100 101
# 低出生     122 121
# 体重儿     121 122
# 病毒感染   197 130
# 障碍       196 194
# 障碍       196 195
# 胆汁酸     194 196
# 排泄       195 196
# 巨细胞     130 197

prc <- prcomp(tdMtx)
which.max(cumsum(prc$sdev ^ 2 / sum(prc$sdev ^ 2)) >= 0.8)
