# -*- coding: utf-8 -*-
"""
Created on Tue Mar 28 11:14:34 2017

@author: slj
"""
import sys
import os
import getopt
from datetime import datetime
import re
from tqdm import tqdm
from nltk import ngrams
from collections import Counter, defaultdict
from math import log

# stopwords_path = 'data/stop_words.utf8'
# in_encoding = 'utf-8'
def stopwordsCreate(stopwords_path):
    stopwords = []
    with open(stopwords_path, encoding = 'utf-8') as f:
        for line in f:
            stopwords.append(re.sub('\n', '', line))
    stopwords = stopwords[156:877]
    stopwords.extend(['分钟', '周', '小时', '天', '克', '次', '次分'])
    stopwords = '|'.join(stopwords)
    return stopwords
        
def entropyCaculate(vec):
    result = 0
    vecCnt = Counter(vec)
    total_value = sum(vecCnt.values())
    for value in vecCnt.values():
        p = value / total_value
        result += -p * log(p)
    return result

def dicCreate(words_original, term_max_len, thr_p, thr_f, thr_freq, thr_idf):
    words = ''.join(words_original)

    print(datetime.now().strftime('%H:%M:%S'), 'making n-gram words list')
    n_gram_list = []
    for i in range(1, term_max_len + 1):
        i_gram_list = [''.join(_) for _ in ngrams(words, i)]
        n_gram_list.extend(i_gram_list)

#    word_freq = {x[0]:x[1] for x in filter(lambda x:x[1] >= thr_freq, 
#                                             Counter(n_gram_list).items())}
    print(datetime.now().strftime('%H:%M:%S'), 'counting words frequency')
    word_freq = Counter(n_gram_list)
    total_value = sum(word_freq.values())
    for k,v in word_freq.items():
        word_freq[k] = v / total_value
    
    pmi = defaultdict(float)
    free = defaultdict(float)
    freq = defaultdict(float)
    idf = defaultdict(float)
    
    print(datetime.now().strftime('%H:%M:%S'), 'caculating pmi, free, freq, idf...')
    for w in tqdm(set(n_gram_list[len(words):])):
        
        # freq_temp = word_freq[w] * len(n_gram_list)
        freq_temp = word_freq[w] * total_value
        if freq_temp < thr_freq:
            continue
        
        idf_temp = log(len(words_original) / 
                       (sum([1 for _ in words_original if re.search(w, _) is not None]) + 1))
        if idf_temp >= thr_idf:
            continue
        
        left_free_vec = []
        right_free_vec = []
        for pos in re.finditer(w, words):
            if pos.start() == 0:
                left_free_vec.append('')
            else:
                left_free_vec.append(words[pos.start() - 1])
            
            if pos.end() == len(words):
                right_free_vec.append('')
            else:
                right_free_vec.append(words[pos.end()])
                
        free_temp = min(entropyCaculate(left_free_vec), 
            entropyCaculate(right_free_vec))
        if free_temp <= thr_f:
            continue
        
        max_substring_freq = 0
        for i in range(1, len(w)):
            temp = word_freq[w[:i]] * word_freq[w[i:]]
            if temp > max_substring_freq:
                max_substring_freq = temp
        pmi_temp = log(word_freq[w] / max_substring_freq)
        if pmi_temp <= thr_p:
            continue
        
        pmi[w] = pmi_temp
        free[w] = free_temp
        freq[w] = freq_temp
        idf[w] = idf_temp
        
    return pmi, free, freq, idf

#term_max_len = 5
#thr_freq = 1
#thr_idf = 99
#thr_p = 0
#thr_f = 0
#corpus_file = '../corpus/para.txt'
#user_dict = '../corpus/python_dict.utf8'
def userDictWrite(corpus_file, stopwords, term_max_len, 
                  thr_p, thr_f, thr_freq, thr_idf, user_dict, 
                  in_encode = 'utf-8'):
    print(datetime.now().strftime('%H:%M:%S'), 
          'reading corpus from ', corpus_file)
    
    words_original = []
    with open(corpus_file, encoding = in_encode) as f:
        for line in f:
            line = re.sub('\W', '', line)
            line = re.sub('[0-9]', '', line)
            line = re.sub(stopwords, '', line)
            words_original.append(line)
        
    pmi, free, freq, idf = dicCreate(words_original, term_max_len, thr_p, 
                                     thr_f, thr_freq, thr_idf)
    
    print(datetime.now().strftime('%H:%M:%S'), 
          'writing dictionary to', user_dict)
    with open(user_dict, 'w', encoding = 'utf-8') as f:
        f.writelines([(_ + '\n') 
                        for _ in sorted(freq.keys(), key = freq.get, reverse=True)])

def main():
    corpus_file = None
    user_dict = None
    stopwords_path = None
    term_max_len = 5
    thr_p = 0
    thr_f = 0
    thr_freq = 1
    thr_idf = 10
    in_encode = 'utf-8'
    try:
        opts, args = getopt.getopt(sys.argv[1:], 'hi:o:s:', 
                                   ['help', 'corpus_file=', 'user_dict=', 'stopwords=', 
                                    'term_max_len=', 'thr_p=', 'thr_f=', 'thr_freq=', 
                                    'thr_idf=', 'in_encode='])
    except getopt.GetoptError as err:
        print(err)
        sys.exit(2)
    for opt, arg in opts:
        if opt in ('-h', '--help'):
            print('dicCreatePy.py -i <corpus_file> -o <user_dict> -s <stopwords_path> \
              --term_max_len 5 --thr_p 0 --thr_f 0 --thr_freq 5 --thr_idf 5 --in_encode utf-8')   
            sys.exit()
        elif opt in ('-i', '--corpus_file'):
            corpus_file = arg
        elif opt in ('-o', '--user_dict'):
            user_dict = arg
        elif opt in ('-s', '--stopwords'):
            stopwords_path = arg
        elif opt == '--term_max_len':
            term_max_len = int(arg)
        elif opt == '--thr_p':
            thr_p = float(arg)
        elif opt == '--thr_f':
            thr_f = float(arg)
        elif opt == '--thr_freq':
            thr_freq = float(arg)
        elif opt == '--thr_idf':
            thr_idf = float(arg)
        elif opt == '--in_encode':
            in_encode = arg

    stopwords = stopwordsCreate(stopwords_path)
    userDictWrite(corpus_file, stopwords, term_max_len, 
                  thr_p, thr_f, thr_freq, thr_idf, user_dict, 
                  in_encode)

if __name__ == '__main__':
    # os.chdir('E:/R Project/dict/corpus/')
    main()


















