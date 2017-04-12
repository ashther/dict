# dict
## 0_baikeCrawl.R
针对http://baike.baidu.com/wikitag/taglist?tagId=75953 页面的爬虫脚本
```r
Rscript 0_baikeCrawl.R --n_urls 25 --n_scroll_down 1 --max_sleep_time 0.2 --baike_all_path data/baike_all.rda --baike_para_path data/baike_para.txt --pjs_path phantomjs-2.1.1-windows/bin/phantomjs.exe
```
* `n_urls`:需要抓取的疾病条目数
* `n_scroll_down`:页面向下滚动次数（防止页面总条目数小于n_url时无法停止循环）
* `max_sleep_time`:抓取休眠间隔的最大时长，实际值为0到max_sleep_time之间的随机数
* `baike_all_path`:存储包括疾病所属科室、病因、症状、描述等字段的数据框文件地址
* `baike_para_path`:只存储用于后续分析的描述字段的文件地址
* `pjs_path`:phantomjs地址

## 1_dicCreatePy.py
新词发现算法
```r
python 1_dicCreatePy.py -i data/para.txt -o data/python_dict.utf8 -s data/stop_words.utf8 --term_max_len 5 --thr_p 0 --thr_f 0 --thr_freq 5 --thr_idf 5 --in_encode utf8
```
* `-i`:用于新词发现的语料
* `-o`:新词词典（也是用于分词的用户自定义词典）
* `-s`:停止词词典
* `--term_max_len`:最大词长，大于该值得新词无法被算法发现
* `--thr_p`:互信息值阈值，必须大于0
* `--thr_f`:邻词自由度值，应大于0
* `--thr_freq`:词频阈值
* `--thr_idf`:逆文档词频阈值
* `--in_encode`:语料文件的编码

## 2_segment.R
合并用户自定义词典和细胞词库，并进行分词，如果提供了人工标注好的分词文件，可以计算准确率和召回率
```r
Rscript 2_segment.R --user_dict dict/user_dict_complaint_all.utf8 --scels_path dict/ICD-10_1.scel, dict/ICD10.scel --mix_dict dict/user_dict_StatAndScel.utf8 --stopwords_path data/complaint_all.txt --labels_path data/complaint_all_label.utf8 --out_path data/segment.txt
```
* `--user_dict`:用户自定义词典（通过新词发现算法创建的词典）
* `--scels_path`:细胞词库文件地址，可提供多个，用逗号隔开
* `--mix_dict`:合并用户自定义词典和细胞词库后产生的新词典的输出地址
* `--stopwords_path`:停止词词典文件地址
* `--words_path`:用于分词的语料
* `--labels_path`:人工标注好的分词文件地址，可不提供
* `--out_path`:分词文件输出地址

## 3_word2vecPy.py
创建词向量
```r
python 3_word2vecPy.py -i data/segment.txt -o data/word2vec.model
```
* `-i`:经过分词后的语料文件，词之间空格隔开
* `-o`:词向量模型文件输出地址

## 4_cnn.py
进行卷积神经网络训练
```r
python 4_cnn.py -t data/segment.txt -l data/labels.txt -v data/word2vec.model -m data/model.json -w data/model.h5
```
* `-t`:用于训练的语料，已经经过分词，词之间用空格隔开
* `-l`:用于训练的语料分类标签
* `-v`:用于训练的词向量模型文件
* `-m`:训练好的模型输出地址
* `-w`:训练好的模型参数输出地址
