# -*- coding: utf-8 -*-
"""
Created on Fri Mar 31 15:42:21 2017

@author: slj
"""

import sys
import os
import re
import getopt
import numpy as np
from keras.preprocessing.text import Tokenizer
from keras.preprocessing.sequence import pad_sequences
from keras.layers import Embedding, Conv1D, MaxPooling1D
from keras.layers import Dense, Input, Flatten
import keras.utils as np_utils
from keras.models import Model, model_from_json
from gensim.models import Word2Vec

def readText(in_text, in_label):
    text = []
    labels = []
    with open(in_text, encoding='utf-8') as f:
        for line in f:
            text.append(re.sub('\n', '', line))
    
    with open(in_label, encoding='utf-8') as f:
        for line in f:
            labels.append(int(re.sub('\n', '', line)))
    return text, labels

def textPreprocess(text, labels):
    # tokenizer and get word_index, sequences
    tokenizer = Tokenizer(num_words=20000)
    tokenizer.fit_on_texts(text)
    sequences = tokenizer.texts_to_sequences(text)
    
    word_index = tokenizer.word_index
    print("found %d unique words." %len(word_index))
    
    data = pad_sequences(sequences, 1000)
    labels = np_utils.to_categorical(np.asarray(labels))
    print("shape of data tensor: ", data.shape)
    print("shape of labels tensor: ", labels.shape)
    
    # get train and test data
    ind = np.arange(data.shape[0])
    np.random.shuffle(ind)
    data = data[ind]
    labels = labels[ind]
    
    validation_number = int(0.2 * data.shape[0])
    
    x_train = data[:-validation_number]
    y_train = labels[:-validation_number]
    x_val = data[-validation_number:]
    y_val = labels[-validation_number:]
    
    return x_train, y_train, x_val, y_val, word_index
    
def train(word_vec_model_path, x_train, y_train, x_val, y_val, word_index, 
          out_model, out_weight):
#    embedding_index = {}
    word_vec_model = Word2Vec.load(word_vec_model_path)
#    for w in word_index.keys():
#        embedding_index[w] = np.asarray(word_vec_model[w], dtype = 'float32')
#    print("found %d word vectors" %len(embedding_index))
    
    embedding_matrix = np.zeros((len(word_index), 100))
    for word, i in word_index.items():
        try:
            word_vector = word_vec_model[word]
            embedding_matrix[i] = word_vector
        except:
            pass
    
    embedding_layer = Embedding(len(word_index), 100, 
                                weights = [embedding_matrix], 
                                input_length = 1000, 
                                trainable = False)
    
    sequences_input = Input(shape = (1000, ), dtype = 'int32')
    embedding_sequence = embedding_layer(sequences_input)
    x = Conv1D(128, 5, activation='relu')(embedding_sequence)
    x = MaxPooling1D(5)(x)
    x = Conv1D(128, 5, activation='relu')(x)
    x = MaxPooling1D(5)(x)
    x = Conv1D(128, 5, activation='relu')(x)
    x = MaxPooling1D(35)(x)
    x = Flatten()(x)
    x = Dense(128, activation='relu')(x)
    preds = Dense(21, activation='softmax')(x)
    
    model = Model(sequences_input, preds)
    model.compile(loss='categorical_crossentropy', 
                  optimizer = 'rmsprop', 
                  metrics=['acc'])
    model.fit(x_train, y_train, validation_data=(x_val, y_val), 
              epochs=10, batch_size=128)
    
#    out_model = 'model.json'
#    out_weight = 'model.h5'
    model_json = model.to_json()
    f = open(out_model, 'w')
    f.write(model_json)
    f.close()
    model.save_weights(out_weight)
    
def main(in_text, in_label, word_vec_model_path, out_model, out_weight):
    text, label = readText(in_text, in_label)
    x_train, y_train, x_val, y_val, word_index = textPreprocess(text, label)
    train(word_vec_model_path, x_train, y_train, x_val, y_val, word_index, 
          out_model, out_weight)


    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    