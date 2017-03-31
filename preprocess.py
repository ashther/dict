# -*- coding: utf-8 -*-
"""
Created on Thu Mar 23 16:52:06 2017

@author: slj
"""

# exec(open('./dataSource.py').read())
import os
import numpy as np
from keras.preprocessing.text import Tokenizer
from keras.preprocessing.sequence import pad_sequences
from keras.layers import Embedding, Conv1D, MaxPooling1D
from keras.layers import Dense, Input, Flatten
import keras.utils as np_utils
from keras.models import Model, model_from_json


# read data
text_path = './20_newsgroup/'
text = []
label_index = {}
labels = []

for name in os.listdir(text_path):
    path = os.path.join(text_path, name)
    if os.path.isdir(path):
        label_id = len(label_index)
        label_index[name] = label_id
        for fname in os.listdir(path):
            if fname.isdigit():
                f = open(os.path.join(path, fname), 'r', encoding = 'latin-1')
                text.append(f.read())
                f.close()
                labels.append(label_id)

print("found %d texts." %len(text))

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

embedding_index = {}
f = open('../word2vec/vec.txt')
for line in f:
    values = line.split()
    # word = values[0]
    embedding_index[values[0]] = np.asarray(values[1:], dtype = 'float32')
f.close()
print("found %d word vectors" %len(embedding_index))

embedding_matrix = np.zeros((len(word_index), 100))
for word, i in word_index.items():
    word_vector = embedding_index.get(word)
    if word_vector is not None:
        embedding_matrix[i] = word_vector

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
preds = Dense(20, activation='softmax')(x)

model = Model(sequences_input, preds)
model.compile(loss='categorical_crossentropy', 
              optimizer = 'rmsprop', 
              metrics=['acc'])
model.fit(x_train, y_train, validation_data=(x_val, y_val), 
          epochs=10, batch_size=128)

model_json = model.to_json()
f = open('model.json', 'w')
f.write(model_json)
f.close()
model.save_weights('model.h5')

#load model from json file

#f = open('model.json', 'r')
#model = model_from_json(f.read())
#f.close()
#model.load_weights('model.h5')
#model.compile(loss='categorical_crossentropy', 
#              optimizer = 'rmsprop', 
#              metrics=['acc'])
#model.evaluate(x_val, y_val)

















