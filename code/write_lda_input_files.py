from scipy import *
from scipy.sparse import *
import numpy as np
import networkx as nx
import os
import pickle
import matplotlib as mpl
mpl.use('Agg') ## Allows mpl to function w/o active X session
import matplotlib.pyplot as plt

os.chdir('/home/markhuberty/Documents/stackexchange/code/')
## Load the sparse matrix of COUNT(posts) * COUNT(unique tags)
filename = "../data/sparse_tag_matrix.pickle"

f = open(filename, 'rb')

tag_object = pickle.load(f) 

f.close()

## Multiply the resulting matrix
## NOTE: need this as a unique_tags * unique_tags output
## Thus take the transpose--not looking for post proximity, looking
## for tag proximity
unique_tags = tag_object['unique_tags']
tag_matrix = tag_object['tag_matrix']

doc_word_counts = tag_matrix.sum(axis=1)

tag_matrix = tag_matrix.tocsr()
conn = open('../data/lda_tag_input_file.txt', 'wt')

for i in range(tag_matrix.shape[0]):
    string_out = str(doc_word_counts[i,0])
    row = tag_matrix.getrow(i)
    idx = row.indices
    data = row.data
    term_list = [':'.join(map(str, t)) for t in zip(idx, data)]
    str_list = ' '.join(term_list)
    string_out += ' ' + str_list
    conn.write(string_out)
    conn.write('\n')


conn.close()
