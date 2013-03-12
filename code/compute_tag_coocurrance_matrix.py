from scipy import *
from scipy.sparse import *
from stackexchange import *
import matplotlib as mpl
mpl.use('Agg') ## Allows mpl to function w/o active X session
import matplotlib.pyplot as plt
import networkx as nx
import numpy as np
import operator
import os
import pickle

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

## Calculate absolute term occurrance freq. as column sum of the 
## post-term matrix
tag_frequency = tag_matrix.sum(axis=0)
tag_frequency = [tag_frequency[0,i] for i in range(tag_frequency.shape[1])]
tag_frequency = np.array(tag_frequency)

## Take only the top tags based on median tag counts
bool_top_tags = tag_frequency >= np.median(tag_frequency)
idx_top_tags = [i for i,b in enumerate(bool_top_tags) if b]
print len(idx_top_tags)
tag_matrix = tag_matrix[:,idx_top_tags]
unique_tags = [unique_tags[i] for i in idx_top_tags]
tag_frequency = [tag_frequency[i] for i in idx_top_tags]

## Compute the co-occurrance matrix
cooc_matrix = compute_cooc_matrix(tag_matrix, tag_frequency)

## Write two variants on the matrix: the straight proximity matrix,
## and the format needed for the MCL algorithm
print 'Done with the proximity calculation'    
filename='../data/tag_cooc_matrix.pickle'
with open(filename, 'wt') as f:
    pickle.dump(cooc_matrix, f)


## Create the nx graph and add the top tags as nodes
g_tag = nx.Graph()

## Then add the edges as weights
## Takes the max values
## Add a small value to each edgeweight to make sure that even
## 0-proximity edges have some weight.
eps = 0.00001
edges = [(unique_tags[r], unique_tags[c], 1-tag_matrix_multiply[r,c] + eps)
         if tag_matrix_multiply[r,c] > tag_matrix_multiply[c,r] else
         (unique_tags[c], unique_tags[r], 1-tag_matrix_multiply[c,r] + eps)
         for r, c in zip(row_indices, col_indices) 
         ]
edgeweights = [e[2] for e in edges]
edgeweights_max = np.array(edgeweights).mean()

g_tag.add_weighted_edges_from(edges)

## Write out for mcl clustering
write_mcl_format(g_tag, '../data/g_tag_mcl.txt')

## Calculate betweenneess for node importance to the
## structure; then sort
tag_betweenness = nx.betweenness_centrality(g_tag)
btwn_sorted = sorted(tag_betweenness.iteritems(),
                     key=operator.itemgetter(1),
                     reverse=True
                     )
