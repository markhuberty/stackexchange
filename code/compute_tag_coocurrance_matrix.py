from scipy import *
from scipy.sparse import *
import numpy as np
import networkx as nx
import os
import pickle
import matplotlib.pyplot as plt

## Load the sparse matrix of COUNT(posts) * COUNT(unique tags)
filename = "../data/sparse_tag_matrix.pickle"

with open(filename, 'rb') as f:
    tag_object = pickle.load(f) 

f.close()

## Multiply the resulting matrix
## NOTE: need this as a unique_tags * unique_tags output
## Thus take the transpose--not looking for post proximity, looking
## for tag proximity
unique_tags = tag_object['unique_tags']
tag_matrix = tag_object['tag_matrix']

tag_frequency = [tag_matrix[:,j].sum() for j in range(tag_matrix.get_shape()[1])]

tag_frequency = list(tag_frequency)
tag_frequency_pct = [float(freq) / tag_matrix.get_shape()[0] for freq in 
                     tag_frequency]


## Multiply. This should return a count(tags) * count(tags) matrix
## where the cells are counts of co-occurance between tags
tag_matrix_transpose = tag_matrix.transpose()
tag_matrix_multiply = tag_matrix_transpose * tag_matrix
tag_matrix_multiply = tag_matrix_multiply.asfptype()

tag_frequency_np = tag_matrix_transpose.sum(axis=1)


## Divide out the colsums to get the conditional probability of 
## co-incidence
row_indices, col_indices = tag_matrix_multiply.nonzero()
tag_frequencies = tag_frequency_np[col_indices]

for d in range(len(tag_matrix_multiply.data)):
    tag_matrix_multiply.data[d] = tag_matrix_multiply.data[d] / tag_frequencies[d]
                                        
filename = '../data/tag_proximity_matrix_full.pickle'
with open(filename, 'wt') as f:
    pickle.dump(tag_matrix_multiply, f)

## Then render the matrix much smaller by dumping nodes not in 
## the top 1000 tags

tags_freqs = zip(tag_frequency, unique_tags)
tags_freqs.sort(reverse = True)
freqs, tags = zip(*tags_freqs)

top_tags = tags[1:1000]
tag_indices = [unique_tags.index(tag) for tag in top_tags]

## Create the nx graph and add the top tags as nodes
g_tag = nx.Graph()

## Then add the edges as weights
## Get values below threshold
threshold = 0.7
edges = [(unique_tags[r], unique_tags[c], 1-tag_matrix_multiply[r,c])
         for r, c in zip(row_indices, col_indices) 
         if tag_matrix_multiply[r,c] >= threshold
         ]

g_tag.add_weighted_edges_from(edges)    


# g_tag_pos = nx.draw_spring(g_tag)
# plt.savefig('../figures/tag_association_tree.png')


## And generate the MST w/ Kruskal's alg
mst = list(nx.minimum_spanning_tree(g_tag))
mst_graph = nx.Graph()
mst_graph.add_weighted_edges_from(mst)

mst_graph_pos = nx.draw_spring(mst_graph)
plt.savefig('../figures/tag_association_mst.png')



