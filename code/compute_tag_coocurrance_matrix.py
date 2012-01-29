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
## Generate the co-occurrance matrix
## Multiply. This should return a count(tags) * count(tags) matrix
## where the cells are counts of co-occurance between tags
tag_matrix_multiply = tag_matrix.transpose() * tag_matrix
tag_matrix_multiply = tag_matrix_multiply.asfptype() 


## Divide out the colsums to get the conditional probability of 
## co-incidence
row_indices, col_indices = tag_matrix_multiply.nonzero()
tag_frequencies = [tag_frequency[i] for i in col_indices]


to_keep = [idx for idx, r in enumerate(row_indices) if row_indices[idx] != col_indices[idx]]
row_indices = [row_indices[idx] for idx in to_keep]
col_indices = [col_indices[idx] for idx in to_keep]


for d in range(len(tag_matrix_multiply.data)):
    tag_matrix_multiply.data[d] = tag_matrix_multiply.data[d] / tag_frequencies[d]

                                    
print 'Done with the proximity calculation'    

filename='../data/tag_proximity_csc.pickle'
with open(filename, 'wt') as f:
    pickle.dump(tag_matrix_multiply, f)

## Way too slow...
# ## Now take the pairwise minima
# prox_minima = []
# for i in range(len(unique_tags)):
#     for j in range(i+1, len(unique_tags)):
#         if i is not j:
#             val_a = tag_matrix_multiply[i,j]
#             val_b = tag_matrix_multiply[j,i]
#             if val_a > val_b:
#                 prox_minima.append((i,j, val_b))
#             else:
#                 prox_minima.append((j,i, val_a))
    

## Create the nx graph and add the top tags as nodes
g_tag = nx.Graph()

## Then add the edges as weights
## Takes the max values
edges = [(unique_tags[r], unique_tags[c], 1-tag_matrix_multiply[r,c])
         if tag_matrix_multiply[r,c] > tag_matrix_multiply[c,r] else
         (unique_tags[c], unique_tags[r], 1-tag_matrix_multiply[c,r])
         for r, c in zip(row_indices, col_indices) 
         ]
edgeweights = [e[2] for e in edges]
edgeweights_max = np.array(edgeweights).mean()

g_tag.add_weighted_edges_from(edges)

# g_tag_pos = nx.draw_spring(g_tag)
# plt.savefig('../figures/tag_association_tree.png')


## And generate the MST w/ Kruskal's alg
mst = nx.minimum_spanning_tree(g_tag)
## Here, want to do layout and then add back in some edges
## See the innovation_space code for details
## Size would be nice, too

## Write out the graph file
nx.write_gexf(mst, '../data/tag_mst_graph.gexf')

## Generate the graph position
prox_graph_layout = nx.graphviz_layout(mst, prog='neato')

## Add in extra edges and colors
supp_edgelist = [(e[0], e[1], e[2]) for e in edges if e[2] < 0.75]
mst.add_weighted_edges_from(supp_edgelist)
edge_weights = nx.get_edge_attributes(mst, 'weight')
edge_color = np.array([1-edge_weights[k] for k in edge_weights.keys()])
node_labels = {}
for n in mst.nodes():
    node_labels[n] = n

nx.draw_networkx_nodes(mst,
                       prox_graph_layout,
                       alpha=0.6,
                       node_size=4,
                       linewidths=0.2,
                       )
nx.draw_networkx_labels(mst,
                        prox_graph_layout,
                        labels=node_labels,
                        font_size=2
                        )
nx.draw_networkx_edges(mst,
                       prox_graph_layout,
                       linewidths=0.5,
                       edge_color=edge_color,
                       edge_cmap=plt.cm.get_cmap('BrBG'),
                       edge_vmin=edge_color.min(),
                       edge_vmax=edge_color.max()
                       )
plt.colorbar()
plt.savefig('../figures/tag_association_mst.pdf')
plt.close()

## Right now, this plots but is really very messed up
## Need to use graphviz instead to get the plotting right. 
## NEATO layout should work better

        
        

            


