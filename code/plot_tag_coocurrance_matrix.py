from scipy import *
from scipy.sparse import *
import numpy as np
import networkx as nx
import os
import pickle
import matplotlib as mpl
mpl.use('Agg') ## Allows mpl to function w/o active X session
import matplotlib.pyplot as plt
import operator

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



## Create the nx graph and add the top tags as nodes
g_tag = nx.Graph()

## Then add the edges as weights
## Note that the matrix is square but not symmetric. So
## we take the maximum of the two coocurrance frequencies.
## Add a small value to each edgeweight to make sure that even
## 0-proximity edges have some weight.
eps = 0.00001
edges = [(unique_tags[r], unique_tags[c], 1-tag_matrix[r,c] + eps)
         if tag_matrix[r,c] > tag_matrix[c,r] else
         (unique_tags[c], unique_tags[r], 1-tag_matrix[c,r] + eps)
         for r, c in zip(row_indices, col_indices) 
         ]
edgeweights = [e[2] for e in edges]

g_tag.add_weighted_edges_from(edges)

## Calculate betweenneess for node importance to the
## structure; then sort

## And generate the MST w/ Kruskal's alg
mst = nx.minimum_spanning_tree(g_tag)

## Add in extra edges and colors
supp_edgelist = [(e[0], e[1], e[2]) for e in edges if e[2] < 0.5]
mst.add_weighted_edges_from(supp_edgelist)

tag_betweenness = nx.betweenness_centrality(mst)
btwn_sorted = sorted(tag_betweenness.iteritems(),
                     key=operator.itemgetter(1),
                     reverse=True
                     )


## Color edges so that more proximate (closer) values are more intense
edge_weights = nx.get_edge_attributes(mst, 'weight')
edge_color = np.array([1-edge_weights[k] for k in edge_weights.keys()])

## Set node size and label
## First load up the cluster membership
## Load up the MST clusters
with open(mst_filename, 'rt') as f:
    mst_clusters = [re.sub('\n', '', line).split(' ') for line in f]

## Now for each cluster, find the tag with the largest betweenness
## And label it; ignore the rest of the labels
core_tags = set()
for cluster in mst_clusters:
    btwn_vec = [tag_betweenness[n] for n in cluster]
    idx_max = np.argmax(btwn_vec)
    tag_max = cluster[idx_max]#, btwn_vec[idx_max])
    set.add(tag_max)

node_labels = {}
for n in mst.nodes():
    if n in core_tags:
        node_labels[n] = n
    else:
        node_labels[n] = ''

## Set node size proportionate to betweenness:
nodesize = [tag_betweenness[n] * 10 for n in mst.nodes()]

    
prox_graph_full_layout = nx.graphviz_layout(mst, prog='sfdp')

nx.draw_networkx_nodes(mst,
                       prox_graph_full_layout,
                       alpha=0.6,
                       node_size=nodesize,
                       linewidths=0.1,
                       )
nx.draw_networkx_labels(mst,
                        prox_graph_full_layout,
                        labels=node_labels,
                        font_size=2,
                        font_color='red',
                        font_weight='bold'
                        )
nx.draw_networkx_edges(mst,
                       prox_graph_full_layout,
                       linewidths=0.1,
                       edge_color=edge_color,
                       edge_cmap=plt.cm.get_cmap('Blues'),
                       edge_vmin=edge_color.min(),
                       edge_vmax=edge_color.max()
                       )
plt.colorbar()
plt.savefig('../figures/tag_association_tree.pdf')
plt.close()

        
        

            


