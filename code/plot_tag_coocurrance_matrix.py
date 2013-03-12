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
filename = "../data/tag_cooc_matrix.pickle"

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
row_indices, col_indices = tag_matrix.nonzero()
eps = 0.00001
edges = [(unique_tags[r], unique_tags[c], 1-tag_matrix[r,c] + eps)
         if tag_matrix[r,c] > tag_matrix[c,r] else
         (unique_tags[c], unique_tags[r], 1-tag_matrix[c,r] + eps)
         for r, c in zip(row_indices, col_indices) 
         ]
edgeweights = [e[2] for e in edges]

g_tag.add_weighted_edges_from(edges)


## Write out the betweeness data to a csv file
# conn = open('../data/tag_betweenness.csv', 'wt')
# writer = csv.writer(conn)
# writer.writerow(['node', 'btwnness'])
# for k in btwn_sorted.keys():
#     writer.writerow([k, btwn_sorted[k]])
# conn.close()

## And generate the MST w/ Kruskal's alg
mst = nx.minimum_spanning_tree(g_tag)

## Add in extra edges and colors
supp_edgelist = [(e[0], e[1], e[2]) for e in edges if e[2] < 0.5]
mst.add_weighted_edges_from(supp_edgelist)

## Calculate betweenneess for node importance to the
## structure; then sort
tag_degree = nx.degree_centrality(mst)
btwn_sorted = sorted(tag_degree.iteritems(),
                     key=operator.itemgetter(1),
                     reverse=True
                     )


## Color edges so that more proximate (closer) values are more intense
edge_weights = nx.get_edge_attributes(mst, 'weight')
edge_color = np.array([1-edge_weights[k] for k in edge_weights.keys()])

## Set node size and label
## First load up the cluster membership
## Load up the MST clusters
mst_filename = '../data/mcl_out/cluster_labels_mst_I50'
with open(mst_filename, 'rt') as f:
    mst_clusters = [re.sub('\n', '', line).split('\t') for line in f]

## Now for each cluster, find the tag with the largest betweenness
## And label it; ignore the rest of the labels
core_tags = set()
for cluster in mst_clusters:
    btwn_vec = [tag_degree[n] for n in cluster]
    idx_max = np.argmax(btwn_vec)
    tag_max = cluster[idx_max]#, btwn_vec[idx_max])
    core_tags.add(tag_max)

degree_median = np.max([tag_degree[n] for n in tag_degree])
node_labels = {}
for n in mst.nodes():
    if n in core_tags and tag_degree[n] > 0.003:
        node_labels[n] = n
    else:
        node_labels[n] = ''

## Set node size proportionate to betweenness:
nodesize = [tag_degree[n] * 100 for n in mst.nodes()]

    
prox_graph_full_layout = nx.graphviz_layout(mst, prog='sfdp')

nx.draw_networkx_edges(mst,
                       prox_graph_full_layout,
                       width=0.1,
                       alpha=0.5,
                       edge_color=edge_color,
                       edge_cmap=plt.cm.get_cmap('Blues'),
                       edge_vmin=edge_color.min(),
                       edge_vmax=edge_color.max()
                       )
plt.colorbar()
nx.draw_networkx_nodes(mst,
                       prox_graph_full_layout,
                       alpha=0.6,
                       node_size=nodesize,
                       linewidths=0.1,
                       )
nx.draw_networkx_labels(mst,
                        prox_graph_full_layout,
                        labels=node_labels,
                        font_size=4,
                        font_color='red',
                        font_weight='bold'
                        )
plt.savefig('../figures/tag_association_tree.pdf')
plt.close()

        
        

            


