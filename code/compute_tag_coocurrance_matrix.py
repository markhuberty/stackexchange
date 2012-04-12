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
## co-incidence. First strip out the diagonal.
to_keep = [idx for idx, r in enumerate(row_indices) if row_indices[idx] != col_indices[idx]]
row_indices = [row_indices[idx] for idx in to_keep]
col_indices = [col_indices[idx] for idx in to_keep]
row_indices, col_indices = tag_matrix_multiply.nonzero()
tag_frequencies = [tag_frequency[i] for i in col_indices]


for d in range(len(tag_matrix_multiply.data)):
    tag_matrix_multiply.data[d] = tag_matrix_multiply.data[d] / tag_frequencies[d]

                                    
print 'Done with the proximity calculation'    

filename='../data/tag_proximity_csc.pickle'
with open(filename, 'wt') as f:
    pickle.dump(tag_matrix_multiply, f)


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

    
def write_mcl_format(g, filename):
    """
    Takes as input a networkx graph g and a valid filepath as a string. Writes
    the graph edgelist with weights to a text file of format E1 E2 W,
    consistent with the file format required by the mcl algorithm.

    See http://www.micans.org/mcl/ for more detail on mcl.
    """    
    edges = g.edges()
    weights = nx.get_edge_attributes(g, 'weight')
    conn = open(filename, 'wt')
    ## Note the weight inversion here; again, 
    for i, e in enumerate(edges):
        out = map(lambda(x): str(x), [e[0], e[1], 1-weights[e]])
        conn.write(' '.join(out))
        conn.write('\n')
    conn.close()
    return 'Done'

write_mcl_format(g_tag, '../data/g_tag_mcl.txt')

tag_betweenness = nx.betweenness_centrality(g_tag)

## And generate the MST w/ Kruskal's alg
mst = nx.minimum_spanning_tree(g_tag)

## Here, want to do layout and then add back in some edges
## See the innovation_space code for details
## Size would be nice, too

## Add in extra edges and colors
supp_edgelist = [(e[0], e[1], e[2]) for e in edges if e[2] < 0.5]
mst.add_weighted_edges_from(supp_edgelist)

write_mcl_format(mst, '../data/g_tag_mst_mcl.txt')

edge_weights = nx.get_edge_attributes(mst, 'weight')
edge_color = np.array([1-edge_weights[k] for k in edge_weights.keys()])
node_labels = {}
for n in mst.nodes():
    node_labels[n] = n

prox_graph_full_layout = nx.graphviz_layout(mst, prog='neato')

nx.draw_networkx_nodes(mst,
                       prox_graph_full_layout,
                       alpha=0.6,
                       node_size=4,
                       linewidths=0.2,
                       )
nx.draw_networkx_labels(mst,
                        prox_graph_full_layout,
                        labels=node_labels,
                        font_size=0.5
                        )
nx.draw_networkx_edges(mst,
                       prox_graph_full_layout,
                       linewidths=0.5,
                       edge_color=edge_color,
                       edge_cmap=plt.cm.get_cmap('BrBG'),
                       edge_vmin=edge_color.min(),
                       edge_vmax=edge_color.max()
                       )
plt.colorbar()
plt.savefig('../figures/tag_association_mst_full.pdf')
plt.close()

## Right now, this plots but is really very messed up
## Need to use graphviz instead to get the plotting right. 
## NEATO layout should work better

        
        

            


