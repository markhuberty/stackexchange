import scipy
import scipy.sparse
import networkx as nx

## Load the sparse matrix of COUNT(posts) * COUNT(unique tags)
filename = "../data/sparse_tag_matrix"

with open(filename, 'rb') as f:
    pickle.load(f) 

## Multiply the resulting matrix
## NOTE: need this as a unique_tags * unique_tags output
## Thus take the transpose--not looking for post proximity, looking
## for tag proximity
unique_tags = tag_object[0]
tag_matrix = tag_object[1]

tag_matrix_transpose = tag_matrix.transpose()

## Multiply. This should return a count(tags) * count(tags) matrix
## where the cells are counts of co-occurance between tags
tag_matrix_multiply = tag_matrix_transpose.multiply(tag_matrix_transpose).dense()

## Do row-wise divide by the column sums of the tag matrix
tag_matrix_sums = tag_matrix.sum(axis = 'col').dense()

## Divide out the totals for each tag to get the conditional probability
## Not sure the most efficient way to do this 20k+ rows/cols is probably 
## too big for a dense matrix
for i in range(len(tag_matrix_sums)):
    for j in shape(tag_matrix_multiply)[2]:
        tag_matrix_multiply[i][j] = tag_matrix_multiply[i][j] / tag_matrix_sums[i]


## Write the resulting matrix out as a sparse matrix


## Convert to non-sparse matrix


## Write out a non-sparse version for use elsewhere



## for networkx, add the nodes first, then edges

g_tag = nx.Graph()

g_tag.add_nodes_from(unique_tags)


