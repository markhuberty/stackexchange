from scipy import *
from scipy.sparse import *
import csv
import gc
import re

## Taken from here: http://www.peterbe.com/plog/uniqifiers-benchmark
def uniquify(seq, idfun=None):  
    # order preserving 
    if idfun is None: 
        def idfun(x): return x 
    seen = {} 
    result = [] 
    for item in seq: 
        marker = idfun(item) 
        # in old Python versions: 
        # if seen.has_key(marker) 
        # but in new ones: 
        if marker in seen: continue 
        seen[marker] = 1 
        result.append(item) 
    return result

## Takes the vector of tags as a list of strings with regularized
## separators (as , for instance, wtih XML, (<tag1><tag2><tag3>), 
## parses the tags into a list of lists, 
## one for each entry in the vector, and then writes the result
## to a sparse matrix of dimension ROWS * COUNT(unique_tags)
def generate_sparse_tag_matrix(tag_vec, to_delete, to_split):
    """
    Given a list of tag strings, splits the string into
    component tags and writes out a sparse matrix of form
    list index * tag index
    """
    p_delete = re.compile(to_delete)
    p_split = re.compile(to_split)

    tag_temp = [p_delete.sub("", tag[0]) for tag in tag_vec]
    tag_list = [p_split.split(tag) for tag in tag_temp]
    
    all_tags = []
    for tag in tag_list:
        all_tags.extend(tag)

    ## Generate a list of unique tags
    unique_tags = uniquify(seq = all_tags)

    ## Declare three lists to hold the coordinates and value
    print 'Tags split and collected, writing indices'

    gc.disable()
    print 'Writing col coords'
    col_coord = [unique_tags.index(tag_sub) for 
                 tag_group in tag_list for 
                 tag_sub in tag_group]

    print 'Writing row coords'
    row_coord = [[i] * len(tag_list[i]) for i in range(len(tag_list))]
    row_coord = [item for sublist in row_coord for item in sublist]

    cell_value = [1] * len(col_coord)

    col_dim = len(unique_tags)
    row_dim = len(tag_list)

    print (col_dim)
    print(row_dim)

                
    print 'Indices written, generating sparse matrix'
    gc.enable()
    ## Declare and populate the sparse matrix
    row_coord = array( row_coord )
    col_coord = array( col_coord )
    mat_out = csc_matrix((array(cell_value), (row_coord, col_coord)),
                         shape=(row_dim, col_dim)
                         )

    dict_out = {'unique_tags': unique_tags, 
                'tag_matrix': mat_out
                }
    return(dict_out)

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

def compute_cooc_matrix(sparse_mat, tag_freqs):
    mat_mult = sparse_mat.transpose() * sparse_mat
    mat_mult = mat_mult.asfptype()

    ## Divide out the tag counts
    tag_frequencies = sparse_mat.sum(axis=0)
    row_indices, col_indices = tag_matrix_multiply.nonzero()

    for d in range(len(mat_mult.data)):
        mat_mult.data[d] = mat_mult.data[d] / tag_freqs[d]

    return mat_mult
