## Alternative to R file of the same name
## NOTE: this doesn't really work well right now
## Needs to use scipy to build a sparse matrix and do 
## algebra on it. 
## Also need to change the SQL query to extract only the tags 
## that aren't blank. 
import MySQLdb
import re
import csv
import os
from scipy.sparse import *
from scipy import *
import gc
import pickle

#os.chdir("../data/")

## Read in the tag vector from the database
conn = MySQLdb.connect(host="localhost",
                       user="markhuberty",
                       passwd="overflowpwd",
                       db="stackoverflow"
                       )

conn_cursor = conn.cursor()
conn_cursor.execute("SELECT TAGS FROM posts WHERE tags != \"\"")

## Note tag_data here is a tuple of strings
tag_data = conn_cursor.fetchall()

conn_cursor.close()
conn.close()
## End data read-in

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


## Generate the sparse matrix and associated values

## Define the input values for the regexp
to_delete = "^[<]{1}|[>]{1}$"
to_split = "><"

print 'Data loaded, initiating function'
tag_object = generate_sparse_tag_matrix(tag_vec = tag_data, 
                                        to_delete = to_delete, 
                                        to_split = to_split
                                        )
 
print 'Function done, writing out'
## Write the file out
## Pickle dump here or similar
filename = '../data/sparse_tag_matrix.pickle'
with open(filename, 'wt') as f:
    pickle.dump(tag_object, f)

    
                        

                        
