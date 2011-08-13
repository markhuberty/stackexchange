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
import scipy
import scipy.sparse
import gc

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
    tag_list = []
    all_tags = []

    p_delete = re.compile(to_delete)
    p_split = re.compile(to_split)
    
    ## Parse each set of tags into a list
    ## as in <tag1><tag2><tag3> -> [tag1, tag2, tag3]
    for tag in tag_vec:        
        tag_temp = p_delete.sub("", tag[0])
        tag_split = p_split.split(tag_temp)
        tag_list.append(tag_split)
        if tag_split[0] != '':
            all_tags.extend(tag_split)

    ## Generate a list of unique tags
    unique_tags = uniquify(seq = all_tags)

    ## Declare three lists to hold the coordinates and value
    print 'Tags split and collected, writing indices'
    row_coord = []
    col_coord = []
    cell_value = []
    col_dim = len(unique_tags)
    row_dim = 0

    ## Loop down the list of tag lists
    ## and write their indices based on the 
    ## indices in the unique_tags vector
    ## NOTE: empty records have 0 entries for the entire row
    gc.disable()
    for tag_group in tag_list:
        if tag_group[0] != '':
            row_coord.append(tag_list.index(tag_group) * len(tag_group))
            col_coord.append(map(unique_tags.index, list(tag_group)))
            cell_value.append([1] * len(tag_group))
            row_dim += 1
            if (row_dim % 10000) == 0:
                print row_dim
                print 'Row coord length' + len(row_coord)
                print 'Col coord length' + len(col_coord)

        # for tag in tag_group:
        #     if tag != '':
        #         row_coord.append(tag_list.index(tag_group))
        #         col_coord.append(unique_tags.index(tag))
        #         cell_value.append(1)
                
                
    print 'Indices written, generating sparse matrix'
    gc.enable()
    ## Declare and populate the sparse matrix
    mat_out = csc_matrix(array(cell_value),
                         (array(row_coord), array(col_coord)),
                         shape=(row_dim, col_dim)
                         )

    list_out = [unique_tags, mat_out]
    return(list_out)


## Generate the sparse matrix
to_delete = "^[<]{1}|[>]{1}$"
to_split = "><"
filename = '../data/sparse_tag_matrix'

print 'Data loaded, initiating function'
tag_object = generate_sparse_tag_matrix(tag_vec = tag_data, 
                                        to_delete = to_delete, 
                                        to_split = to_split
                                        )
 
print 'Function done, writing out'
## Write the file out
## Pickle dump here or similar
with open(filename, 'wt') as f:
    pickle.dump(tag_object, f)

    
                        

                        
