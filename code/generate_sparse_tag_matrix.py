from stackexchange import *
import MySQLdb
import os
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
tag_data = conn_cursor.fetchall()
conn_cursor.close()
conn.close()
## End data read-in


## Generate the sparse matrix and associated values

## Define the input values for the regexp
to_delete = "^[<]{1}|[>]{1}$"
to_split = "><"

print 'Data loaded, parsing tags to matrix'
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

    
                        

                        
