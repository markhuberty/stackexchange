## Code to generate the postid:tagid mapping given the set of
## unique tags as constructed in the sparse tag matrix
## Begun 9 March 2012
## Mark Huberty


import csv
import os
from scipy.sparse import *
from scipy import *
import gc
import pickle
import MySQLdb

os.chdir('/home/markhuberty/Documents/stackexchange/data')

tag_object = pickle.load(open('sparse_tag_matrix.pickle', 'rb'))

unique_tags = tag_object['unique_tags']
tag_matrix = tag_object['tag_matrix']

conn = MySQLdb.connect(host="localhost",
                       user="markhuberty",
                       passwd="overflowpwd",
                       db="stackoverflow"
                       )

conn_cursor = conn.cursor()
conn_cursor.execute("SELECT ID FROM posts WHERE tags != \"\"")

## Note tag_data here is a tuple of strings
postid = conn_cursor.fetchall()
postid = list(postid)

conn_cursor.close()
conn.close()

tag_list_out = [{'id': i, 'tag': t} for i, t in enumerate(unique_tags)]

indices = tag_matrix.indices
indptr = tag_matrix.indptr
post_tag_out = []
for i, t in enumerate(unique_tags):
    id_idx = indices[indptr[i]:indptr[i+1]]
    id_post = [postid[j][0] for j in id_idx]
    out = zip(id_post, [i] * len(id_post))
    post_tag_out.extend(out)


f = open('tagid_tag_table.csv', 'wt')
writer = csv.DictWriter(f, fieldnames=['id', 'tag'])
for t in tag_list_out:
    writer.writerow(t)
f.close()

f = open('postid_tagid_table.csv', 'wt')
writer = csv.writer(f)
for p in post_tag_out:
    writer.writerow(p)
f.close()
