## Alternative to R file of the same name

import MySQLdb
import re
import csv
import os

#os.chdir("../data/")

## Read in the tag vector from the database
conn = MySQLdb.connect(host="localhost",
                       user="markhuberty",
                       passwd="overflowpwd",
                       db="stackoverflow"
                       )

conn_cursor = conn.cursor()
conn_cursor.execute("SELECT TAGS FROM posts")

## Note tag_data here is a tuple of strings
tag_data = conn_cursor.fetchall()

conn_cursor.close()
conn.close()
## End data read-in

# ## Begin parse
p_delete = re.compile("^[<]{1}|[>]{1}$")
p_split = re.compile("><")

tag_list = []
all_tags = []
for tag in tag_data:
    tag_temp = p_delete.sub("", tag[0])
    tag_split = p_split.split(tag_temp)
    tag_list.append(tag_split)
    if tag_split[0] != '':
        all_tags.extend(tag_split)

# del tag_data

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

unique_tags = uniquify(seq = all_tags)

sparse_tag_array = [unique_tags]

for tag_group in tag_list:
    #temp_vec = [0] * len(unique_tags)
    for tag in tag_group:
        if tag != '':
            #temp_vec.insert(unique_tags.index(tag), 1)
            unique_tags.index(tag)
    #sparse_tag_array.append(temp_vec)





## 
def generate_sparse_tag_matrix(tag_vec, to_delete, to_split, filename):
    tag_list = []
    all_tags = []

    p_delete = re.compile(to_delete)
    p_split = re.compile(to_split)
    
    ## Get the unique tag set
    for tag in tag_vec:        
        tag_temp = p_delete.sub("", tag[0])
        tag_split = p_split.split(tag_temp)
        tag_list.append(tag_split)
        if tag_split[0] != '':
            all_tags.extend(tag_temp)

    unique_tags = uniquify(seq = all_tags)

    ## parse the tag list into an indicator matrix
    sparse_tag_array = [unique_tags]

    with open(filename), 'wt') as f:
        writer = csv.writer(f)
        for tag_group in tag_list:
            temp_vec = [0] * len(unique_tags)
            for tag in tag_group:
                if tag != '':
                    temp_vec.insert(unique_tags.index(tag), 1)
            writer.writerow(temp_vec)

    return('Done')


## Generate the resulting csv file

to_delete = "^[<]{1}|[>]{1}$"
to_split = "><"
filename = '../data/sparse_tag_matrix.csv'
mat_out = generate_sparse_tag_matrix(tag_data, to_delete, to_split)

# with open('sparse_tag_matrix.csv', 'wt') as f:
#     writer = csv.writer(f)
#     for row in mat_out:
#         writer.writerow(row)


    
                        

                        
