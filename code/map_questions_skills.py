## Alternative to R file of the same name
## NOTE: this doesn't really work well right now
## Needs to use scipy to build a sparse matrix and do 
## algebra on it. 
## Also need to change the SQL query to extract only the tags 
## that aren't blank. 
import re
import csv
import os
from scipy.sparse import *
from scipy import *
import gc
import pickle

os.chdir('/home/markhuberty/Documents/stackexchange/')

f = open('./data/sparse_tag_matrix.pickle', 'rb')
tag_mat = pickle.load(f)
f.close()

f = open('./data/mcl_out/cluster_labels_mst_I50', 'rt')
skill_cat = [re.split('\t', re.sub('\n', '', row)) for row in f]
f.close()

def map_q_skill(questions, tags, skills):
    q_map = []
    for q in range(questions.shape[0]):
        if q % 1000 == 0:
            print q
        this_s = []
        this_s_prop = []
        tag_idx = questions.getrow(q).indices
        #print tag_idx
        q_tags = [tags[j] for j in tag_idx]
        #print q_tags
        for i, s in enumerate(skills):
            #print i
            temp = list(set(q_tags) & set(s))
            prop_s = len(temp) /x float(len(q_tags))
            if prop_s > 0:
                this_s.append(i)
                this_s_prop.append(prop_s)
            if sum(this_s_prop) == 1:
                dict_out = dict(zip(this_s, this_s_prop))
                q_map.append(dict_out)
                break
    return(q_map)

tag_mat_csr = tag_mat['tag_matrix'].tocsr()
unique_tags = tag_mat['unique_tags']

question_skill_map = map_q_skill(tag_mat_csr, unique_tags, skill_cat)

filename = './data/question_skill_map.pickle'
with open(filename, 'wt') as f:
    pickle.dump(question_skill_map, f)
