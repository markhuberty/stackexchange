###############################
## test_map_meh.py
## Author: Mark Huberty
## Date begun: 8 April 2012
## Code to map from the question:skill table to users via the user:answer table
## Generates user:skill scores weighted by the skill content of the
## question to which the answer applies
###############################
import re
import csv
import os
from scipy.sparse import *
from scipy import *
import gc
import pickle
import numpy as np
import pandas as pds

#os.chdir('C:/Users/miaomiaocui/stackexchange/')
#os.chdir('/home/markhuberty/Documents/stackexchange')
os.chdir('/Users/markhuberty/Documents/Research/Papers/stackexchange')

question_answer_vote = pds.read_csv('./data/vote_counts_by_qid_aid_uid.csv')

with open('./data/question_skill_map.pickle', 'rb') as f:
    question_skill_map = pickle.load(f)


# Fix the dict encoding. This just collapses a list of dicts into
# a dict of dicts
question_skill_map_keys = [q.keys()[0] for q in question_skill_map] 
question_skill_map_new = {}
for i, k in enumerate(question_skill_map_keys):
    question_skill_map_new[k] = question_skill_map[i][k]
    
## Take question_answer_vote as pandas data frame
## question_skill is predefined by question
user_skill_map = {}
for i, q in enumerate(question_answer_vote['question_id']):
    if i % 10000 is 0:
        print i
        this_record = question_answer_vote.ix[i]
    this_user = this_record['answer_user_id']
    if not user_skill_map.has_key(this_user):
        user_skill_map[this_user] = {}            
    this_type = this_record['vote_type']
    this_vote = this_record['vote_count']
    if this_type == 1:
        continue # skip the accepted-vote stuff for now
    if this_type == 3:
        this_vote = this_vote * -1 # Invert for downvotes
    ## check to make sure questions are in the skill map
    if question_skill_map_new.has_key(q): 
        skills = question_skill_map_new[q]
    else:
        continue
    for s in skills.keys():
        weighted_vote = this_vote * skills[s]
        #print weighted_vote
        if s in user_skill_map[this_user].keys():
            user_skill_map[this_user][s] += weighted_vote
        else:
            user_skill_map[this_user][s] = weighted_vote
            
                   
# Write this out as userid:skillid:vote
conn = open('./data/user_skill_map.csv', 'wt')
writer = csv.writer(conn)
writer.writerow(['userid', 'skillid', 'skill_value'])
for u in user_skill_map.keys():
    record = user_skill_map[u]
    for k in record.keys():
        out = [u, k, record[k]]
        writer.writerow(out)
conn.close()
