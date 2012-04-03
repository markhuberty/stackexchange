import re
import csv
import os
from scipy.sparse import *
from scipy import *
import gc
import pickle
import MySQLdb
import numpy as np
import pandas as pds

#os.chdir('C:/Users/miaomiaocui/stackexchange/')
os.chdir('/home/markhuberty/Documents/stackexchange')

question_answer_vote = pds.read_csv('./data/vote_counts_by_qid_aid_uid.csv')

with open('./data/question_skill_map.pickle', 'rb') as f:
    question_skill_map = pickle.load(f)

conn = MySQLdb.connect(host="localhost",
                       user="markhuberty",
                       passwd="overflowpwd",
                       db="stackoverflow"
                       )

conn_cursor = conn.cursor()
conn_cursor.execute("SELECT ID FROM posts WHERE TAGS !=\"\"")

qid = conn_cursor.fetchall()



# pseudocode
# get the actual questionids from the table
# for row in question:answer:user:vote table
# get skill dict for question q
# for answers to q:
# for answer_user: votes * skill weight
# assign to up or down in that skill category

# for i, q in enumerate(questionids):
# Get the skill dict for that questionid
# calculate a skill:votes dict for
# output: dict of userid:{skillid:[upvote, downvote]}
# for question
# for answer
# get skill, UV, DV
# if skill in user dict update
# else create new dict entry


## Take question_answer_vote as pandas data frame
## question_skill is predefined by question
def map_user_skills(question_answer_vote, question_skill, question_id):
    user_skill_map = {}
    for i, q in enumerate(question_id):
        this_record = question_answer_vote.ix[i]
        this_user = this_record['answer_user_id']
        if this_user not in user_skill_map.keys():
            user_skill_map[this_user] = {}
            
        this_type = this_record['vote_type']
        this_vote = this_record['vote_count']
        # invert sum if this is a downvote
        if this_type == 3:
            this_vote = this_vote * -1
        skills = question_skill[q]
        for s in skills.keys():
            weighted_vote = this_vote * skills[s]
            if s in user_skill_map[this_user].keys():
                user_skill_map[this_user][s] += weighted_vote
            else:
                user_skill_map[this_user][s] = weighted_vote
    return user_skill_map
            
# Strategy: for grouped python dataframe, split by user and apply
# the same function to each, counting skills and returning by userid (using aggregate)

                   
