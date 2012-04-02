import re
import csv
import os
from scipy.sparse import *
from scipy import *
import gc
import pickle
import MySQLdb
import NumPy

os.chdir('C:/Users/miaomiaocui/stackexchange/')

f = open('./data/vote_country_by_qid_aid_uid.csv','rb')
vote_postid = pickle.load(f)
f.close()

f= open('./data/question_skill_map.pickle')
que_skill=pickle.load(f)
f.close()

conn=MySQLdb.connect(host="localhost",
                     user="cui",
                     passwd="overflowpwd",
                     db="stackoverflow"
                     )
qid = conn.cursor()
qid.execute("SELECT ID FROM posts WHERE TAGS !=\"\"")
  
def map_v_q_skill(question,answer,user,votetype,votecount):
    vqk_map=[]
    for idx in que_skill.keys():
    this_qid=qid[idx]
      for row in vote_postid:
          if qid=this_qid:
          these_votes=row
         for line in these_votes:
             for skill in que_skill[idx]:
                 weighted_skill=
          
    
