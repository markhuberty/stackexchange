'''
Created on Apr 14, 2012

@author: miaomiaocui
'''

import MySQLdb
import re
import csv
import os
from scipy.sparse import *
from scipy import *
import gc
import pickle

conn = MySQLdb.connect(host="localhost",
                       user="cui",
                       passwd="cui_overflowpwd",
                       db="stackoverflow"
                       )

conn_cursor = conn.cursor()

conn_cursor.execute("""SELECT qu.LOCATION as q_u_location,au.LOCATION as a_u_LOCATION,qu.REPUTATION as
q_u_rep, au.REPUTATION as a_u_rep, q.ID as question_id, q.OWNERUSERID as question_user,
a.ID as answer_id, a.OWNERID as answer_user, v.VOTETYPE as vote_type, count(*) as vote_coount
FROM posts q INNER JOIN posts a
ON q.ID=a.PARENTID and a.POSTTYPEID=2
INNER JOIN votes v ON v.POSTID=a.ID and v.VOTETYPEID IN (1,2,3)
INNER JOIN users qu ON qu.ID=q.OWNERUSERID 
INNER JOIN users au ON au.ID=a.OWNERUSERID
GROUP BY a.ID,v.VOTETYPEID 
ORDER BY NULL""")

flow_data = conn_cursor.fetchall()

conn_cursor.close()
conn.close()
## End data read-in

conn = open('C:/Users/miaomiaocui/stackexchange/data/question_answer_flow.csv', 'wt')
writer = csv.writer(conn)
writer.writerow(['question_user_location','answer_user_location','question_user_rep',
                 'answer_user_rep','question_id','question_userid','answer_id','answer_userid','vote_type_vote_count'])
for r in flow_data:
    writer.writerow(list(r))
conn.close()
