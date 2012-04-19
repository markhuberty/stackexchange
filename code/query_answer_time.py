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

conn_cursor.execute("""SELECT q.CREATIONDATE as question_date, 
q.ID as question_id, 
q.OWNERUSERID as question_user,
a.CREATIONDATE as answer_date,
a.ID as answer_id, 
a.OWNERUSERID as answer_user, 
v.VOTETYPEID as vote_type, count(*) as vote_coount
FROM posts q INNER JOIN posts a
ON q.ID=a.PARENTID and a.POSTTYPEID=2
INNER JOIN votes v ON v.POSTID=a.ID and v.VOTETYPEID IN (1,2,3)
GROUP BY a.ID,v.VOTETYPEID 
ORDER BY NULL""")

flow_data = conn_cursor.fetchall()

conn_cursor.close()
conn.close()
## End data read-in

conn = open('../data/question_answer_time.csv', 'wt')
writer = csv.writer(conn)
writer.writerow(['question_date','question_id','question_userid','answer_date','answer_id',
                 'answer_userid','vote_type','vote_count'])
for r in flow_data:
    writer.writerow(list(r))
conn.close()
