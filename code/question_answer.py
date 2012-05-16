'''
Created on May 14, 2012

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

conn_cursor.execute("""SELECT CONCAT(YEAR(CREATIONDATE),'/',WEEK(CREATIONDATE)) AS week,
OWNERUSERID AS user, POSTTYPEID, COUNT(POSTTYPEID)
FROM posts WHERE POSTTYPEID!=3
GROUP BY user,week,POSTTYPEID""")

flow_data = conn_cursor.fetchall()

conn_cursor.close()
conn.close()
## End data read-in

conn = open('/mnt/fwire_80/question_answer_counts.csv', 'wt')
writer = csv.writer(conn)
writer.writerow(['week','user','post_type','count'])
for r in flow_data:
    writer.writerow(list(r))
conn.close()
