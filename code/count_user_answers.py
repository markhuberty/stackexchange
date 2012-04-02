## Alternative to R file of the same name
## NOTE: this doesn't really work well right now
## Needs to use scipy to build a sparse matrix and do 
## algebra on it. 
## Also need to change the SQL query to extract only the tags 
## that aren't blank. 
import MySQLdb
import re
import csv
import os
from scipy.sparse import *
from scipy import *
import gc
import pickle

#os.chdir("../data/")

## Read in the tag vector from the database
conn = MySQLdb.connect(host="localhost",
                       user="markhuberty",
                       passwd="overflowpwd",
                       db="stackoverflow"
                       )

conn_cursor = conn.cursor()
conn_cursor.execute("""
SELECT OWNERUSERID, count(*) FROM posts 
WHERE POSTTYPEID=2
GROUP BY OWNERUSERID
""")

user_answer_count = conn_cursor.fetchall()
conn_cursor.close()
conn.close()

conn = open('../data/user_answer_counts.csv', 'wt')
writer = csv.writer(conn)
writer.writerow(['user_id', 'answer_count'])
for r in user_answer_count:
    writer.writerow(list(r))
conn.close()

