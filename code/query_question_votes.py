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
conn_cursor.execute("SELECT q.ID as question_id,
a.ID as answer_id,
a.OWNERUSERID as answer_user,
v.VOTETYPEID as vote_type,
count(*) as vote_count
FROM 
posts q INNER JOIN posts a 
ON q.ID=a.PARENTID and a.POSTTYPEID=2
INNER JOIN
votes v ON v.POSTID=a.ID and v.VOTETYPEID IN (1,2,3)
GROUP BY 
a.ID, v.VOTETYPEID
ORDER BY NULL")

## Note tag_data here is a tuple of strings
vote_count_data = conn_cursor.fetchall()

conn_cursor.close()
conn.close()
## End data read-in
