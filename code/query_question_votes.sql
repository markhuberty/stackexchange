SELECT q.ID as question_id, a.ID as answer_id, a.OWNERUSERID as answer_user, v.VOTETYPEID as vote_type, count(*) as vote_count
FROM 
posts q INNER JOIN posts a 
ON q.ID=a.PARENTID and a.POSTTYPEID=2
INNER JOIN
votes v ON v.POSTID=a.ID and v.VOTETYPEID IN (1,2,3)
GROUP BY 
a.ID, v.VOTETYPEID
ORDER BY NULL;
