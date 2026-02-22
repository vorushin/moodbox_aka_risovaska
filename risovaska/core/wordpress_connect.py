# -*- coding: utf-8 -*-
import os
import MySQLdb

class BlogPost:
	def __init__(self, link, title, date, message):
		self.link = link
		self.title = title
		self.date = date
		self.message = message

# getting latest blog posts from Wordpress database
def get_latest_blog_posts():
	blog_posts = []
	conn = MySQLdb.connect(
			host = os.environ.get("WP_DB_HOST", "localhost"),
			user = os.environ.get("WP_DB_USER", "blog_user"),
			passwd = os.environ.get("WP_DB_PASSWORD", ""),
			db = os.environ.get("WP_DB_NAME", "blog")
		)
	cursor = conn.cursor()

	try:		
		cursor.execute("SET NAMES utf8;") # fixing mysql error with default encoding (by default = latin1)
		cursor.execute("""SELECT `ID`,`post_date`,`post_content`,`post_title` 
			FROM `wp2_posts` WHERE `post_status`='publish' 
			ORDER BY `post_date` DESC LIMIT 2""")
		records = cursor.fetchall()
		for record in records:
			post = BlogPost(
				link="http://risovaska.ru/news/?p=" + str(record[0]), 
				date = record[1],
				message = record[2],
				title = record[3]
			)
			more_pos = post.message.find("<!--more-->")
			if more_pos != -1:
				post.message = post.message[0:more_pos] + '<a href="' + post.link + u'">Читать дальше...</a>'.encode('utf-8')
				
			blog_posts.append(post)
	finally:	
		cursor.close()
		conn.close()
	
	return blog_posts