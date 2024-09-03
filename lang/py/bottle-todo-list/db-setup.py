import sqlite3


conn = sqlite3.connect('todo.db')
conn.execute("CREATE TABLE todo (id INTEGER PRIMARY KEY, task char(100) NOT NULL, status bool NOT NULL)")
conn.execute("INSERT INTO todo (task,status) VALUES ('a thing',1)")
conn.execute("INSERT INTO todo (task,status) VALUES ('another thing',0)")
conn.commit()
