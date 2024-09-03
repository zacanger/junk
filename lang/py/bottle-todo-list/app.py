import sqlite3
from bottle import (
    route,
    run,
    debug,
    template,
    request,
    static_file,
    error,
)


# debug(True)


@route("/todo")
def todo_list():
    conn = sqlite3.connect("todo.db")
    c = conn.cursor()
    c.execute("SELECT id, task FROM todo WHERE status LIKE '1'")
    result = c.fetchall()
    c.close()
    output = template("make_table", rows=result)
    return output


@route("/new", method="GET")
def new_todo():
    if request.GET.save:
        new = request.GET.task.strip()
        conn = sqlite3.connect("todo.db")
        c = conn.cursor()
        c.execute("INSERT INTO todo (task,status) VALUES (?,?)", (new, 1))
        new_id = c.lastrowid
        conn.commit()
        c.close()
        return "<p>The new task was inserted into the database, the ID is %s</p>" % new_id
    else:
        return template("new_task.tpl")


@route("/edit/<n:int>", method="GET")
def edit_item(n):
    if request.GET.save:
        edit = request.GET.task.strip()
        status = request.GET.status.strip()

        if status == 'open':
            status = 1
        else:
            status = 0

        conn = sqlite3.connect("todo.db")
        c = conn.cursor()
        c.execute(
            "UPDATE todo SET task = ?, status = ? WHERE id LIKE ?",
            (edit, status, n))
        conn.commit()

        return '<p>The item number %s was successfully updated</p>' % n
    else:
        conn = sqlite3.connect("todo.db")
        c = conn.cursor()
        c.execute("SELECT task FROM todo WHERE id LIKE ?", (str(n)))
        curr = c.fetchone()
        return template('edit_task', old=cur_data, no=no)


@route("/item<item:re[0-9]+>")
def show_todo(item):
    conn = sqlite3.connect("todo.db")
    c = conn.cursor()
    c.execute("SELECT task FROM todo WHERE id LIKE ?", (item))
    result = c.fetchall()
    c.close()
    if not result:
        return "oh noes"
    return "Task %s" % result[0]


@route("/help")
def help():
    return static_file("help.html", "path-here ???")


@route("/json<json:re[0-9]+>")
def show_json(json):
    conn = sqlite3.connect("todo.db")
    c = conn.cursor()
    c.execute("SELECT task FROM todo WHERE id LIKE ?", (json))
    result = c.fetchall()
    c.close()
    if not result:
        return {"task": "nope"}
    return {"task": result[0]}


@error(401)
@error(403)
@error(404)
def nope(code):
    return "hell nah"


if __name__ == "__main__":
    run(reloader=True, port=9090, host="0.0.0.0")
