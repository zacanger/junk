#!/usr/bin/env python

import socket

server_host = '0.0.0.0'
server_port = 9999

server_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
server_socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
server_socket.bind((server_host, server_port))
server_socket.listen(1)

print("Listening on port %s" % server_port)

while True:
    client_connection, client_addr = server_socket.accept()
    req = client_connection.recv(1024).decode()

    headers = req.split('\n')
    filename = headers[0].split()[1]

    if filename == '/':
        filename = 'index.html'

    try:
        html = open(filename)
        content = html.read()
        html.close()
        resp = 'HTTP/1.0 200 OK\n\n' + content

    except FileNotFoundError:
        resp = 'HTTP/1.0 404 NOT FOUND\n\n404 oh no'

    client_connection.sendall(resp.encode())
    client_connection.close()

server_socket.close()
