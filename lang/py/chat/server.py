#!/usr/bin/env python

import asyncio
from aiohttp import web
import socketio
from json import dumps


sio = socketio.AsyncServer(async_mode="aiohttp")
app = web.Application()
sio.attach(app)


@sio.event
async def join_chat(sid, message):
    print(message.get("name", sid) + "joined {}".format(message["room"]))
    sio.enter_room(sid, message["room"])


@sio.event
async def leave_chat(sid, message):
    sio.leave_room(sid, message["room"])


@sio.event
async def send_chat(sid, message):
    await sio.emit("get_message", {
        "message": message["message"],
        "from": message["name"],
        "room": message["room"]})


@sio.event
async def connect(sid, env):
    await sio.emit("response", {"data": "Connected", "count": 0}, room=sid)


@sio.event
def disconnect(sid):
    print("Client disconnected")


if __name__ == "__main__":
    web.run_app(app)
