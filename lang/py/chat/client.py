#!/usr/bin/env python

from socketio import AsyncClient
import asyncio
from json import dumps
from aioconsole import ainput


def run():
    IP = "0.0.0.0"
    PORT = "8080"
    client_name = "whoever"
    room_name = "test"
    message = ""

    sio = AsyncClient()
    uri = "http://" + IP + ":" + PORT

    @sio.event
    async def connect():
        print("connected")
        await sio.emit("join_chat", {"room": room_name, "name": client_name})

    @sio.event
    async def get_message(message):
        if client_name == message["from"]:
            print("Me: " + message["message"])
        else:
            print(message["from"] + ": " + message["message"])

    async def send_message():
        while True:
            await asyncio.sleep(0.1)
            message_to_send = await ainput()
            await sio.emit("send_chat", {
                "message": message_to_send,
                "name": client_name,
                "room": room_name})

    async def connect_to_server():
        await sio.connect(uri)
        await sio.wait()

    async def main(ip):
        await asyncio.gather(
            connect_to_server(),
            send_message())

    loop = asyncio.get_event_loop()
    loop.run_until_complete(main(uri))


if __name__ == "__main__":
    run()
