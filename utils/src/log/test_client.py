import socket
import time

HOST = '127.0.0.1'
PORT = 10001

client = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
client.connect((HOST, PORT))


for i in range(1,5):
    client.send('test log'.encode())
    time.sleep(1)

client.send('END'.encode())
client.close()