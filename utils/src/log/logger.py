import socket
from time import sleep

'''
This script is used to log string received via tcp by the rae process.
No special format is applied to the string.
It ends when it received the string "END" (unsafe but easiest way to terminate the script)

A simpler version is used using tail -f <log-file>.
The option -f enable following, printing in real time updates on the file.
The drawback of the simpler version is that we have to check process to kill the terminals process.
'''
HOST = '127.0.0.1'
PORT = 10001


print("RAE LOGGER:")
serveur = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
serveur.bind((HOST, PORT))
serveur.listen(1)

client, adrClient = serveur.accept()
print("client connected!")

while True:
    log_data = client.recv(16384)
    if not log_data:
        print("Error on reception")
        break
    else:
        value = log_data.decode()
        if value == "END":
            print("terminating terminal")
            break
        else:
            print('>> '+log_data.decode())