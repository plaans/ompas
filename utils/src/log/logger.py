import socket
from time import sleep

HOST = '127.0.0.1'
PORT = 10001


print("RAE LOGGER:")
serveur = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
serveur.bind((HOST, PORT))
serveur.listen(1)

client, adrClient = serveur.accept()
print("client connected!")

while True:
    log_data = client.recv(1024)
    if not log_data:
        print("Error on reception")
        break
    else:
        value = log_data.decode()
        if value == "END":
            print("terminating terminal")
            break
        else:
            print(log_data.decode())