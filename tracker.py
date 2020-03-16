import socket
import threading
import json

def tracker():
    connections = []
    peers = []
    server = []
    def handle():
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        sock.bind(('localhost', 5656))
        sock.listen(1)
        print("tracker running")
        while True:
            c, a = sock.accept()
            cThread = threading.Thread(target=handler, args=(c,a))
            cThread.daemon = True
            cThread.start()
            connections.append(c)
            peers.append(str(a[0]) + ':' + str(a[1]))
            print(str(a[0]) + ':' + str(a[1]), "connected")
            print("peers: ", peers)
            peerconnection(c)
    def handler(c, a):
        while True:
            header = c.recv(1)
            if (header == b'\x01'):
                size = int.from_bytes(c.recv(2), byteorder='big')
                data = c.recv(size)
                ports = str(data, 'utf-8')
                server.append(str(a[0]) + ':' + ports)
            elif(header == b'\x02'):
                print("received a request about member list")
                senspeers = []
                index = []
                for peer in peers:
                    p = peer.split(":")
                    if p[0] != str(a[0]) or p[1] != str(a[1]):
                        index.append(peers.index(peer))
                # print("peer to send", senspeers)
                for i in index:
                    senspeers.append(server[i])
                data_string = json.dumps(senspeers)
                encode_data = bytes(data_string, "utf-8")
                size = len(encode_data).to_bytes(2, byteorder='big')
                c.sendall(b'\x02' + size + encode_data)
            elif (header == b'\x03'):
                for peer in peers:
                    p = peer.split(":")
                    if p[0] == str(a[0]) and p[1] == str(a[1]):
                        peers.remove(peer)
                print("peers after removing transactor: ", str(peers))
            if not header:
                print(str(a[0]) + ':' + str(a[1]), "disconnected")
                connections.remove(c)
                peers.remove(str(a[0]) + ':' + str(a[1]))
                c.close()
                break
    def peerconnection(c):
        message = "Connect to tracker successful"
        encode_message = bytes(message, "utf-8")
        len_message = len(encode_message)
        size = len_message.to_bytes(2, byteorder='big')
        c.sendall(b'\x01' + size + encode_message )
    t = threading.Thread(target=handle, args=())
    return t

tracker().start()
