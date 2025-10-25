## Cách biên dịch và chạy

Mở **3 cửa sổ terminal** riêng biệt, 1 Server và 2 Terminal

---

### 1. Terminal 1: Server

Chạy các lệnh sau trong terminal đầu tiên:

# 1. Biên dịch server
ghc --make Server.hs -o server

# 2. Chạy server
./server.exe

### 2. Terminal 2, 3: Server

# 1. Biên dịch client (Chỉ cần làm ở 1 terminal)
ghc --make Client.hs -o client

# 2. Chạy client (Chạy ở cả 2 terminal)
./client.exe