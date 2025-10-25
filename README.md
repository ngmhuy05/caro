## Cách biên dịch và chạy

Bạn sẽ cần mở **3 cửa sổ terminal** riêng biệt. Để giữ thư mục dự án sạch sẽ, chúng ta sẽ biên dịch các file trung gian (`.o`, `.hi`) vào thư mục `build`.

(Nếu bạn chưa có thư mục `build`, hãy chạy: `mkdir build`)

---

### 1. Terminal 1: Server

Chạy các lệnh sau trong terminal đầu tiên:

```bash
# 1. Biên dịch server
ghc --make Server.hs -o server -odir build -hidir build

# 2. Chạy server
./server.exe


Bạn sẽ thấy thông báo: Server dang lang nghe tai port 4444...

2. Terminal 2 & 3: Client
Trong hai terminal còn lại, hãy chạy các lệnh sau.

Bash

# 1. Biên dịch client (Chỉ cần làm ở 1 terminal)
ghc --make Client.hs -o client -odir build -hidir build

# 2. Chạy client (Chạy ở cả 2 terminal)
./client.exe
Client đầu tiên sẽ chờ, client thứ hai kết nối vào game sẽ bắt đầu.