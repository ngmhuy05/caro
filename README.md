<p align="center">
  <img src="https://img.shields.io/badge/CARO-Gomoku-%23ff4757">
  <img src="https://img.shields.io/badge/GHC-9.6%2B-blue">
  <img src="https://img.shields.io/badge/Gloss-OpenGL-informational">
</p>

<h1 align="center"> Trò chơi CARO trong Haskell </h1>

## Luật chơi
Hai người chơi X và O đánh lần lượt, X đi trước.
Bước hợp lệ: đặt quân vào ô trống.

Kết thúc ván:
 - Win: có 5 quân liên tiếp thẳng hàng.

 - Draw: bàn đầy mà chưa có chuỗi thắng.

## Yêu cầu
- GHC ≥ 9.6.
- Windows: `libfreeglut.dll`

## Clone
```bash
git clone https://github.com/ngmhuy05/caro
cd caro
```

Trường hợp bạn clone mà bị thiếu file `libfreeglut.dll`
thì hãy cài MSYS2 rồi mở nó lên và chạy dòng này ```pacman -S mingw-w64-x86_64-freeglut```

## Build
PowerShell
```bash
Stop-Process -Name client,server -Force -ErrorAction SilentlyContinue
Remove-Item .\client.exe, .\server.exe -Force -ErrorAction SilentlyContinue
if (Test-Path .\bin) { Remove-Item .\bin\* -Recurse -Force -ErrorAction SilentlyContinue } else { New-Item -ItemType Directory -Path .\bin | Out-Null }

ghc -isrc -O2 -Wall -threaded -odir bin -hidir bin -outputdir bin -o .\server.exe .\src\Server.hs
ghc -isrc -O2 -Wall -threaded -odir bin -hidir bin -outputdir bin -o .\client.exe .\src\Client.hs
```

## Tạo 3 terminal để chạy server cho 2 người chơi tham gia
Terminal 1
```terminal
./server.exe   # tạo server
```

Terminal 2
```bash
./client.exe   # người chơi 1 (X)
```

Terminal 3
```bash
./client.exe   # người chơi 2 (O)
```

## (OPTIONAL) Chơi qua mạng sử dụng Radmin VPN
Cài Radmin VPN trên cả hai máy, tạo private network chung 
Lấy IP ảo của máy chủ trong Radmin (thường 26.x.x.x).
Kiểm tra từ máy khách: ```ping <IP_RADMIN_MAY_CHU>```

Máy chủ
```bash
./server.exe IP_MÁY_CHỦ 4.4.4.4
```
Người chơi 1-2
```bash
./client.exe <IP_RADMIN_MAY_CHU> 4444
```
Nếu bị lỗi không kết nối được thì hãy mở PowerShell bằng Admin và chạy 
```bash
netsh advfirewall firewall add rule name="Caro-Server-4444" dir=in program="%CD%\server.exe" action=allow enable=yes
```
