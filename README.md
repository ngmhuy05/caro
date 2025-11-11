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
Remove-Item .\server.exe, .\client.exe -Force -ErrorAction SilentlyContinue
mkdir bin -ErrorAction SilentlyContinue | Out-Null

ghc -isrc -O2 -Wall -threaded -odir bin -hidir bin -outputdir bin -o .\server.exe .\src\Server.hs
ghc -isrc -O2 -Wall -threaded -odir bin -hidir bin -outputdir bin -o .\client.exe .\src\Client.hs

```

## Tạo 3 terminal để chạy server cho 2 người chơi tham gia
Terminal 1
```terminal
./server.exe   # server lắng trên cổng 4444
```

Terminal 2
```bash
./client.exe   # người chơi 1 (X)
```

Terminal 3
```bash
./client.exe   # người chơi 2 (O)
```
