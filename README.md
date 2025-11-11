# CARO – Quick Start

## Yêu cầu
- GHC ≥ 9.6.
- Windows: `libfreeglut.dll`

## Clone
```bash
git clone https://github.com/ngmhuy05/caro
cd caro

# build
Stop-Process -Name client,server -Force -ErrorAction SilentlyContinue
mkdir bin -ErrorAction SilentlyContinue | Out-Null
ghc -isrc -O2 -Wall -threaded -odir bin -hidir bin -outputdir bin -o server.exe src\Server.hs
ghc -isrc -O2 -Wall -threaded -odir bin -hidir bin -outputdir bin -o client.exe src\Client.hs

# Terminal 1
.\server.exe   # server lắng trên cổng 4444

# Terminal 2
.\client.exe   # người chơi 1 (X)

# Terminal 3
.\client.exe   # người chơi 2 (O)

