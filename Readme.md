# Lazy.unix2win
- 2w is Mac or UNIX to Windows file converter.
- 2u is Mac or Windows to UNIX file converter.
- 2m is Windows or UNIX to Mac file converter.

## About Lazy.unix2win
- My first Haskell programming.
- Created for proof my Haskell skill.
- unix2dos can convert UNIX file to Dos file.
- 2w can convert any(UNIX or Mac) to Dos file.
- Dos/Windows end line = CR(13) and LF(10)
- UNIX/linux end line = LF(10)
- Modern Mac end line = CR(13)

## How to build version 1
```sh
$ make
```
## How to build version 2
```sh
$ cabal build
```
## How to build version 3
```sh
$ runhaskel Setup.hs configure --prefix="install directory"
$ runhaskel Setup.hs build
$ runhaskel Setup.hs install
```


- Email: booskillerz@gmail.com
- FB: facebook.com/watt.duean
