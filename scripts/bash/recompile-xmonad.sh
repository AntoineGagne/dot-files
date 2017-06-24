#! /bin/sh

ghc --make ~/.xmonad/xmonad.hs -i -ilib -dynamic -fforce-recomp -main-is main -v0 -o ~/.xmonad/xmonad-x86_64-linux
