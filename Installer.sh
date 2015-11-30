#!/bin/sh
apt-get update
sudo apt-get install ghci -y
sudo apt-get install cabal-install -y
cabal update
cabal install random -y
cabal install parsec -y
sudo apt-get install texlive-full -y
