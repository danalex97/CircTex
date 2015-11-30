#!/bin/sh
apt-get update
apt-get install ghci -y
apt-get install cabal-install -y
cabal install random -y
cabal install parsec -y
sudo apt-get install texlive-full -y
