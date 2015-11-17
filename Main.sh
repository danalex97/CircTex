#!/bin/sh

while [ true ]
do
   runghc Main.hs
   pdflatex aux.tex
   gnome-open aux.pdf
done
