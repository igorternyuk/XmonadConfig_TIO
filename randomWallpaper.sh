#!/bin/bash
PATH=/usr/local/sbin:/usr/local/bin:/sbin:/bin:/usr/sbin:/usr/bin
export DISPLAY=:0.0

picsfolder="/home/igor/Pictures/Walpapers/xmonad/"
cd $picsfolder

files=(*.jpg)
N=${#files[@]}

((N=RANDOM%N))
randomfile1=${files[$N]}

feh --bg-fill $picsfolder$randomfile1