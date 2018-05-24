#!/bin/sh
# $1: timelist_ut_bastille_day.txt remove first line

LENGTH=276

for i in $(seq 1 $LENGTH); do
  filename="/home/jgrangien/Code/OpenSpace/data/mas/bastille/generated/mas_merged_step_${i}.dictionary"

  sed -i -e 's/MaxValue=[A-Za-z0-9.-]*/MaxValue=1/g' $filename
  sed -i -e 's/MinValue=[A-Za-z0-9.-]*/MinValue=0/g' $filename

  time=$(sed -n ${i}p "$1" | grep -o -P "2000.*$")
  sed -i -e "s/Time=\"0000-01-01T00:00:00.001\"/Time=\"${time}\"/g" $filename
done
