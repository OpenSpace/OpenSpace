#!/bin/sh
# $1: timelist_ut_bastille_day.txt remove first line
#
# For every dictionary file 1 to LENGTH,
# Set MaxValue to 1,
# MinValue to 0,
# Time to corresponding time for sequence number in timelist_ut_bastille_day.txt

LENGTH=117

for i in $(seq 1 $LENGTH); do
  filename="/home/jgrangien/Code/OpenSpace/data/mas/bastille/generated/mas_merged_step_${i}.dictionary"

  if [ ! -f /tmp/foo.txt ]; then
    echo "Skipping file ${i}, not found."
    continue
  fi

  sed -i -e 's/MaxValue=[A-Za-z0-9.-]*/MaxValue=1/g' $filename
  sed -i -e 's/MinValue=[A-Za-z0-9.-]*/MinValue=0/g' $filename

  time=$(sed -n ${i}p "$1" | grep -o -P "2000.*$")
  sed -i -e "s/Time=\"0000-01-01T00:00:00.001\"/Time=\"${time}\"/g" $filename
done
