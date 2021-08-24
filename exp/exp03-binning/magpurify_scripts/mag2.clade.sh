#!/bin/bash
filename="$1"

while read -r field1 field2 ;  
do
echo $field1
echo $field2

magpurify clade-markers $field1 $field2 --threads 8 --db /home/shared/data/db/MAGpurify-db-v1.0

done < "$filename"

