#!/bin/bash
filename="$1"

while read -r field1 field2 ;  
do
echo $field1
echo $field2

magpurify tetra-freq $field1 $field2

done < "$filename"

