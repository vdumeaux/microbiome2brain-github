#!/bin/bash
filename="$1"

while read -r field1 field2 field3 ;  
do
echo $field1
echo $field2

magpurify clean-bin $field1 $field2 /home/data/refined/microbiome2brain/microbiome/03-metabat/cleaned_bins/$field3.fna

done < "$filename"

