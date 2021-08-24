#!/bin/bash


binDIR="/home/data/refined/microbiome2brain/microbiome/03-metabat/cleaned_bins"
files=`find $binDIR -name "*.fna"`
echo $files

for i in $files
do
out=`basename "${i%%.fna}"`
/home/data/analysis-tools/kraken/kraken2 --db /home/shared/data/db/kraken2_microbiome $i \
--report /home/data/refined/microbiome2brain/microbiome/03-metabat/cleaned_bins/$out.kreport > /home/data/refined/microbiome2brain/microbiome/03-metabat/cleaned_bins/$out.kraken
done