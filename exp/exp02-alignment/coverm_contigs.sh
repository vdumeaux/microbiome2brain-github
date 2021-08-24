#!/bin/bash

for i in E1A_B1 E1B_B2 E2A_A3 E2B_A4 E3A_A5 E3B_A6 E4A_A7 E4B_A8 E5A_B3 E5B_B4 F1A_A1 F1B_A2 F2A_B5 F2B_B6
do
    coverm contig --bam-files /home/data/refined/microbiome2brain/microbiome/02-aligned/$i.bam > /home/data/refined/microbiome2brain/microbiome/02-aligned/coverage_contigs/$i.coverage.contigs.tsv
done
