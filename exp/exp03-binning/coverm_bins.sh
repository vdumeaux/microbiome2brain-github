#!/bin/bash

for i in E1A_B1 E1B_B2 E2A_A3 E2B_A4 E3A_A5 E3B_A6 E4A_A7 E4B_A8 E5A_B3 E5B_B4 F1A_A1 F1B_A2 F2A_B5 F2B_B6
do
coverm genome -t 8 --coupled /home/data/refined/microbiome2brain/microbiome/01-trimmed/$i/barcoded_R1_val_1.fq /home/data/refined/microbiome2brain/microbiome/01-trimmed/$i/barcoded_R2_val_2.fq --genome-fasta-directory /home/data/refined/microbiome2brain/microbiome/03-metabat/drep_bins/dereplicated_genomes > /home/data/refined/microbiome2brain/microbiome/03-metabat/drep_bins/dereplicated_genomes/$i.coverage.bins.tsv
done
