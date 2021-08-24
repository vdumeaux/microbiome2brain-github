The microbiome2btain project developed a novel exercise regimen based on short high-intensity intervals (HIIT) designed to provide high benefits in brain health. Although few studies have highlighted that exercise intensity may be more beneficial for cognitive function than duration, the neurobiological underpinning of these associations remain unknown. In addition to deepening our understanding of important parameters controlling the efficacy of an exercise regimen, our project aims to capture how exercise affects the gut microbiome, a complex environment that is known to affect aspects of brain and cognitive function.

Our pilot study included 7 sedentary male aged 51-67 years. They performed the same HIIT program (3x/week for 8 weeks). Stool samples and brain images (resting-state and task-based fMRI) were collected before and after the intervention. Participants maintained their habitual diet. Gut microbial composition and pathways levels were estimated from whole-genome linked-reads (10x genomics) metagenomic profiles.


## Microbiome data analysis ##


### Preprocessing ###
Source code in src/preprocessing.R
Run-all script in exp/exp01-preprocessing/preprocessing.Rmd

1. [The longranger basic pipeline ](https://support.10xgenomics.com/genome-exome/software/pipelines/latest/advanced/other-pipelines)
It performs basic read and barcode processing including read trimming, barcode error correction, barcode whitelisting, and attaching barcodes to reads. 
It outputs barcode-attached reads either as FASTQ or unaligned BAM.

2. longranger basic does not perform adapter trimming (this steps is important in our data as without it many contigs map to the carp genome which is [due to reads with illumina adapters](http://www.opiniomics.org/we-need-to-stop-making-this-simple-fcking-mistake/)). To run TrimGalore! we need 2 files for each sample instead of interleaved paired reads (output of longranger basic). we use BBMap reformat to do so, and run TrimGalore keeping only paired reads.


### Assembly-based microbiome reconstruction ###

1. We then conducted Metagenome assemblies using [cloudspades](https://academic.oup.com/bioinformatics/article/35/14/i61/5529115)
Although cloudspades deals with assembling synthetic long reads, it does not allow co-assembly (assembly based on reads from multiple samples).
From this step we obtain assembled contigs for each sample (co-assembly is not supported at this time by cloudspades)


2. [MetaBAT2](https://peerj.com/articles/1165/) (v.2.12.1) was used to bin the assemblies using default parameters (min contigs 1500bp). Depth of coverage required for the binning was inferred by mapping the raw reads back to their assemblies with bowtie2 and then calculating the corresponding read depths of each individual contig with samtools (‘samtools view -Sbu’ followed by ‘samtools sort’) together with the jgi_summarize_bam_contig_depths function from MetaBAT 2.


3. Bin refinment was done using [MAGPurify](https://github.com/snayfach/MAGpurify) as descirbed in [Nayfach et al. 2019](http://dx.doi.org/10.1038/s41586-019-1058-x).


4. [dRep](https://www.nature.com/articles/ismej2017126) first filters incomplete bins ( [CheckM](https://genome.cshlp.org/content/25/7/1043) completeness >= 75% and contamination <= 25% ) and then identifies groups of organisms that share similar DNA content in terms of Average Nucleotide Identity (ANI).


Bins above certain quality criteria can be labeled taxonomically using tools like Kraken or [BAT](https://genomebiology.biomedcentral.com/articles/10.1186/s13059-019-1817-x), phylogenetically using [PhyloPhlAn 3.0](https://www.nature.com/articles/s41467-020-16366-7).

We used [CoverM](https://github.com/wwood/CoverM) to count reads in each cleaned and dereplicated MAG for each sample (2 methods: relative abundance and rau read counts). 


### Read-based microbiome reconstruction ###

An alternative approach to assembly-based microbiome profiling methods is to reconstruct a microbial community frommetagenomics data by leveraging the information about existing microbial genomes in public databases to infer what is contained in the metagenome without explicitly reconstructing the genomes. These approaches are commonly known as ‘read-based’ or ‘reference-based’. Generally speaking, read-based analyses yield good results when the microbial diversity of the samples is relatively well known (Knight et al. 2018), but the final results can be heavily influenced by the choice of database (Shaiber and Eren 2019; Breitwieser et al. 2019). 


In this analysis, we conducted read-based microbiome reconstruction using Kraken2 and metaphlan 3.0. Functional profile inference is done using HumanN 2.0.


## MRI data analysis ##

We adhere to the guidelines outlined in the [Brain Imaging Data Structure](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4978148/). After quality control using [MRIQC](https://mriqc.readthedocs.io/en/stable/), preprocessing of functional images is performed using [Nipype-based tool](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3159964/) [fMRIprep](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6319393/).

A [thread](https://twitter.com/VDumeaux/status/1400097518504054785)




