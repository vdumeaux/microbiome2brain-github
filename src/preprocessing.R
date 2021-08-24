

longranger.basic <- function(x){
  system(capture.output(
    cat("/home/shared/tools/longranger-2.2.2/longranger basic",  paste0("--id=",sample.table$V2[x]),
        paste0("--sample=",sample.table$V1[x]), paste0("--fastqs=", fqdir))
  ))
}



deinterleave.fastq <- function(x){
  system(capture.output(
    cat("/home/shared/tools/BBMAP/bbmap/reformat.sh", 
        paste0("in=", file.path(datdir, "longranger", x, "outs/barcoded.fastq.gz")), 
        paste0("out1=",file.path(datdir, "longranger", x, "outs/barcoded_R1.fastq")),
        paste0("out2=",file.path(datdir, "longranger", x, "outs/barcoded_R2.fastq"))
        )
  ))
}

interleave.fastq <- function(x){
  system(capture.output(
    cat("/home/shared/tools/BBMAP/bbmap/reformat.sh ow=t", 
        paste0("in=", file.path(datdir, "01-trimmed", x, "barcoded_R1_val_1.fq")), 
        paste0("in2=",file.path(datdir, "01-trimmed", x, "barcoded_R2_val_2.fq")),
        paste0("out=",file.path(datdir, "01-trimmed", x, "barcoded.fq"))
    )
  ))
}




trimming <- function(x){
  system(capture.output(
    cat("/home/shared/tools/TrimGalore-0.6.0/trim_galore --path_to_cutadapt ~/.local/bin/cutadapt --paired --phred33 -o",
        file.path(datdir, "01-trimmed", x), "--length 60 -q 5 --stringency 1 -e 0.1",
        file.path(datdir, "00-longranger", x, "outs/barcoded_R1.fastq"), file.path(datdir, "00-longranger", x, "outs/barcoded_R2.fastq"))
  ))
}

clouspades <- function(x){
  system(capture.output(
    cat("/home/shared/tools/spades-cloudspades-paper/SPAdes-3.12.0-dev/spades.py --meta -m 700",
        "--gemcode1-1", file.path(datdir, "trimmed", x, "barcoded_R1_val_1.fq"),
        "--gemcode1-2", file.path(datdir, "trimmed", x, "barcoded_R2_val_2.fq"),
        "-o", file.path(datdir, "trimmed", x))
  ))
}

bracken <- function(x){
  system(capture.output(
    cat("/home/data/analysis-tools/Bracken/bracken -d", kdb, 
                            "-i", file.path("/home/data/refined/microbiome2brain/microbiome/01-trimmed",x, "reads.kreport"), 
                            "-o", file.path("/home/data/refined/microbiome2brain/microbiome/01-trimmed",x, "reads.bracken"),
                            "-r 150 -l 'S' -t 10")
  ))
}


### align reads along contigs

build.index <- function(x){
  system(capture.output(
    cat("bowtie2-build", file.path(datdir, "trimmed", x, "contigs.fasta"),
        file.path(datdir, "trimmed", x, "contigs_index"))))
}

aligning <- function(x){
  system(capture.output(
    cat("bowtie2 -p 12 -q -1", file.path(datdir, "trimmed", x, "barcoded_R1_val_1.fq"),
        "-2", file.path(datdir, "trimmed", x, "barcoded_R2_val_2.fq"),
        "-x", file.path(datdir, "trimmed", x, "contigs_index"), 
        "-S", file.path(datdir,"aligned", paste0(x, ".sam")))
  ))
}

samToBam <- function(x){
  system(capture.output(
    cat("/home/shared/tools/samtools-1.9/samtools sort -@ 8 -o", gsub(".sam",".bam", x), x)
  ))
}


indexBam <- function(x){
  system(capture.output(
    cat("/home/shared/tools/samtools-1.9/samtools index", x)))
}

# merge.read1 <- function(x){
#   system(capture.output(
#     cat("cat", raw.read1[grep(x, raw.read1)], ">", file.path(datdir, "fastq", paste0(x, "_read1.fq")))
#     ))
# }
# merge.read2 <- function(x){
#   system(capture.output(
#     cat("cat", raw.read2[grep(x, raw.read2)], ">", file.path(datdir, "fastq", paste0(x, "_read2.fq")))))
# }

# merge.unpaired.1 <- function(x){
#   system(capture.output(
#     cat("cat", trimmed.paired1[grep(x, trimmed.paired1)], ">", file.path(datdir, "trimmed", paste0(x, "_paired_1.fq")))
#   ))
# }
# merge.paired.2 <- function(x){
#   system(capture.output(
#     cat("cat", trimmed.paired2[grep(x, trimmed.paired2)], ">", file.path(datdir, "trimmed", paste0(x, "_paired_2.fq")))))
# }
# 
# interleave.fastq <- function(x){
#   system(capture.output(
#      cat("/home/shared/tools/BBMAP/bbmap/reformat.sh", 
#          paste0("in",1:4,"=",trimmed.paired[grep(x, trimmed.paired)]), 
#          paste0("out=",file.path(datdir, "trimmed", paste0(x, "_paired.fq"))))
#   ))
# }

# metaspades <- function(x){
#   system(capture.output(
#     cat("/home/shared/tools/MetaSpades/SPAdes-3.11.1-Linux/bin/metaspades.py --pe1-1", reads1[grep(x, reads1)][1], 
#         "--pe2-1", reads1[grep(x, reads1)][2],"--pe1-2", reads2[grep(x, reads2)][1], 
#         "--pe2-2", reads2[grep(x, reads2)][2], "--pe1-s", unpaired[grep(x, unpaired)][1], "--pe2-s", unpaired[grep(x, unpaired)][2], 
#         "--pe3-s", unpaired[grep(x, unpaired)][3], "--pe4-s", unpaired[grep(x, unpaired)][4], "-o", file.path(datdir, "metaspades")
#         
