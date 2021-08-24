library(rprojroot)
root<-rprojroot::find_root(".git/index")

setwd(file.path(root,"exp/exp15-maaslin2Tax"))   

library(phyloseq)
library(dplyr)

kraken_count <- readRDS("/home/data/refined/microbiome2brain/microbiome/R/kraken_count.rds")
kraken_count_spec <- tax_glom(kraken_count, "species")

dt <- as.data.frame(otu_table(kraken_count_spec))
spec.nms <- kraken_count_spec %>% tax_table() %>% as("matrix") %>% data.frame()
rownames(dt) <- spec.nms$species

dt = data.frame(t(dt), check.names = F)

pdata <- kraken_count_spec %>% sample_data() %>% data.frame()

library(Maaslin2)
# Run Maaslin2
#defaults are tss normalization (turning that off since I already did that), log transform, lm test, max_significance=0.25,
#min_abundance=0, min_prevalence=0.1 (changing this to 0 because I already filtered). 
Maaslin2(dt, pdata, "output/timepoint",
         random_effects = "participant", 
         fixed_effects = "timepoint")

# Maaslin2(dt, pdata, "output/rest1_R",
#          random_effects = "participant", 
#          fixed_effects = "rest1_R")


Maaslin2(dt, pdata, "output/bmi",
         random_effects = "participant",
         fixed_effects = "calculatedBMI")

Maaslin2(dt, pdata, "output/bmiGroup",
         random_effects = "participant",
         fixed_effects = "bmi_group")

Maaslin2(dt, pdata, "output/vo2",
         random_effects = "participant",
         fixed_effects = "VO2max_ml.kg.min")

# Maaslin2(dna_path_unstratified, pdata, "output/bmi_leptin",
#          random_effects = "participant",
#          fixed_effects = c("calculatedBMI", "leptin_ng_ml"))

Maaslin2(dt, pdata, "output/vo2_bmiGroup",
         random_effects = "participant",
         fixed_effects = c("VO2max_ml.kg.min", "bmi_group"))

Maaslin2(dt, pdata, "output/vo2_leptin",
         random_effects = "participant",
         fixed_effects = c("VO2max_ml.kg.min", "leptin_ng_ml"))

Maaslin2(dt, pdata, "output/enterotype",
         random_effects = "participant",
         fixed_effects = "enterotype")

# Timepoint A -----

pdata_a <- pdata[pdata$timepoint=="A",]
dt_a <- dt[pdata$timepoint=="A",]

Maaslin2(dt_a, pdata_a, "output/a_enterotype",
         random_effects = "participant",
         fixed_effects = c("enterotype"))

Maaslin2(dt_a, pdata_a, "output/a_bmi",
         #min_abundance = 10,
         analysis_method = "CPLM",
         fixed_effects = "calculatedBMI")

Maaslin2(dt_a, pdata_a, "output/a_vo2",
         min_abundance = 10,
         analysis_method = "CPLM",
         fixed_effects = "VO2max_ml.kg.min")



