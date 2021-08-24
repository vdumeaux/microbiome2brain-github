
library(rprojroot)
root<-rprojroot::find_root(".git/index")

setwd(file.path(root,"exp/exp06-readMicrobiome"))   

library(dplyr)
source(file.path(root, "src/functions.R"))


datdir <- "/home/data/refined/microbiome2brain/microbiome/01-trimmed"

bracken.files <-list.files(datdir, pattern="^reads.bracken$", full.names = TRUE, recursive = TRUE) 
sn <- list.dirs(datdir, recursive = FALSE, full.names = FALSE)
sn <- sn[-grep("metaphlan", sn)]
sn <- unlist(lapply(strsplit(sn, "_"), "[", 1))
# participant <- substr(sn,1,2)
# timepoint <- substr(sn,3,3)

# Load the data

res <- lapply(seq_along(bracken.files), function(x){
  dat <- read.csv(bracken.files[x], header=TRUE, sep = "\t",
                  col.names = c("name", "tax_id", "tax_lvl", paste0(sn[x],"_assigned"), paste0(sn[x],"_added"), paste0(sn[x],"_est"), paste0(sn[x],"_fraction")))
  datsub <- dat[, c(2,6,7)]
  return(datsub)
})


######################################
# NCBI Taxonomy download
#   We assume that the taxonomy has been download during the Kraken processing
#   e.g. via  kraken2-build --download-taxonomy -use-ftp  --db $RES
#
######################################


nodes <- read.csv("/home/shared/data/db/kraken2_microbiome/taxonomy/nodes.dmp", header=FALSE, sep = "\t")
nodes <- nodes[, c(1,3,5,7,9)]
colnames(nodes) <- c( "tax_id", "parent", "rank", "embl_code", "division_id" )

nms <- read.csv("/home/shared/data/db/kraken2_microbiome/taxonomy/names.dmp", header=FALSE, sep = "\t")
nms <- nms[, c(1,3,5,7)]
colnames(nms) <- c( "tax_id", "name_txt", "unique name", "name_class" )

divisions <- read.csv("/home/shared/data/db/kraken2_microbiome/taxonomy/division.dmp", header=FALSE, sep = "\t")
divisions <- divisions[, c(1,3,5,7)]
colnames(divisions) <- c( "id", "code", "name", "comments" )

######################################
test <- res %>%
  purrr::reduce(full_join, by="tax_id")

tree <- nodes

tree <- left_join(tree, test, by="tax_id")

tree[, -c(1:5)] <- tree[, -c(1:5)] %>% replace(is.na(.), 0)


#### Vanessa stops here ############



#####################################
# Set parent of root to NA to avoid
# infinite recursion.
######################################

tree[1, "parent"] <- NA

sample_cols <- colnames(tree)[c(seq(6, 32, by=2))]  # just make it easier 
# percolate the reads at the leaves up through the tree to the root.


tree$isLeaf <- TRUE
lapply( 1:nrow(tree), FUN = function(i) {
  print(i)
  parent <- tree[i, "parent"]
  tree[t2i(parent), "isLeaf"] <<- FALSE
})

to_remove <- lapply( 1:nrow(tree), FUN = function(i) {
  print(i)
  if ((tree[i, "isLeaf"]) & (all(tree[i,sample_cols]==0))) 
    return(TRUE)
  return(FALSE)
})

leaf.tree <- tree

to_remove <- unlist(to_remove)
tree <- tree[ !to_remove, ]

void <- percolate(1, sample_cols)
# save(tree, file = "temporary_after_percolate2.RData")
# 
# #########################
# #
# # add names directly to each row of the tree. 
# # use the scientific name if it exists.
# #
# ##########################

for (i in 1:nrow(tree)) {
  tmp <- which(nms$tax_id == tree[i, "tax_id"])
  if (length(tmp) == 0) next
  if (length(tmp) == 1) { tree[i, "name"] <- as.character(nms[tmp, "name_txt"]); next }
  tmp2 <- which( nms[tmp, "name_class"] == "scientific name")
  if (length(tmp2) == 0)  { tree[i, "name"] <- as.character(nms[tmp[1], "name_txt"]); next }
  if (length(tmp2) == 1) { tree[i, "name"] <- as.character(nms[tmp[tmp2], "name_txt"]); next }
  if (length(tmp2) > 1) { tree[i, "name"] <- as.character(nms[tmp[tmp2[1]], "name_txt"]); next }
  print(tree$name[i, "name"])
}

tree <- tree[ , c( ncol(tree), (ncol(tree)-2), 1:(ncol(tree)-2) )]
save(tree, file = "temporary_after_percolate3.RData")


# ##################################################
# # Now add frequencies.  src/function.R
# ##################################################
# 

#void <- local_frequencies(1, sample_cols)
#void <- global_frequencies( 1, sample_cols )
# 
# ##################################################
# # Now statistical tests. src/function.R
# ##################################################
# 
# void <- multinomial_tree_test(1)
# 
# void <- polarity_test( 1 )
# 
# ##################################################
# # Include the taxon to root path through the tree of life.  
# #       src/functinon.R
# ##################################################

for (i in 1:nrow(tree)) {
  tree[i, "path"] <- unlist( paste( path2root(tree[i, "tax_id"])$name, collapse="."))
}



# rearrange for convenience
# tree <- tree[, c(1:18, 23, 19:22)]


save(tree, file = "/home/data/refined/microbiome2brain/microbiome/R/tree_1.0.RData")
# write.csv(tree, file = "version_1.0.csv")
# 


