library(rprojroot)
root<-rprojroot::find_root(".git/index")

setwd(file.path(root,"exp/exp16-exploreBPpet"))

library(ggplot2)
library(pheatmap)
library(dplyr)
library(ggpubr)

dat0 <- read.csv("/home/data/refined/microbiome2brain/PET/PET_MPPF_binding/AAL_values_n4_Oct302020.csv", row.names = 1)

dat <- t(dat0)
long.dat <- reshape2::melt(dat)
colnames(long.dat) <- c("id", "roi", "value")


cl <- data.frame(id = rownames(dat),
                 participant = gsub("15200", "", rownames(dat)))
rownames(cl) <- cl$id


### Heatmap -----
pheatmap::pheatmap(dat0, scale = "row", filename = "output/MPPF_heatmap.pdf", height = 14)
pheatmap::pheatmap(dat0, scale = "column", filename = "output/MPPF_heatmap_colScale.pdf", height = 14)
pheatmap::pheatmap(dat0, scale = "none", filename = "output/MPPF_heatmap_noScale.pdf", height = 14)





### Graphs -----


cl$group <- ifelse(cl$participant %in% c("EN05", "FR01", "FR02"), "high_FvsN_S1", "low_FvsN_S2")

long.dat <- dplyr::left_join(long.dat, cl)

# long.dat <- long.dat %>%
#   group_by(participant, visit, contrast) %>%
#   mutate(scaled_Tvalue = scale(Tvalue))

### Histograms ----


p000 <- ggplot(long.dat, aes(x=value))+
  geom_histogram() + 
  theme_bw() +
  facet_grid(~id) +
  #geom_vline(xintercept = c(-2.3, 2.3), color = "red", linetype = "dashed") +
  geom_vline(xintercept = 0, color = "red") +
  ggtitle("MPPF binding, all ROI")

ggsave("output/MPPF_histogram.pdf",p000, width = 12)


p0 <- ggplot(subset(long.dat, roi %in% roi[grep("Hippocamp", roi)]), 
       aes(x= visit, y=value, group = participant,color=participant)) +
  geom_line() +
  facet_grid(group~roi) +
  theme_bw() +
  ggtitle("Food vs Neutral contrast")

ggsave("output/hippocamp_group_FoodvsNeutral.pdf", p0, width = 20, height = 8)

# p0_scaled <- ggplot(subset(long.dat, roi %in% roi[grep("hippocamp", roi)] & contrast =="FvsN"), 
#              aes(x= visit, y=scaled_Tvalue, group = participant,color=participant)) +
#   geom_line() +
#   facet_grid(group~roi) +
#   theme_bw() +
#   ggtitle("Scaled T - Food vs Neutral contrast")
# 
# ggsave("output/scaled_hippocamp_group_FoodvsNeutral.pdf", p0_scaled, width = 20, height = 8)


p1 <- ggplot(subset(long.dat, roi %in% roi[grep("amygdala", roi)] & contrast =="FvsN"), 
             aes(x= visit, y=Tvalue, group = participant,color=participant)) +
  geom_line() +
  facet_grid(group~roi) +
  theme_bw() +
  ggtitle("Food vs Neutral contrast")

ggsave("output/amygdala_group_FoodvsNeutral.pdf", p1, width = 20, height = 8)

# p1_scaled <- ggplot(subset(long.dat, roi %in% roi[grep("amygdala", roi)] & contrast =="FvsN"), 
#              aes(x= visit, y=scaled_Tvalue, group = participant,color=participant)) +
#   geom_line() +
#   facet_grid(group~roi) +
#   theme_bw() +
#   ggtitle("Scaled T- Food vs Neutral contrast")
# 
# ggsave("output/scaled_amygdala_group_FoodvsNeutral.pdf", p1_scaled, width = 20, height = 8)
# 


# sig.scaled.FvsN <- subset(long.dat, contrast =="FvsN" & abs(scaled_Tvalue) > 2.3) %>%
#   select(participant, roi)
# 
# 
# sig.scaled.FvsN <- subset(long.dat, contrast =="FvsN" & abs(scaled_Tvalue) > 2.3) %>%
#   select(participant, roi)

pdf("output/ROI_group.pdf",width = 20, height = 20)
for(i in unique(long.dat$contrast)){
p2 <- ggplot(subset(long.dat, contrast == i), 
             aes(x= roi, y=Tvalue, group = visit,color=visit)) +
  geom_line() +
  theme_bw() +
  ylim(-6, 6) +
  geom_hline(yintercept = c(-2,2), color = "red", linetype = "dashed") +
  facet_grid(~participant) +
  ggtitle(paste0(i, " contrast"))+
  coord_flip()
print(p2)
}
dev.off()

# pdf("output/ROI_group_scaled.pdf",width = 20, height = 20)
# for(i in unique(long.dat$contrast)){
#   p2 <- ggplot(subset(long.dat, contrast == i), 
#                aes(x= roi, y=scaled_Tvalue, group = visit,color=visit)) +
#     geom_line() +
#     theme_bw() +
#     ylim(-6, 6) +
#     geom_hline(yintercept = c(-2,2), color = "red", linetype = "dashed") +
#     facet_grid(~participant) +
#     ggtitle(paste0(i, " contrast - scaled Tvalue"))+
#     coord_flip()
#   print(p2)
# }
# dev.off()


### sig Food vs Neutral

library(ggplot2)
library(dplyr)
library(scales)
library(ggradar)
#dat2 <- as.data.frame(t(dat1))

pdf("output/T_heatmap_sig_roi.pdf", width = 20)

for (i in unique(long.dat$contrast)){ 
  dat22 <- subset(long.dat, contrast == i) %>%
    select(id, Tvalue, roi) %>%
    mutate(sel.roi = abs(Tvalue) > 2) 
  dat23 <- dat22 %>%
    reshape2::dcast(id~roi, value.var = "Tvalue") %>%
    tibble::column_to_rownames("id")%>%
    select(dat22$roi[dat22$sel.roi])


  for (j in unique(cl$participant)){
    p <- tibble::rownames_to_column(dat23, "id") %>% 
      as_tibble() %>%
      mutate_at(vars(-id),rescale) %>%
      subset(id %in% id[grep(i, id)]) %>%
      #select(1:20) %>%
      ggradar(base.size = 4, grid.label.size = 4, axis.label.size = 3, legend.position = "bottom")
    ggsave(paste0("output/radar/",i, "_", j, "_radar_sig.pdf"),p, width = 10, height = 8)
  }


  sig.roi <- table(as.character(dat22$roi[dat22$sel.roi]), dat22$id[dat22$sel.roi])
  
  sig.roi <- sapply(dat23, function(x){
    x <- ifelse(abs(x)<2, 0, x)
    return(x)
  })
  rownames(sig.roi) <- rownames(dat23)
  
  long.sig.roi <- reshape2::melt(sig.roi)
  colnames(long.sig.roi) <- c("roi", "id", "value")
  
  limit <- max(abs(long.sig.roi$value)) * c(-1, 1)
  
  gg <- ggplot(long.sig.roi) + 
    geom_tile(aes(x=id, y=roi, fill=value), color="#7f7f7f") + 
    scale_fill_gradient2(high = muted("red"), low = muted("blue"), limit = limit) +
    coord_equal() +
    labs(x=NULL, y=NULL) + 
    theme_bw() + coord_flip() + 
    theme(panel.grid=element_blank()) + 
    theme(panel.border=element_blank(), axis.text.x = element_text(angle = 90)) +
    ggtitle(paste0("Significant T-values - ", i))
  print(gg)
}
dev.off()

### radar all regions --------
# devtools::install_github("ricardo-bion/ggradar", 
#                          dependencies = TRUE)


library(ggradar)

for (i in unique(cl$participant)){
  q <- tibble::rownames_to_column(dat2, "id") %>% 
    as_tibble() %>%
    mutate_at(vars(-id),rescale) %>%
    subset(id %in% id[grep(i, id)]) %>%
    select(c(1,21:40)) %>%
    ggradar(base.size = 4, grid.label.size = 4, axis.label.size = 3, legend.position = "bottom")
  ggsave(paste0("output/",i, "radar21_40.pdf"),q, width = 10, height = 8)
}


for (i in unique(cl$participant)){
  q <- tibble::rownames_to_column(dat2, "id") %>% 
    as_tibble() %>%
    mutate_at(vars(-id),rescale) %>%
    subset(id %in% id[grep(i, id)]) %>%
    select(c(1,41:65)) %>%
    ggradar(base.size = 4, grid.label.size = 4, axis.label.size = 3, legend.position = "bottom")
  ggsave(paste0("output/",i, "radar41_65.pdf"),q, width = 10, height = 8)
}


for (i in unique(cl$participant)){
  q <- tibble::rownames_to_column(dat2, "id") %>% 
    as_tibble() %>%
    mutate_at(vars(-id),rescale) %>%
    subset(id %in% id[grep(i, id)]) %>%
    select(c(1,66:90)) %>%
    ggradar(base.size = 4, grid.label.size = 4, axis.label.size = 3, legend.position = "bottom")
  ggsave(paste0("output/",i, "radar66_90.pdf"),q, width = 10, height = 8)
}


for (i in unique(cl$participant)){
  q <- tibble::rownames_to_column(dat2, "id") %>% 
    as_tibble() %>%
    mutate_at(vars(-id),rescale) %>%
    subset(id %in% id[grep(i, id)]) %>%
    select(c(1,91:117)) %>%
    ggradar(base.size = 4, grid.label.size = 4, axis.label.size = 3, legend.position = "bottom")
  ggsave(paste0("output/",i, "radar91_117.pdf"),q, width = 10, height = 8)
}
