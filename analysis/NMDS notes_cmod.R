rm(list=ls())

library(tidyr)
library(vegan)
library(dplyr)
set.seed

setwd("C:/Users/chiti/OneDrive/Desktop/classified-notes-selection-tables/n-grams")
dir()
anaim = utils::read.csv("ANAIM-n-grams.csv",header = T)
bandr = utils::read.csv("BANDR-n-grams.csv",header = T)
berjm = utils::read.csv("BERJM-n-grams.csv",header = T)
highw = utils::read.csv("HIGHW-n-grams.csv",header = T)
kodai = utils::read.csv("KODAI-n-grams.csv",header = T)

combine= rbind(anaim,bandr,berjm,highw,kodai)

write.csv(combine, "combine_5pops_ngrams_isbe.csv", row.names = FALSE)

indv = unique(combine$Individual)

#col = c("#fcffa4",  "#c73e4c","#0c0826","#f2e661",  "#dd513a",
#        "#ae305c",  "#f8850f",  "#ed6925",  "#240c4f",  "#fac62d",  "#000004","#fca50a")
#lab = c("S01","S08","S15","S02","S07","S09","S05","S06","S14","S03","S16","S04")
group <- c("ANAIM","ANAIM","ANAIM","ANAIM","ANAIM",
           "BANDR","BANDR","BANDR","BANDR","BANDR","BANDR","BANDR","BANDR","BANDR",
           "BERJM","BERJM","BERJM","BERJM","BERJM","BERJM","BERJM","BERJM","BERJM",
           "HIGHW","HIGHW","HIGHW","HIGHW","HIGHW",
           "KODAI","KODAI","KODAI","KODAI","KODAI", "KODAI")

names(combine)

combine1 <- combine[,c(1,5,6,4)] 

combine2 <- subset(combine1, Length == 2 )

combine2 <- subset(combine2, Occurance > "1")

combine2 <- dplyr::distinct(combine2)

# First, aggregate the data to handle duplicates
combine3 <- combine2 %>%
  group_by(n_Gram, Individual, Length) %>%
  summarise(Occurance = sum(Occurance), .groups = 'drop')

#data_wide <- spread(combine2, Individual, Occurance)
data_wide <- tidyr:::spread.data.frame(combine3, Individual, Occurance)

data_wide[is.na(data_wide)] <- 0
rownames(data_wide) <- data_wide$n_Gram
data_wide <- data_wide[,c(3:ncol(data_wide))]

example_NMDS=vegan::metaMDS(t(data_wide),k=3,trymax=2000)

#vegan::ordiplot(example_NMDS,type="n")
#orditorp(example_NMDS,display="species",col="black",air=0.01, pch = 1, cex  = 0.1)
#vegan::orditorp(example_NMDS,display="sites",cex=1.25,air=0.01, col = col, labels = lab)


ordiplot(example_NMDS,type="n")
orditorp(example_NMDS,display="sites",cex=0.75,air=0.01)
ordiellipse(example_NMDS,groups=group,draw="polygon",col= c("#4575B4","#FDAE61","#F46D43","#1b7837","#A50026"),label=F,
            conf = 0.75, alpha = 0.55)
#"#E41A1C" "#377EB8" "#4DAF4A" "#984EA3" "#FF7F00" "#FFFF33"
#"BANDR", "ERAVI", "GRASS", "HIGHW", "KODAI", "MATHI"


#sites <- example_NMDS[["points"]]
#sites <- cbind(as.data.frame(lab),sites)

#plotgg1 <- ggplot2::ggplot() +  
#  ggplot2::xlab("NMDS1") +
#  ggplot2::ylab("NMDS2") +   
#  ggplot2::geom_point(data=sites, col = col,
#                      ggplot2::aes(x=MDS1, y=MDS2, colour=lab), 
#             size=5)+
#  ggplot2::theme_bw()+
#  ggplot2::geom_text(data=sites, col = col,
#                     ggplot2::aes(x=MDS1, y=MDS2,label=lab),hjust=0, vjust=1.75)+
#  ggplot2::xlim(-0.5,0.75)+
#  ggplot2::ylim(-0.4,0.4)

#plotgg1
