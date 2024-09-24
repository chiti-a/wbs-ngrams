rm(list = ls())

library(ggVennDiagram)
#library(ggvenn)
library(ggplot2)

combine <- utils::read.csv("combine_5pops_ngrams_isbe.csv",header = T)

one_gram = subset(combine, Length == 1)
two_gram = subset(combine, Length == 2)
three_gram = subset(combine, Length == 3)
four_gram = subset(combine, Length == 4)

one_gram_list <- split(one_gram$n_Gram, one_gram$Population)

two_gram_list <- split(two_gram$n_Gram, two_gram$Population)

three_gram_list <- split(three_gram$n_Gram, three_gram$Population)

four_gram_list <- split(four_gram$n_Gram, four_gram$Population)

ggVennDiagram(four_gram_list[1:5], label_alpha = 0) + scale_fill_gradient(low="grey90",high = "red")

ggVennDiagram(one_gram_list, label_alpha = 0) +
  scale_fill_manual(values = c("#4575B4", "#FDAE61", "#F46D43", "#1b7837", "#A50026"))
              
ggVennDiagram(one_gram_list, label_alpha = 0) +
  scale_fill_manual(values = c(
    "ANAIM" = "#4575B4", 
    "BANDR" = "#FDAE61", 
    "BERJM" = "#F46D43", 
    "HIGHW" = "#1b7837", 
    "KODAI" = "#A50026"
  ))              
              

