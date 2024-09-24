rm(list = ls())

library(dplyr)
setwd("C:/Users/chiti/OneDrive/Desktop/classified-notes-selection-tables/n-grams")
dir()


combine <- 
  utils::read.csv("combine_5pops_ngrams_isbe.csv",
                  header = T)
names(combine)

combine = combine[,c(1,4,5,6)]
combine2 <- subset(combine, Length <= 3)
combine2 <- subset(combine2, Occurance > "1")
combine2 <- dplyr::distinct(combine2)
combine2 <- combine2[!duplicated(combine2), ]
combine2$n_Gram <- gsub(" ", "_", combine2$n_Gram)

#rep_structure(Inputfile = combine2, k.max = 5, nboot =10,
#              optimal.k.method = "gap_stat")
################################################################################
#function
#rep_structure <- function(Inputfile, k.max = 10, nboot = 100,
#                          optimal.k.method = c("silhouette", "wss", "gap_stat")){
  
  
  Inputfile=combine2
Inputfile_aggregated <- Inputfile %>%
  dplyr::group_by(n_Gram, Individual) %>%
  dplyr::summarise(Occurance = sum(Occurance)) %>%
  dplyr::ungroup()

data_wide <- tidyr::spread(Inputfile_aggregated, Individual, Occurance)
# colnames(data_wide)
  #data_wide <- tidyr::spread(Inputfile, Individual, Occurance)
  data_wide[is.na(data_wide)] <- 0
  


data_wide$n_Gram <- gsub(" ", "_", data_wide$n_Gram)
ngram_names <- data_wide$n_Gram
  
#  duplicates <- data_wide$n_Gram[duplicated(data_wide$n_Gram)]
#  print(duplicates)
  
  data_wide <- data_wide[,c(-1)] #depends on the number of individuals all together
rownames(data_wide) <- ngram_names

  example_NMDS=vegan::metaMDS(t(data_wide),k=20,trymax=1000)

  A <- example_NMDS[["species"]]
  rownames(A) <- ngram_names

  k.max = 5
  nboot = 10
  optimal.k.method = "gap_stat"
  
  for (i1 in 2:k.max) {
    km.res <- stats::kmeans(A, centers = i1, nstart = 1)
    
    B <- base::as.data.frame(km.res$cluster)
    B[,2] <- row.names(A)
    row.names(B) <- NULL
    colnames(B) <- c("cluster", "n_Gram")
    
    # Ensure merge produces data
    C <- merge(B, Inputfile_aggregated, by = "n_Gram")
    if (nrow(C) == 0) {
      warning("No matching data found in merge for k =", 3)
    }
    C$cluster <- base::as.character(C$cluster)
    
    # Create D and check if non-empty after filtering out NA
    D <- base::as.data.frame(unique(C$Individual))
    colnames(D) <- c("Individual")
    
    # Remove NA individuals
    D <- base::as.data.frame(unique(C$Individual))
    
    colnames(D) <- c("Individual")
    for ( i2 in 1:nrow(D)){
      for (i3 in 1:i1){
        D[i2,1+i3] <-
          nrow(subset(subset(C, Individual == as.character(D[i2,1])),
                      cluster == i3))
      }
      
    }
    
    D1 <- D[, c(2:ncol(D))]
    
    # Set rownames only if Individual is non-NA
    rownames(D1) <- D$Individual
    
    D1 <- D1[sort(rownames(D1)), ]
    D1 <- D1[, base::rev(base::order(colSums(D1)))]
    
    data_percentage <- base::apply(t(D1[, c(1:i1)]), 2, function(x) {x/sum(x, na.rm = TRUE)})
    T_dp <- as.data.frame(t(data_percentage))
    
    col1 <- RColorBrewer::brewer.pal(9, "Set1")
    col2 <- RColorBrewer::brewer.pal(8, "Pastel2")
    col <- c(col1, col2)
    
    graphics::barplot(t(T_dp), border = NA, space = 0.0, col = col, cex.names = 0.5, xlab = "Individuals", las = 2, xaxt = "n", main = paste("k = ", i1, sep = ""))
    graphics::abline(v = c(10, 14, 19, 29, 39), col = "black", lwd = 2) # Change accordingly
    graphics::text(c(5, 12, 16.5, 24, 34, 40.5), 0.05, c("ANAIM", "BANDR", "BERJM", "HIGHW", "KODAI"), cex = 1, col = "black") # Change accordingly
  }
  
  
  return(factoextra::fviz_nbclust(A, kmeans, nboot = nboot, k.max = k.max,
                                  method = optimal.k.method))
}

################################################################################
#lines to change
#combine1 <- combine[,c(1:4)]


combine2 <- subset(combine, Length <= 3)
combine2 <- subset(combine2, Occurance > "1")
combine2 <- dplyr::distinct(combine2)
combine2 <- combine2[!duplicated(combine2), ]


rep_structure(Inputfile = combine2, k.max = 5, nboot =10,
              optimal.k.method = "gap_stat")

