rm(list = ls())
#packages required
library(readr)
library(progress)
library(Rraven)
library(data.table)
#library(foreach)
#library(doParallel)

#Note_segment("Combine-notes-RUGG-2021.txt", tbuffer = 0.1, fbuffer = 500) #ignore

## DATA ########################################################################

#Set dir
setwd("C:/Users/chiti/ISBE_2024/ANAIM-notes")
Dir <- "C:/Users/chiti/ISBE_2024/ANAIM-notes"
setwd(Dir)
#dir()
#training and test data file names
#these are selection tables created for individual note segments
train_data <- "manually_classified_data_ca_v2.txt"
test_data <- "ANAIM-classified-set1-forclass.txt"
#names(test_data)
#names(test_data_warbler)
## -----------------------------------------------------------------------------

## Read Selection Tables #######################################################
#import training and test data
train_data_warbler <- Rraven::imp_raven(path = Dir,warbler.format = TRUE,
                                    files = train_data)
test_data_warbler <- Rraven::imp_raven(path = Dir,warbler.format = TRUE,
                                        files = test_data)

# Construct the full file path
file_path_train <- file.path(Dir, train_data)
file_path_test <- file.path(Dir, test_data)
# Read the data ensure both files have the same columns, if not then add a correlation column for training, and Note type and Correlation for test
train_data_complete <- fread(file_path_train)
test_data_complete <- fread(file_path_test)
#test_data_complete <- cbind(test_data_complete,NA,NA)
#colnames(test_data_complete)[c(ncol(test_data_complete)-1,
#                               ncol(test_data_complete))] <- 
#  c("Note types","Correlation")
## -----------------------------------------------------------------------------

##Ensure that both test data combine and train data combine have the same column names

## Note Types ##################################################################
#extract note types from training data
train_types <- train_data_complete$`Note types`
unique_train_types <- unique(train_types)

#create combinations of possible note type names - A01, A02, A03,...
combinations <- expand.grid(LETTERS, sprintf("%02d", 1:30))
combinations_vec <- paste(combinations$Var1, combinations$Var2, sep = "")

#Creat a vector of unique note type names not present in the training data
unique_new_types <- setdiff(combinations_vec, unique_train_types)
rm(combinations_vec, combinations)

##------------------------------------------------------------------------------


## Set Parameters ##############################################################
#set lower and upper thresholds for correlatio values
lowlim <- 0.8 #if there is no correlation value above this, it will create a new note type
highlim <- 0.97 #if any correlation value is above this, it will stop the look and assign the corresponging note type class

#Test_N <- nrow(test_data_warbler) #total number of test data notes

Test_N<-nrow(test_data_warbler[1:5000,]) ## TEST FOR A SUBSET OF 

## -----------------------------------------------------------------------------


## Progress bar ################################################################

#create progress bar
pb <- progress::progress_bar$new(
  format = "  [:bar] :current/:total (:percent) elapsed: :elapsed eta: :eta",
  total = Test_N, clear = FALSE, width = 60
)

## -----------------------------------------------------------------------------


## FOR loop ####################################################################

#Run a loop to correlate every note from test data to every note in training data
#to classify them into a category

for (Test_n in 1:Test_N){
  corr_vec <- c() #empty vector for correlation values
  Train_N <- nrow(train_data_warbler) #It can change automatically if it adds a new type meanwhile
  
  for (Train_n in 1:Train_N){
    correlation <- suppressMessages(suppressWarnings(
      warbleR::cross_correlation(
        X = rbind(train_data_warbler[Train_n,],
                  test_data_warbler[Test_n,]), pb = FALSE,
        wl = 512, ovlp = 70, bp = c(2, 10)))) #pairwise correlation
    
    corr <- correlation[2,1] #extract the value of A x B correlation from the 2(AB)x2(AB) matrix
    
    if (corr >= highlim){
      corr_vec <- c(corr_vec,corr)
      break #break loop if the value is above upper limit
    }
    
    corr_vec <- c(corr_vec,corr) #assign max correlation
  }
  
  if (max(corr_vec) >= lowlim){
    test_data_complete$`Note types`[Test_n] <- 
      train_data_complete$`Note types`[which(corr_vec == max(corr_vec))] #assign the note type of max correlation value
    test_data_complete$Correlation[Test_n] <- max(corr_vec) #corresponding correlation value
    
   } else { #if all correlation values less that lower cutoff
     test_data_complete$`Note types`[Test_n] <- unique_new_types[1] #assign a new note type
     test_data_complete$Correlation[Test_n] <- 1 #correlation value of 1 (max possible)
     
     train_data_warbler <- rbind(train_data_warbler,
                                 test_data_warbler[Test_n,]) #add new type to the training data
     train_data_complete <- 
       rbind(train_data_complete,
             test_data_complete[Test_n,1:ncol(test_data_complete)]) #add new type to the training data
     
     unique_new_types <- unique_new_types[-1] #update the vector of new note type names
    }
  pb$tick() #progress bar
}
## -----------------------------------------------------------------------------

#ignore next part for the main classification code, need to modify based on requirements
#check if the classification works
table(train_data_complete$`Note types`[c(101:300)] ==
        test_data_complete$`Note types`)

hist(test_data_complete$Correlation)
length(unique(train_data_complete$`Note types`[c(101:300)]))
