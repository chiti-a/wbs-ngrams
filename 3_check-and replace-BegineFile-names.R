##this script is to copy all the file names of the WVA notes generated in 1_Note_segments and past them in the `Begin file` column in the 2_Combine tables script

rm(list = ls())
library(data.table)
setwd("E:/Acoustics/ISBE_Oct24/ANAIM-Notes")
Dir <- "E:/Acoustics/ISBE_Oct24/ANAIM-Notes"
test_data <- "merged-ANAIM-notes-v1.txt"
file_path_test = file.path(Dir, test_data)
test_data_complete <- fread(file_path_test)
##get listr of wav files 
# Get the list of WAV files
wav_files <- list.files(path = Dir, pattern = "*.WAV", full.names = TRUE) ##check if the no of files matches the test_data_complete
list_of_wavs<- as.data.table(wav_files)
list_of_wavs[, `Begin File` := sub(".*/([^/]*)$", "\\1", list_of_wavs)] #prints the entire path of the WAV files

list_of_wavs[, `Begin File` := sub(".*/([^/]*)$", "\\1", wav_files)] #creates another column with only file name

pop_wavs<-list_of_wavs[,-1] #remove path

test_data_complete<-cbind(test_data_complete,pop_wavs) #merge the two tables

colnames(test_data_complete) 

test_data_complete<-test_data_complete[,-10]

colnames(test_data_complete) #check column names to ensure the new begin file is replaced

test_data_complete_final<-test_data_complete[,c(1:9,14,10,12,13)]

colnames(test_data_complete_final)

write.table(test_data_complete_final,"merged-ANAIM-notes-v2.txt",sep = "\t", row.names = F, quote = F)

