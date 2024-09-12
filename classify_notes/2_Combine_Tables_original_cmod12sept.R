#' This script combines Raven selection tables of individual sound files to import on the
#' sequence of files
#' 
#'  THIS SCRIPT HAS NOT been modified to be imported on the note level WAV files but only for the recordings in a pop read into raven together
#'
#' @param Directory Path of the directory from which Raven selection table to be input
#' @param File_Dur A list of file lengths in seconds for each sound file mentioned in
#' the Raven selection tables
#'
#' @return A combined Raven selection table as a data frame
#' @export
#'
#' @examples Combine_Tables("C:/Desktop/Test_data", c(1204.23,1260.15,223.2,45.25))
#'

##function to combine selection tables is given below
Combine_Tables <- function(List.Raven.Sel, Directory = NULL, File_Dur = NULL,
                           All.Raven.Sel = FALSE) {

  if (is.null(Directory)){
    Directory <- getwd()
  }

  if (All.Raven.Sel == TRUE){
    List.Raven.Sel <- list.files(path = getwd(), pattern = ".*.txt")
  }

  Number_of_files <- length(List.Raven.Sel)

  File_Dur <- as.array(File_Dur)

  #reading first file in the list
  notefile1 <- List.Raven.Sel[1]
  Note_Selections1 <-
    as.data.frame(readr::read_tsv(notefile1, col_names = T,
                                  show_col_types = FALSE))
  Note_Selections1$`Song No.` <- as.numeric(Note_Selections1$`Song No.`)

  Time_diff1 <- 0

  #Clubbing all the files into one raven importable table
  for (x in c(2:(Number_of_files))){

    notefile2 <- List.Raven.Sel[x]
    Previous_songs <- length(unique(Note_Selections1$`Song No.`))
    Time_diff2 <- Time_diff1 + as.numeric(File_Dur[x-1])

    Note_Selections2 <-
      as.data.frame(readr::read_tsv(notefile2, col_names = T,
                                    show_col_types = FALSE))
    Note_Selections2$`Song No.` <-
      as.numeric(Note_Selections2$`Song No.`) + Previous_songs
    Note_Selections2$`Begin Time (s)` <-
      Note_Selections2$`Begin Time (s)` + Time_diff2
    Note_Selections2$`End Time (s)` <-
      Note_Selections2$`End Time (s)` + Time_diff2

    clubfile <- rbind(Note_Selections1,Note_Selections2)

    Note_Selections1 <- clubfile
    Time_diff1 <- Time_diff2
  }

  #set the selection numbers
  clubfile[,1] <- c(1:nrow(clubfile))

  #convert song number format from '1' to '001'
  clubfile$`Song No.` <- as.factor(clubfile$`Song No.`)
  clubfile$`Song No.` <- formatC(clubfile$`Song No.`,x,flag="0",width=4)
  clubfile$`Selection` <- formatC(clubfile$`Selection`,x,flag="0",width=4)
  clubfile$`Begin File` <- gsub(".wav", "", clubfile$`Begin File`)
  clubfile$`Begin File` <- paste0(clubfile$`Begin File`, "_", clubfile$`Selection`, ".WAV")
  clubfile$`File Offset (s)`<- 0.05
  clubfile <- clubfile[,-c(12,14)]
  return(clubfile)
}


### GET A LIST OF SELECTION TABLEES IN THE DIRECTORY

rm(list = ls())
Directory = "E:/Acoustics/ISBE_Oct24/test-pop-selections-ANAIM"

##set directory path
setwd(Directory)

##get a list of all the text
list_of_files = list.files(pattern = "\\.txt$")

##GET A LIST OF SONG FILE DURATIONS OF ALL SELECTION TABLES FROM THE WAVE FILES IN THE DIRECTORY
library(tuneR)
# Get the list of WAV files
wav_files <- list.files(path = Directory, pattern = "*.wav", full.names = TRUE)

# Calculate the sound lengths using lapply()
sound_lengths <- lapply(wav_files, function(x) {
  one_wav <- readWave(x)
  round(length(one_wav@left) / one_wav@samp.rate, 2)
})

# Convert the list to a string
sound_lengths_string <- paste(unlist(sound_lengths), collapse = ", ")

# Print the string of sound lengths AND PASTE IT IN THE File_Dur COLUMN BELOW
print(sound_lengths_string)


## CALL THE FUNCTION TO COMBINE TABLES

result <- Combine_Tables(List.Raven.Sel = list_of_files, 
                         Directory = "D:/ca/ISBE_Oct24/test-pop-selections-ANAIM", 
                         File_Dur = c(1687.81, 94.79, 277.19, 215.44, 130.43, 527.42, 248.02, 110.21, 300.08, 1921.85, 356.99, 2234.01, 972.13, 198.06, 622.58, 301.6, 106.68, 694, 254.7, 466.38),
                         All.Raven.Sel = FALSE)

write.table(result,"merged-ANAIM-notes-v1.txt", sep = "\t", row.names = F, quote = F) 

