################################################################################
## this script is for extracting individual note level WAV files from a recordings
## this script will only run for one WAV recordings file and its corresponding selection table with note level annotations


#JUST ONE SELECTION TABLE - MANUAL - FILE.CHOOSE(READ.TABLE())
#install.packages("Rraven")
#install.packages("tuneR")
#install.packages("seewave")

library(Rraven)
library(seewave)
library(tuneR)

#Used for debug: (to be defined while calling the function)
#' @param File Path and name of Raven Selection table file
#' @param Path A character string indicating the path of the directory in which
#' to look for the 'Raven' selection (text) files. If not provided (default)
#' the function searches into the current working directory.
#' @param Output.Path A character string indicating the path of the directory
#' for output
#' @param tbuffer Time in seconds to be used as buffer while saving sound files
#' @param fbuffer Frequency in Hertz to be used for clipping the sound files

#example: File = "SHAL-ANAIM-UURW-0001.Table.1.selections_songs"
#example: Path = "E:/Trial/Annotation"
#example: Output.Path = "E:/Trial/Output"

rm(list = ls())
setwd("E:/Acoustics/ISBE_Oct24/test-pop-selections-ANAIM") 

dir()


#File="SHAL-ANAIM-GGBW-0001.Table.1.selections_songs.txt"
#Path="D:/ca/manually_classified_data_8sept"
#Output.Path="D:/ca/manually_classified_data_8sept"

#tbuffer=0.05
#fbuffer=500

##below is the function to extract note segments
Note_segment <- function(File, Path, Output.Path, tbuffer=0.05, fbuffer=500) ##set tbuffer (sec) and fbuffer(Hz) and keep it consistent across all recordings
  {
  
  if (is.null(Path)){
    Path = getwd()
  }
  setwd(Path)
  
  if (is.null(Output.Path)){
    dir.create(paste(Path, tools::file_path_sans_ext(File),
                     sep = "/"))
    Output.Path <- paste(Path, tools::file_path_sans_ext(File),
                         sep = "/")
  }
  
    # Inspect the data
names(Selection_table)
  
  Selection_table$`Delta Time (s)` <- Selection_table$`End.Time..s.` - Selection_table$`Begin.Time..s`
  
# Create Sound_files vector
read_sound_files <- Selection_table$`Begin.File`
Sound_files <- file.path(Path, read_sound_files)
  

  Start_time <- Selection_table$`File.Offset..s.`
  End_time <- Selection_table$`File.Offset..s.` + Selection_table$`Delta.Time..s.`
  Total_notes <- nrow(Selection_table)
  
  #print(Total_notes)
  
  for (i in 1:Total_notes) {
    input_sound_file_name <- Sound_files[i]  
    
    print(input_sound_file_name)  # Debug: Check file path
    
    # Debug: Check if the file exists before trying to read it
    if (!file.exists(input_sound_file_name)) {
      stop(paste("Audio file not found:", input_sound_file_name))
    }
    
    # Use tuneR::readWave() to read the file
    input_sound_file <- tuneR::readWave(
      filename = input_sound_file_name, 
      from = Start_time[i] - tbuffer, 
      to = End_time[i] + tbuffer, 
      units = "seconds"
    )
    
    # Filtering Sound
    low.freq <- Selection_table$`Low.Freq..Hz.`[i] - fbuffer
    low.freq <- base::pmax(low.freq, 0)
    high.freq <- Selection_table$`High.Freq..Hz.`[i] + fbuffer
    print(paste("Low frequency:", low.freq))   # Debug: Check low frequency
    print(paste("High frequency:", high.freq)) # Debug: Check high frequency
    
    output_sound_file <- seewave::ffilter(
      input_sound_file,
      f = input_sound_file@samp.rate,
      from = low.freq,
      to = high.freq,
      output = "Wave",
      rescale = TRUE
    )
    
    # Optional: Visualize Spectrogram
    par(mar = c(2, 7, 7, 2))
    seewave::spectro(output_sound_file, f = 44100)
    
    output_file_name <- paste(
      paste(tools::file_path_sans_ext(input_sound_file_name),  # Remove extension
      formatC(Selection_table$Selection[i], width = 4, flag = "0"),  ##add note number extension with 4 char to the filename while saving a note
      sep = "_"),
    ".WAV", sep=""
    )# Add the ".WAV" extension
    
 
    # Save the filtered sound file
    seewave::savewav(output_sound_file,
                     f = input_sound_file@samp.rate,
                     channel = 1,
                     filename = paste(output_file_name, sep = "/"))
    
  }
}

Selection_table <- read.table(file.choose(), header = TRUE, sep = "\t") #select the selection table in the pop up generated
#Calling the function
Note_segment(File="SHAL-ANAIM-YYXX-0005.Table.1.selections_songs.txt",#name of the file to be segmented
             Path="E:/Acoustics/ISBE_Oct24/test-pop-selections-ANAIM",#path to the file to be segmented
             Output.Path="E:/Acoustics/ISBE_Oct24/ANAIM-notes", #save to wd, this is not incorporated in the script yet
             tbuffer=0.05, #time buffer set for each note
             fbuffer=500) #frequency buffer set for each note
  
