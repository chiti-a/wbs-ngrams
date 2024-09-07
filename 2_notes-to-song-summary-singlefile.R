rm(list=ls())
setwd("E:/Acoustics/Hand-held-only-ringed/test-loop/")
library(readr)
library(dplyr)
library(stringr)
dir()


# Define the directory containing the text files
#input_dir <- "E:/Acoustics/Hand-held-only-ringed/test-loop/"
#output_dir <-  input_dir#"E:/Acoustics/Hand-held-only-ringed/test-loop/2-song_summary"  # You can use a different output directory if needed

# Get a list of all text files in the directory
#file_list <- list.files(path = input_dir, pattern = "\\.txt$", full.names = TRUE)

# Loop through each file in the list
#for (file_path in file_list) {
  # Extract the base filename (without the extension)

 # base_name <- tools::file_path_sans_ext(basename(file_path))
  
  # Construct the output filename
 # output_filename <- paste0(base_name, "_song_summary.txt")
 # output_path <- file.path(output_dir, output_filename)
  # Read file
  notes_selections <- read.table("notes_with_songid_updated1.txt", header = TRUE, sep = "\t", check.names = F)
  names(notes_selections)
  
  # Compute metrics
  songs_selection <- notes_selections %>%
    group_by(`Song No.`) %>%
    summarize(
      Selection = "",
      View = "Spectrogram 1",
      Channel = 1,
      `Begin Time (s)` = min(`Begin Time (s)`),
      `End Time (s)` = max(`End Time (s)`),
      `File Offset (s)` = min(`File Offset (s)`),
      `Begin File` = first(`Begin File`),
      `Low Freq (Hz)` = min(`Low Freq (Hz)`),
      `High Freq (Hz)` = max(`High Freq (Hz)`),
      `Delta Time (s)` = max(`End Time (s)`) - min(`Begin Time (s)`),
      `Delta Freq (Hz)` = (max(`High Freq (Hz)`)) - (min(`Low Freq (Hz)`)),
      `Mean Freq (Hz)` = (max(`High Freq (Hz)`) + min(`Low Freq (Hz)`)) / 2,
      `Note Count` = sum(`Channel`),
      `Average Note Delta Time (s)` = mean(max(`End Time (s)`) - min(`Begin Time (s)`)),
      `Average Note Delta Freq (Hz)` = mean(max(`End Time (s)`) - min(`Begin Time (s)`)),
      #`Standard Deviation in Note Mean Freq (Hz)` = sqrt(var((`Low Freq (Hz)` + `High Freq (Hz)`) / 2)),
      #`% duration occupied by notes` = sum(`Delta Time (s)`) / (`End Time (s)` - `Begin Time (s)`),
      `Note Pace (Notes/s)` = sum(Channel) / (max(`End Time (s)`) - min(`Begin Time (s)`)),
      `Note Types` = "",
      CVM = sum(`CVM`) / sum(Channel)
    )
  
  songs_selection <- songs_selection %>% mutate(Selection = row_number())
  
  songs_selection = songs_selection[,-1]
  
  # Write the updated data frame to a new file
  write.table(songs_selection,"SHAL-ANAIM-GGBW-0001-songsummary.txt", sep = "\t", row.names = FALSE, quote = FALSE)
  
  # Print a message indicating the file has been processed
  #cat("Processed and saved:", output_path, "\n")
#}


# Combine all processed files into one dataset
processed_files <- list.files(pattern = ".*_songs.txt")
dataset <- lapply(processed_files, read_tsv) %>%
  bind_rows()

# Write final combined dataset
write_csv(dataset, "final-GRASS-new-cvms.csv")
