rm(list = ls())
# Load necessary libraries
library(dplyr)
library(tidyr)

# Define the directory containing the text files
input_dir <- "E:/Acoustics/Hand-held-only-ringed/Set_3_annotations/0_original_selections/"
output_dir <- "E:/Acoustics/Hand-held-only-ringed/Set_3_annotations/1_song_number"  # You can use a different output directory if needed

# Get a list of all text files in the directory
file_list <- list.files(path = input_dir, pattern = "\\.txt$", full.names = TRUE)

# Loop through each file in the list
for (file_path in file_list) {
  # Extract the base filename (without the extension)
  base_name <- tools::file_path_sans_ext(basename(file_path))
  
  # Construct the output filename
  output_filename <- paste0(base_name, "_songs.txt")
  output_path <- file.path(output_dir, output_filename)
  
  # Read the TXT file
  note_level <- read.table(file_path, header = TRUE, sep = "\t", check.names = FALSE)
  
  # Convert start_time and end_time to numeric if they are not already
  note_level <- note_level %>%
    mutate(
      `Begin Time (s)` = as.numeric(`Begin Time (s)`),
      `End Time (s)` = as.numeric(`End Time (s)`)
    )
  
  # Sort data by Begin Time (s)
  note_level <- note_level %>%
    arrange(`Begin Time (s)`)
  
  # Calculate time differences between consecutive selections
  note_level <- note_level %>%
    group_by(`Song No.`) %>%
    mutate(previous_end_time = lag(`End Time (s)`),
           time_gap = `Begin Time (s)` - previous_end_time) %>%
    ungroup()
  
  # Define a threshold for time gap (e.g., 2 seconds)
  threshold <- 1
  
  # Create a new group ID based on the time gap threshold
  note_level <- note_level %>%
    group_by(`Song No.`) %>%
    mutate(`Song No.` = cumsum(ifelse(is.na(time_gap) | time_gap > threshold, 1, 0))) %>%
    ungroup()
  
  # Delete last two columns (ensure you are removing the correct columns)
  note_level <- note_level[ , -c(15, 16)]
  
  # Write the updated data frame to a new file
  write.table(note_level, output_path, sep = "\t", row.names = FALSE, quote = FALSE)
  
  # Print a message indicating the file has been processed
  cat("Processed and saved:", output_path, "\n")
}

