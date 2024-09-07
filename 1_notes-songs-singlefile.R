rm(list=ls())
setwd("E:/Acoustics/Hand-held-only-ringed/test-loop/")

# Load necessary libraries
library(dplyr)
library(tidyr)

dir()

# Read the TXT file
# Adjust the delimiter according to your file format (e.g., "\t" for tab-separated, " " for space-separated)
note_level <- read.table("SHAL-GRASS-UR01-0001.Table.1.selections.txt", header = TRUE, sep = "\t", check.names = FALSE)

# Inspect the data (optional)
names(note_level)

##rename columns
#note_level <- note_level %>% rename(
#  Begin_time = Begin.Time..s.,
#  End_time = End.Time..s.,
#  Low_freq = Low.Freq..Hz.,
#  High_freq = High.Freq..Hz.,
#  Delta_freq = Delta.Freq..Hz.,
#  Delta_time = Delta.Time..s.
#)

# Convert start_time and end_time to numeric if they are not already (assuming time in seconds)
note_level <- note_level %>%
  mutate(
    `Begin Time (s)` = as.numeric(`Begin Time (s)`),
    `End Time (s)` = as.numeric(`End Time (s)`)
  )

# Sort data by song_id and start_time
note_level <- note_level %>%
  arrange(`Begin Time (s)`)

# Calculate time differences between consecutive selections
note_level <- note_level %>%
  group_by(`Song No.`) %>%
  mutate(previous_end_time = lag(`End Time (s)`),
         time_gap = `Begin Time (s)` - previous_end_time) %>%
  ungroup()


# Inspect time gaps
head(note_level)

# Define a threshold for time gap (e.g., 2 seconds)
threshold <- 1

# Create a new group ID based on the time gap threshold this is the song id
note_level <- note_level %>%
  group_by(`Song No.`) %>%
  mutate(`Song No.` = cumsum(ifelse(is.na(time_gap) | time_gap > threshold, 1, 0))) %>%
  ungroup()

# delete last two columns
note_level=note_level[,-c(15,16)]

write.table(note_level, "SHAL-GRASS-UR01-0001.Table.1.selections.txt_songs.txt", sep = "\t", row.names = FALSE, quote = FALSE)

##song level summary df
song_level <- note_level %>%
  group_by(`View`,`Channel`,`Begin.File`,`group_id`,`File.Offset..s.`) %>%
  summarise(
    Begin_time = min(`Begin.Time..s.`, na.rm = TRUE),
    End_time = max(`End.Time..s.`, na.rm = TRUE),
    Low_freq = min(`Low.Freq..Hz.`, na.rm = TRUE),
    High_freq = max(`High.Freq..Hz.`, na.rm = TRUE)
    
  ) %>%
  ungroup()



write.table(song_level, "summary_df.txt", sep = "\t", row.names = FALSE, quote = FALSE)
