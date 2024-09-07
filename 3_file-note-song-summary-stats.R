rm(list = ls())
library(dplyr)

note_summary_folder <-  "E:/Acoustics/ISBE_Oct24/note-level-isbe/" 

# List all CSV files in the folder
note_summary_files <- list.files(path = note_summary_folder, pattern = "*.txt", full.names = TRUE)


# Function to read and combine .txt files
read_and_combine_files <- function(note_summary_files) {
  list_of_dfs <- lapply(note_summary_files, function(note_summary_files) {
    read.table(note_summary_files, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
  })
  
  # Combine all dataframes into one
note_df <- do.call(rbind, list_of_dfs)
  
  return(note_df)
}

# Combine the files into one dataframe
note_df <- read_and_combine_files(note_summary_files)

# Check the result
head(note_df)


summary_note_df <- note_df %>%
  group_by(Begin.File) %>%
  summarise(note_count = length(Begin.File), .groups = 'drop')

############################### SONG SUMMARY 

song_summary_folder <-  "E:/Acoustics/ISBE_Oct24/song-level-isbe/" 

# List all CSV files in the folder
song_summary_files <- list.files(path = song_summary_folder, pattern = "*.txt", full.names = TRUE)


# Function to read and combine .txt files
read_and_combine_files <- function(song_summary_files) {
  list_of_dfs <- lapply(song_summary_files, function(song_summary_files) {
    read.table(song_summary_files, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
  })
  
  # Combine all dataframes into one
  combined_df <- do.call(rbind, list_of_dfs)
  
  return(combined_df)
}

# Combine the files into one dataframe
songs_df <- read_and_combine_files(song_summary_files)

# Check the result
head(songs_df)

summary_song_df <- songs_df %>%
group_by(Begin.File) %>%
summarise(song_count = length(Begin.File), .groups = 'drop')

###### merge both

df_merge <- merge(summary_song_df,summary_note_df,by="Begin.File") 


## print the location in another column


    # Split the string by dash and extract the second and third elements
df_merge$location <- sapply(strsplit(as.character(df_merge$Begin.File), "-"), `[`, 2)

df_merge1 <- df_merge %>%
  mutate(location_indiv = sapply(strsplit(as.character(Begin.File), "-"), function(x) paste(x[2], x[3], sep = "-")))

##summarise by individuals across pops

df_indiv1 <- df_merge1 %>% group_by(location_indiv, location) %>% summarise(
    total_notes=sum(note_count), total_songs = sum(song_count)
    )
##summarise by pops only

df_pop1 = df_indiv1 %>% group_by(location) %>%
  summarise(
    pop_notes = sum(total_notes),
    pop_songs = sum(total_songs)
  )
  
