rm(list = ls())

library(dplyr)
library(tidyr)

#-------------------------------------------------------------------------------
setwd("C:/Users/chiti/OneDrive/Desktop/classified-notes-selection-tables")
df <- readr::read_tsv("KODAI_classified_4500_notes.txt")
df <- df %>% drop_na()

## addding individual ID 
# Assuming your dataframe is named df and the column with the strings is 'file_name'
colnames(df)
df0 <- df %>%
  separate(`Begin File`, into = c("part1", "part2", "part3", "part4"), sep = "-", remove = F) %>%
  mutate(Individual_ID = paste(part2, part3, sep = " "))

colnames(df0)
df1 <- df0[,c(16,17,12,19)]
colnames(df1)[1:3] <- c("Song_No","n_Gram","Population")
head(df1)

df1 <- df1 %>%
  mutate(Individual = paste0("KD", sprintf("%02d", as.numeric(factor(Individual_ID)))))

unique(df1$Individual)
#-------------------------------------------------------------------------------

# Step 1: Generate 1-gram output
one_gram <- df1

one_gram_occ<- one_gram %>% group_by(n_Gram,Population,Individual_ID,Individual) %>% summarise(Occurance = n())

one_gram_occ$Length = 1 #########

one_gram_occ_2 <- one_gram_occ %>%  filter(Occurance > 1)

#  one_gram_occ_2$Length = 1

# hist(one_gram_occ_2$occurance)

# one_gram_occ_2_2indv <- one_gram_occ_2 %>%
#  group_by(Note.types) %>%
# summarise(unique_ids = n_distinct(Individual_ID), 
#            total_occurrence = sum(occurance)) %>%
#  filter(unique_ids >= 2)

# Write 1-gram to CSV
# write.csv(one_gram_occ_2, "1-gram_output.csv", row.names = FALSE)

#-------------------------------------------------------------------------------

# Step 2: Generate 2-gram output
two_gram <- df1 %>%
  group_by(Song_No) %>%
  mutate(next_note = lead(n_Gram)) %>%
  filter(!is.na(next_note)) %>%
  unite(n_Gram, n_Gram, next_note, sep = " ")

two_gram_occ<- two_gram %>% group_by(n_Gram,Population,Individual_ID,Individual) %>% summarise(Occurance = n())

two_gram_occ$Length = 2 #########

two_gram_occ_2 <- two_gram_occ %>%  filter(Occurance > 1)

#two_gram_occ_2_2indv <- two_gram_occ_2 %>%
#  group_by(Note.types) %>%
#  summarise(unique_ids = n_distinct(Individual_ID), 
#           total_occurrence = sum(occurance)) %>%
#  filter(unique_ids >= 2)

# Write 2-gram to CSV
# write.csv(two_gram, "2-gram_output.csv", row.names = FALSE)

#-------------------------------------------------------------------------------

# Step 3: Generate 3-gram output
three_gram <- df1 %>%
  group_by(Song_No) %>%
  mutate(next_note = lead(n_Gram),
         next_next_note = lead(n_Gram, 2)) %>%
  filter(!is.na(next_next_note)) %>%
  unite(n_Gram, n_Gram, next_note, next_next_note, sep = " ")


three_gram_occ<- three_gram %>% group_by(n_Gram,Population,Individual_ID,Individual) %>% summarise(Occurance = n())

three_gram_occ$Length = 3


three_gram_occ_2 <- three_gram_occ %>%  filter(Occurance > 1)

#three_gram_occ_2_2indv <- three_gram_occ_2 %>%
#  group_by(Note.types) %>%
#  summarise(unique_ids = n_distinct(Individual_ID), 
#            total_occurrence = sum(occurance)) %>%
#  filter(unique_ids >= 2)

#hist(three_gram_occ_2_2indv$total_occurrence)
#barplot(three_gram_occ_2_2indv$Note.types~three_gram_occ_2_2indv$total_occurrence)
# Write 3-gram to CSV
#write.csv(three_gram, "3-gram_output.csv", row.names = FALSE)

#-------------------------------------------------------------------------------

# Step 4: Generate 4-gram output
four_gram <- df1 %>%
  group_by(Song_No) %>%
  mutate(next_note = lead(n_Gram),
         next_next_note = lead(n_Gram, 2),
         next_next_next_note = lead(n_Gram, 3)) %>%
  filter(!is.na(next_next_next_note)) %>%
  unite(n_Gram, n_Gram, next_note, next_next_note,
        next_next_next_note, sep = " ")

four_gram_occ<- four_gram %>% group_by(n_Gram,Population,Individual_ID,Individual) %>% summarise(Occurance = n())

four_gram_occ$Length = 4

four_gram_occ_2 <- four_gram_occ %>%  filter(Occurance > 1)


#-#-----------------#------------------------#----------------#

kodai = rbind(one_gram_occ,two_gram_occ,three_gram_occ, four_gram_occ)

write.csv(kodai, "KODAI-n-grams.csv", row.names = FALSE)

# Write 4-gram to CSV
write.csv(combine, "combine_5pops_ngrams_isbe.csv", row.names = FALSE)

#-------------------------------------------------------------------------------

# Step 5: Generate 5-gram output
five_gram <- df1 %>%
  group_by(Song.No.) %>%
  mutate(next_note = lead(Note.types),
         next_next_note = lead(Note.types, 2),
         next_next_next_note = lead(Note.types, 3),
         next_next_next_next_note = lead(Note.types, 4)) %>%
  filter(!is.na(next_next_next_next_note)) %>%
  unite(Note.types, Note.types, next_note, next_next_note, next_next_next_note,
        next_next_next_next_note, sep = "-")

# Write 5-gram to CSV
write.csv(five_gram, "5-gram_output.csv", row.names = FALSE)

#_______________________________________________________________________________


a<- two_gram %>% group_by(Note.types) %>% summarise(no.of.types = n())
