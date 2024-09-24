rm(list = ls())
library(data.table)

setwd("E:/Acoustics/Arpitha_Manual_CVM_updated")
Dir <- "E:/Acoustics/Arpitha_Manual_CVM_updated"

file_list = list.files(path = Dir, pattern = "*.txt")

data_list <- lapply(file_list, function(file) {
  fread(file, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
})

## Combine note level selaction tables for all the files listed in a folder
library(stringr)
combined_data <- do.call(rbind, data_list)

## extract the location from the begin file name
combined_data$`Location` <- str_extract(combined_data$`Begin File`, "(?<=-)[A-Z]+(?=-)")




#. ######## FOR CALCULATING CVMS #################################
## convert the CVM column from character to numeric 
combined_data$CVM = as.numeric(combined_data$CVM)
##summarize proportion of cvms per song per population
library(dplyr)

a <- combined_data %>%
  group_by(`Begin File`, `Location`, `Song No.`) %>%
  summarise(`Note count` = n(), `Total CVMs` = sum(`CVM`) )

a$`prop_CVM` = a$`Total CVMs`/(a$`Note count`+(a$`Total CVMs`))

library(ggplot2)
library(ggridges)

location_order <- c("HIGHW", "GRASS", "ERAVI", "ANAIM", "BANDR", "BERJM", "KODAI")
a$Location <- factor(a$Location, levels = location_order)

ggplot(a, aes(x = Location, y = prop_CVM, color = Location)) +
  geom_boxplot(outlier.shape = NA) +  # Hides outliers to prevent overlap with jitter
  geom_jitter(width = 0.2, alpha = 0.6) +  # Adds jitter to make points visible
  labs(title = "Prop_CVM by Location", x = "Location", y = "Proportion of CVMs per song") +
  scale_color_manual(values = c("#1B7837", "#525252", "#4575B4", "#FEE090", "#FDAE61", "#F46D43", "#A50026")) +  # Custom colors for locations
  theme_minimal()


ggplot(a, aes(x = prop_CVM, y = Location, fill = Location)) +
  geom_density_ridges() + 
  theme_ridges() +
  labs(title = "Plot of prop of CVMs", x = "prop_CVM", y = "Location") +
  theme(legend.position = "none")

# Load required libraries
library(ggplot2)

# Ensure the Location variable is a factor if it isn't already
a$Location <- factor(a$Location)

# Create the violin plot with custom colors and median points
ggplot(a, aes(x = Location, y = prop_CVM, fill = Location)) +
  geom_violin(trim = FALSE) +  # Draws the full distribution of the data
  geom_point(stat = "summary", fun = median, color = "black", size = 3) +  # Add median points
  labs(title = "Violin Plot of prop_CVM by Location", x = "Location", y = "proportion of CVMs per song") +
  scale_fill_manual(values = c("#1B7837", "#525252", "#4575B4", "#FEE090", "#FDAE61", "#F46D43", "#A50026")) +  # Custom colors
  theme_minimal() +
  theme(legend.position = "none")  # Hide the legend


 ##total songs
b = a %>% group_by(`Location`) %>% summarise(Total_songs = n())

##total notes
c = a %>% group_by(Location) %>% summarise(Total_notes = sum(`Note count`))


##### FOR PCA

pca_data <- combined_data[,c(15,6,7,8,9)]

numeric_data = pca_data %>% select_if(is.numeric)

scaled_data <- scale(numeric_data)

# Perform PCA
pca_result <- prcomp(scaled_data, center = TRUE, scale. = TRUE)

# Print PCA summary
print(summary(pca_result))

# Create a data frame with PCA results for plotting
pca_df <- as.data.frame(pca_result$x)
pca_df$Location <- combined_data$Location  # Add Location or any other categorical variable for color coding

# Plot the first two principal components
ggplot(pca_df, aes(x = PC1, y = PC2, color = Location)) +
  geom_point() +
  labs(title = "PCA Plot of First Two Principal Components", x = "PC1", y = "PC2") +
  scale_color_manual(values = c("#1B7837", "#525252", "#4575B4", "#FEE090", "#FDAE61", "#F46D43", "#A50026")) +
  theme_minimal()


##Linear Discriminat Analysis 

library(MASS)
library(ggplot2)

# Perform LDA
lda_model <- lda(Location ~ ., data = pca_data)

# Print LDA model summary
print(lda_model)

# Get the LDA results
lda_result <- predict(lda_model)

# Create a data frame with LDA results for plotting
lda_df <- as.data.frame(lda_result$x)
lda_df$Location <- pca_data$Location  # Add the class labels

# Plot the first two linear discriminants
ggplot(lda_df, aes(x = LD1, y = LD2, color = Location)) +
  geom_point() +
  labs(title = "LDA Plot of the First Two Linear Discriminants", x = "LD1", y = "LD2") +
  scale_color_manual(values = c("#1B7837", "#525252", "#4575B4", "#FEE090", "#FDAE61", "#F46D43", "#A50026")) +
  theme_minimal()
