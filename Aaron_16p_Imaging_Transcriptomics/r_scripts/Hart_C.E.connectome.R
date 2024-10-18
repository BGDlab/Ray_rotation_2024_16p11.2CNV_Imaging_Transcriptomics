install.packages("R.matlab")


library(R.matlab)

# Set working directory
setwd("/Users/tammyray/Desktop/worm_wiring")
# Load the .mat file
mat_data <- readMat("GMaleChem.mat")

# View the structure of the imported data
str(mat_data)

str(mat_data$InterNeurons)


DVB_connectome <- mat_data$your_variable_name  # Replace with the actual variable name


# Switching gears to  meital's preliminary transcrtiptome data in DVB ----
#load packages i want to use
library(tidyverse)
# Set wd
setwd("/Users/tammyray/Desktop/2023-2024 year 1/Mike-Hart_2023-Fall/Data/Excel data sheets/From Meital")
# Load the .csv file
DVB <- read_csv("Mean_expression_DVB_for_R.csv")
#change name of columns
colnames(DVB)[1] <- "gene"
colnames(DVB)[2] <- "herm_expression"
colnames(DVB)[3] <- "male_expression"

DVB_filtered_high_exp <- DVB %>%
  filter(herm_expression > 1, male_expression > 1)

DVB_genes <- DVB_filtered_high_exp$gene


DVB_edits <- DVB %>%
  mutate(diff_exp_male_minus_herm = male_expression - herm_expression)


#I want to round all values in columns 2-4 to 5 decimal places
DVB_edits_2 <- DVB_edits %>%
  mutate(herm_expression = round(herm_expression, digits = 6), 
         male_expression = round(male_expression, digits = 6),
         diff_exp_male_minus_herm = round(diff_exp_male_minus_herm, digits = 6))

write_csv(DVB_edits_2, "DVB male vs herm expression r dataframe.csv")
         









