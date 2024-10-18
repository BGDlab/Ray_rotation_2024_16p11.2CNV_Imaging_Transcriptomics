# This code will take the AHBA matrix of gene expression mapped onto the brain parcellations from the DK atlas
# and the differential DK parcellation volumes of 16p11.2 del and dup patients and see if there is a correlation 
# will with any specific genes and the brain regions most changed by 16p11.2 CNV


# load in necessary packages
library(tidyverse)


#Set working directory
setwd("/Users/tammyray/Desktop/Aaron_16p_Imaging_Transcriptomics/CSV_data_sheets")

#Import AHBA data, but a version I edited where I changed cell A1 from blank to "Genes"
AllenHBA_DK_ExpressionMatrix <- read_tsv("AllenHBA_DK_ExpressionMatrix.tsv") #%>%

#Changing name of column 1, because it does not have a column title in the TSV file
names(AllenHBA_DK_ExpressionMatrix)[1] <- "Genes"
  
#Import Smrithi's MRI data analysis
Smrithi_MRI_analysis <- read_csv("16panalysis_BH_adjusted.csv")

#Then, I will filter the dataset to only include deletion data using model 3
Smrithi_MRI_analysis_model3_del <- Smrithi_MRI_analysis %>%
  filter(Genotype == "Genotype16pDeletion...5")
# I may also want to filter out non-ctx ROIs...


# Now, I need to tidy the AHBA data...  
AllenHBA_DK_ExpressionMatrix_edited <- AllenHBA_DK_ExpressionMatrix %>%
  #first I will remove the average column
  select(-"Average donor correlation to median")

  
AllenHBA_DK_ExpressionMatrix_edited <- pivot_longer(AllenHBA_DK_ExpressionMatrix_edited, # dataframe to be pivoted
             cols = 2:last_col(), # column names to be stored as a SINGLE variable
             names_to = "brainParcellations", # name of that new variable (column)
             values_to = "Gene_expression") # name of new variable (column) storing all the values (data)
  
# Next, I will change the brain parcellation names to match the Smrithi MRI data.   

