# First I will load in necessary packages
library(tidyverse)

#Setting my working directory to the location of the data
getwd()
setwd("/Users/tammyray/Desktop/Aaron_16p_Imaging_Transcriptomics/CSV_data_sheets")

#Loading in the data from 2014 Transcriptional Consequences of 16p11.2 Deletion and Duplication in Mouse Cortex and Multiplex Autism Families
#Table S3.Genome-wide_Differential_Expression Results


#I'm not sure how best to organize the data in the csv files. The original supplementary table came as an xlsx file with multiple sheets. I'm going to copy paste the data
#from the original xlsx file into separate csv files per sheet. I just wonder if I can have a csv with multiple sheets. Or If I should keep all info in one csv file, and
#just add a column for treatment
#NOTE: I tried to use read.delim(), but it didn't work (the columns collapsed, such that all info was in 1 column). The read.csv function worked.
Talkowski_2014_TableS3_GenomeWideDiffExpResults <- read.csv("Talkowski_2014_TableS3_Human_Del.csv")
Talkowski_2014_TableS3_GenomeWideDiffExpResults


#Now I want to practice sorting the data. So, I want to filter out data with a p value larger than 0.009, and then I want to sort by log fold change.
Talkowski_2014_p_0.001_sortByFoldChange <- Talkowski_2014_TableS3_GenomeWideDiffExpResults %>%
  filter(permPval_Del <= 0.001) %>%
  arrange(desc(FoldChange))

#Now I want to mutate the first column to separate out the gene name and chromosomal position. I can do this by using the separate() function from the tidyr package.
Talkowski_2014_TableS3_GenomeWideDiffExpResults_tidy <- Talkowski_2014_TableS3_GenomeWideDiffExpResults %>%
  separate(GeneInfo, c("Gene", "chromosomal_position"), sep = "/")

