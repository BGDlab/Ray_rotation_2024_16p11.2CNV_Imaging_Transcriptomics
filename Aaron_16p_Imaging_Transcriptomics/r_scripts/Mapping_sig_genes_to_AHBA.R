#In this r script, I will map the significant genes found from my meta-analysis of published 16p deletion datasets to the AHBA.


# Loading in files and libraries ----

#Loading the necessary libraries
library(tidyverse)


#Setting working directory
setwd("/Users/tammyray/Desktop/Aaron_16p_Imaging_Transcriptomics/output")

#reading in my significant gene list
sig_genes_ALL <- read_csv("sig_genes_p0.001_DEG_data.csv")
up_reg_genes <- sig_genes_ALL %>% filter(combined_log2_fold_change > 0)
up_reg_gene_names <- up_reg_genes$gene_name

#reading in the AHBA data
setwd("/Users/tammyray/Desktop/Aaron_16p_Imaging_Transcriptomics/CSV_data_sheets")
AHBA <- read_tsv("/Users/tammyray/Desktop/Aaron_16p_Imaging_Transcriptomics/CSV_data_sheets/AllenHBA_DK_ExpressionMatrix.tsv")
# Now, I want to cchange the column header of the first column of the AHBA dataframe to gene_symbol
colnames(AHBA)[1] <- "gene_symbol"

#I want to add a column to my dataframe that is yes or no if the gene is in the 16p region, so that in my plot I can see if the gene is in the 16p region
#creating a list of genes in the 16p region
setwd("/Users/tammyray/Desktop/Aaron_16p_Imaging_Transcriptomics/CSV_data_sheets/transcriptomic_datasheets")
chr16p11.2_gene_list_Kusenda2015 <-  read.csv("Kusenda_2015_TableS1_16p11.2_gene_list.csv")
sixteen_p_11.2_genes <- chr16p11.2_gene_list_Kusenda2015$Gene.symbol


# tidying up the AHBA data ----
#Actually, for this purpose, i don't think i need to do that

#First, I will make a vector of significant gene names
gene_list_all <- sig_genes_ALL$gene_name

AHBA_sig_genes_all <- AHBA %>% filter(gene_symbol %in% gene_list_all)
#Only 87 of thr 132 genes mapped to the AHBA dataset. I wonder why
#Making a list of genes in sig gene list that did not map to AHBA
genes_not_mapped <- gene_list_all[!(gene_list_all %in% AHBA_sig_genes_all$gene_symbol)]
genes_not_mapped
#Ask aaron about this

# Need to pivot the datatable, so that genes are one variable, brain regions are 1 variable, and expression is one variable

AHBA_sig_genes_all_pivot <- AHBA_sig_genes_all %>%
  #remove the "Average donor correlation to the median" column
  select(-"Average donor correlation to median") %>%
  pivot_longer(cols = -gene_symbol, names_to = "region", values_to = "expression") %>%
  #adding a column for if gene is in the 16p region
  mutate(in_16p_region = ifelse(gene_symbol %in% sixteen_p_11.2_genes, "Yes", "No")) %>%
  # Adding a column for if gene is up or downregulated
  mutate(upreg = ifelse(gene_symbol %in% up_reg_gene_names, "Yes", "No"))

AHBA_sig_genes_all_pivot_ONLY <- AHBA_sig_genes_all %>%
  #remove the "Average donor correlation to the median" column
  select(-"Average donor correlation to median") %>%
  pivot_longer(cols = -gene_symbol, names_to = "region", values_to = "expression")

  
  
  
# GGPLOT plots ----
# Visualizing expression data of significant genes
ggplot(AHBA_sig_genes_all_pivot, aes(x=expression, y=region, color=gene_symbol)) + 
  geom_point() + 
  labs(title = "regional expression of significant genes from 16p del meta-analysis", x = "Expression (units?)", y = "Brain Region") +
  theme_minimal() +
  #I will remove the legend because it is too big
  theme(legend.position = "none")


# Visualizing expression data of significant genes
ggplot(AHBA_sig_genes_all_pivot, aes(x=expression, y=region, shape=in_16p_region, color=gene_symbol)) + 
  geom_point() + 
  labs(title = "regional expression of significant genes from 16p del meta-analysis", x = "Expression (units?)", y = "Brain Region") +
  theme_minimal() +
  #I will make the legend smaller and put it underneath the graph  because it is too big
  theme(legend.position = "bottom") +
  guides(color="none")

# same plot but aesthetics only for genes in 16p region
ggplot(AHBA_sig_genes_all_pivot, aes(x=expression, y=region, color=in_16p_region)) + 
  geom_point() + 
  labs(title = "regional expression of significant genes from 16p del meta-analysis", x = "Expression (units?)", y = "Brain Region") +
  theme_minimal() +
  #I will make the legend smaller and put it underneath the graph  because it is too big
  theme(legend.position = "bottom") 


ggplot(AHBA_sig_genes_all_pivot, aes(x=expression, y=region, color=upreg)) + 
  geom_point() + 
  labs(title = "regional expression of significant genes from 16p del meta-analysis", x = "Expression (units?)", y = "Brain Region") +
  theme_minimal() +
  #I will make the legend smaller and put it underneath the graph  because it is too big
  theme(legend.position = "bottom") 


#Load R.matlab
library(R.matlab)
#set wd
setwd("/Users/tammyray/Desktop/Aaron_16p_Imaging_Transcriptomics")
#read matlab file
AHBA_normed <- readMat("ROIxGene_aparcaseg_INT.mat")
head(AHBA_normed)

ABHA_normed_df <- as.data.frame(AHBA_normed$probeInformation)
head(ABHA_normed_df)

cats3 <- lapply(AHBA_normed$probeInformation, unlist, use.names=TRUE)

AHBA_samplegeneexpression <- lapply(AHBA_normed$SampleGeneExpression, unlist, use.names=TRUE)

AHBA_parcel_expression0 <- as.data.frame(AHBA_normed$parcelExpression)
AHBA_parcel_expression1 <- as.data.frame(AHBA_normed$parcelExpression)
AHBA_parcel_expression2 <- lapply(AHBA_normed$parcelExpression, unlist, use.names=TRUE)

str(AHBA_normed$parcelExpression)
head(AHBA_normed$parcelExpression[, 1:6])  # viewing the first 6 columns
AHBA_normed$SampleCoordinates
AHBA_normed$SampleGeneExpression

cats4 <- as.data.frame(AHBA_normed$probeInformation)

cats5 <- as.data.frame(AHBA_normed$parcelExpression)

array_gene_names <- unlist(AHBA_normed$probeInformation[[2]])

# Now, I want to rename the columns of the AHBA_parcel_expression dataframe to the gene names in array_gene_names
colnames(AHBA_parcel_expression0) <- array_gene_names
#Actually, first i need to edit my array_gene_names to add a 0 as the first item in my array
array_gene_names <- c(0, array_gene_names)

# I think AHBA_pacrel_expression is the correct dataset... i just now need to extract the gene names and the brain regions from the dataset

#Now i will import the region array that I got from the paper's github:
# Description: Region labels for aparcaseg parcellation
#https://github.com/BMHLab/AHBAprocessing/blob/master/code/dataProcessing/README_processingPipeline.txt
setwd("/Users/tammyray/Desktop/Aaron_16p_Imaging_Transcriptomics/CSV_data_sheets")
region_array <- read_csv("Region_labels_for_aparcaseg_parcellation.csv", col_names = FALSE)



#Now, I want to change the values of column 1 of the AHBA_parcel_expression0 dataframe to the values in the region_array dataframe
#Maybe I just mutate the dataframe to add the array as a column, move that column to the front, and then remove the original column
AHBA_parcel_expression0_labeled <- AHBA_parcel_expression0 %>%
  mutate(region = region_array) %>%
  #Now i want to move the region$x1 var to be the first column
  select(region, everything()) #%>%

AHBA_parcel_expression0_labeled <- AHBA_parcel_expression0_labeled %>%
  #Now i wanna remove the co,un titled 0
  select(-"0")

#Renaming column region$X1 to region in dataframe AHBA_parcel_expression0_labeled
colnames(AHBA_parcel_expression0_labeled)[1] <- "region"



#Yay now data table is as i want it to be!
# Now I will pivot the table so that it matches the AHBA_sig_genes_all_pivot table
AHBA_parcel_expression0_labeled_pivot <- AHBA_parcel_expression0_labeled %>%
  pivot_longer(cols = -region, names_to = "gene_symbol", values_to = "expression") #%>%


#adding a column for if gene is in the 16p region
AHBA_parcel_expression0_labeled_pivot <- AHBA_parcel_expression0_labeled_pivot %>%
  mutate(in_16p_region = ifelse(gene_symbol %in% sixteen_p_11.2_genes, "Yes", "No"))


AHBA_parcel_expression0_labeled_pivot_sigONLY <- AHBA_parcel_expression0_labeled_pivot %>%
  filter(gene_symbol %in% gene_list_all)

#Unnesting the region column 
AHBA_parcel_expression0_labeled_pivot_sigONLY_unnested <- AHBA_parcel_expression0_labeled_pivot_sigONLY %>%
  unnest(cols = region)
# Now, I want to change the column name X1 to region
colnames(AHBA_parcel_expression0_labeled_pivot_sigONLY_unnested)[1] <- "region"


# OK, Now that I have this data, I want to put it into a CSV so I have it for future reference
setwd("/Users/tammyray/Desktop/Aaron_16p_Imaging_Transcriptomics/output")
write.csv(AHBA_parcel_expression0_labeled_pivot, "Normed_AHBA_sig_genes.csv")
write.csv(AHBA_parcel_expression0_labeled_pivot_sigONLY_unnested, "Normed_AHBA_sig_genes_ONLY.csv")
write.csv(AHBA_sig_genes_all_pivot_ONLY, "Raw_AHBA_sig_genes_ONLY.csv")




str(AHBA_parcel_expression0_labeled_pivot_sigONLY)


#Now i will plot the normalized AHBA data ----

ggplot(AHBA_parcel_expression0_labeled_pivot_sigONLY_unnested, aes(x=expression, y=region, color=gene_symbol)) + 
  geom_point() + 
  labs(title = "NORMALIZED regional expression of significant genes from 16p del meta-analysis", x = "Normalized Expression", y = "Brain Region") +
  theme_minimal() +
  #I will remove the legend because it is too big
  theme(legend.position = "none")


# Visualizing expression data of significant genes
ggplot(AHBA_parcel_expression0_labeled_pivot_sigONLY_unnested, aes(x=expression, y=region, shape=in_16p_region, color=gene_symbol)) + 
  geom_point() + 
  labs(title = "NORMALIZED regional expression of significant genes from 16p del meta-analysis", x = "Normalized Expression", y = "Brain Region") +
  theme_minimal() +
  #I will make the legend smaller and put it underneath the graph  because it is too big
  theme(legend.position = "bottom") +
  guides(color="none")

# same plot but aesthetics only for genes in 16p region
ggplot(AHBA_parcel_expression0_labeled_pivot_sigONLY_unnested, aes(x=expression, y=region, color=in_16p_region)) + 
  geom_point() + 
  labs(title = "NORMALIZED regional expression of significant genes from 16p del meta-analysis", x = "Normalized Expression", y = "Brain Region") +
  theme_minimal() +
  #I will make the legend smaller and put it underneath the graph  because it is too big
  theme(legend.position = "bottom") 




#Now, form looking at the plot, I want to do a quick analysis to see if genes that are in 16p locus vs not in 16p locus are more or less expressed
# per brain region
# So, my question is per region, 














#I want to add a column to the dataframe AHBA_parcel_expression0 that contains the values of the array region_array
AHBA_parcel_expression0$region <- region_array
# Now i want to rename that column to "region"
colnames(AHBA_parcel_expression0)[ncol(AHBA_parcel_expression0)] <- "region"


#Now i want to rename the column in the AHBA_parcel_expression0_labeled dataframe from region$X1 to region
colnames(AHBA_parcel_expression0_labeled)[ncol(AHBA_parcel_expression0_labeled)] <- "region"


#Now i want to 









ABHA_normed_df_probeInfo <- as.data.frame(cats3)



# Trying to do analysis to see if there is even differential expression of genes in different brain regions ----

#I would like to do an analysis to see if any individual gene is significantly differentially expressed in any brain region
#I will do this using a linear model
#I will use the lm() function to fit a linear model to the data
#I will use the aov() function to perform an analysis of variance on the linear model
#I will use the summary() function to get the results of the analysis of variance
library(broom)  # for tidy ANOVA results

c %>%
  group_by(gene_symbol) %>%
  group_map(~ aov(expression ~ region, data = .x) %>%
            tidy()) %>%
  bind_rows(.id = "gene_symbol")

library(dplyr)
library(purrr)
library(broom)


# Perform ANOVA for each gene and extract full results including F-statistic and p-value
anova_results_try3 <- AHBA_sig_genes_all_pivot %>%
  group_by(gene_symbol) %>%
  group_map(~ {
    anova_model <- aov(expression ~ region, data = .x)
    summary_df <- as.data.frame(summary(anova_model)[[1]])  # Extract summary (including F-statistic)
    summary_df$gene_symbol <- unique(.x$gene_symbol)  # Add gene_symbol as a column
    summary_df
  }) %>%
  bind_rows()

# View the results
print(anova_results)

colnames(AHBA_sig_genes_all_pivot)





# Get the summary of the ANOVA for each gene
anova_summaries <- anova_results_try2 %>%
  mutate(anova_summary = map(anova_model, summary))


#my try... not good. i'd have to make individual dataframes per gene, or do something else here, to do this analyssi by gene. not sure what
lm.model <- lm(AHBA_sig_genes_all_pivot$expression ~ AHBA_sig_genes_all_pivot$region)
lm.model_summary <- summary(lm.model)

anova_model <- aov(expression ~ region, data = AHBA_sig_genes_all_pivot)
anova_summary <- summary(anova_model)




# Trying to do anova with rstatix package
# https://stackoverflow.com/questions/72927782/running-multiply-anova-and-or-t-test-on-variables-in-one-dataframe-in-r
library(rstatix)
anova_results_try4 <- AHBA_sig_genes_all_pivot %>%
  group_by(gene_symbol) %>%
  anova_test(expression ~ region, p.adjust.method = "bonferroni") %>%
  add_significance_column() %>%
  select(gene_symbol, term, p.adj, .significance)



anova_results_try5 <- AHBA_sig_genes_all_pivot_ONLY %>%
  group_by(gene_symbol) %>%
  anova_test(expression ~ region)

?anova_test()

install.packages("rstatix")
anova_results_try5 <- AHBA_sig_genes_all_pivot_ONLY %>%
  group_by(gene_symbol) %>%
  anova_test(expression ~ region)




anova_results_try5 <- AHBA_sig_genes_all_pivot_ONLY %>%
  group_by(gene_symbol) %>%
  group_map(~ {
    anova_model <- aov(expression ~ region, data = .x)
    summary_df <- as.data.frame(summary(anova_model)[[1]])  # Extract the ANOVA summary
    summary_df$gene_symbol <- unique(.x$gene_symbol)  # Add gene_symbol as a column
    summary_df
  }) %>%
  bind_rows()

anova_results_try5 <- AHBA_sig_genes_all_pivot_ONLY %>%
  group_by(gene_symbol) %>%
  do({
    anova_model <- aov(expression ~ region, data = .)
    summary(as.data.frame(summary(anova_model)[[1]]))
  })


anova_results_try5 <- AHBA_sig_genes_all_pivot_ONLY %>%
  group_by(gene_symbol) %>%
  do({
    anova_model <- aov(expression ~ region, data = .)
    summary_df <- as.data.frame(summary(anova_model)[[1]])  # Convert ANOVA table to data frame
    summary_df$gene_symbol <- unique(.$gene_symbol)  # Add gene_symbol as a column
    summary_df  # Return data frame
  })


anova_results_try6 <- AHBA_sig_genes_all_pivot_ONLY %>%
  group_by(gene_symbol) %>%
  do({
    anova_model <- aov(expression ~ region, data = .)
    summary_df <- as.data.frame(summary(anova_model)[[1]])  # Convert ANOVA table to data frame
    summary_df$F_value <- summary_df$`F value`  # Extract F-statistic column (if exists)
    summary_df$p_value <- summary_df$`Pr(>F)`   # Extract p-value column (if exists)
    summary_df$gene_symbol <- unique(.$gene_symbol)  # Add gene_symbol as a column
    summary_df
  })


AHBA_sig_genes_all_pivot_ONLY_ALDOA <- AHBA_sig_genes_all_pivot_ONLY %>%
  filter(gene_symbol == "ALDOA")

ALDOA_anova <- aov(expression ~ region, data = AHBA_sig_genes_all_pivot_ONLY_ALDOA)



#OK, so APpARENTLY i can't do an anova with my dataset, ebcause i only have one observation for expression for each brain region.
# I will instead use summary statirtics I guess
summary_stats <- summary(AHBA_sig_genes_all_pivot_ONLY_ALDOA$expression)

#I suppose I can do this per gene...
#I will make a boxplot of gene expression for all brain regions of each gene

ggplot(AHBA_sig_genes_all_pivot, aes(x=expression, y=gene_symbol, color=upreg)) + 
  geom_boxplot() + 
  labs(title = "expression of significant genes from 16p del meta-analysis", x = "Expression (units?)", y = "Gene") +
  theme_minimal() +
  #I will remove the legend because it is too big
  theme(legend.position = "none")

#I should arrange the genes by if in [16p locus or not]
























# Chatgpt analysis ----
# Group by gene and run ANOVA for each gene
anova_results <- AHBA_sig_genes_all_pivot %>%
  group_by(gene_symbol) %>%
  do(
    model = aov(expression ~ region, data = .),
    summary = summary(aov(expression ~ region, data = .))
  )
# Post-hoc tukeys test
AHBA_sig_genes_all_pivot %>%
  group_by(gene_symbol) %>%
  do(tukey = TukeyHSD(aov(expression ~ region, data = .)))

?aov


# Now i want to view my anova_results per gene
# View the summary for a particular gene, e.g., "GeneA"
gene_summary_ALDOA <- anova_results %>%
  filter(gene_symbol == "ALDOA") %>%
  pull(summary)

# Display the summary for "GeneA"
gene_summary_ALDOA

# Create an empty list to store the summaries
all_gene_summaries <- list()

# Get unique gene symbols
unique_genes <- unique(anova_results$gene_symbol)

# Loop over each gene
for (gene in unique_genes) {
  # Filter the dataframe for the specific gene and pull the summary
  gene_summary <- anova_results %>%
    filter(gene_symbol == gene) %>%
    pull(summary)
  
  # Store the summary in the list, with the gene name as the key
  all_gene_summaries[[gene]] <- gene_summary[[1]]
}

# You can access the results by gene name, e.g.:
all_gene_summaries[["GeneA"]]


# Convert the list to a dataframe
anova_summary_df <- data.frame(
  gene_symbol = names(all_gene_summaries),
  summary = I(all_gene_summaries), # Use I() to preserve the list structure in the dataframe
  stringsAsFactors = FALSE
)

# View the dataframe
head(anova_summary_df)

# Initialize an empty dataframe
anova_results_df <- data.frame(
  gene_symbol = character(),
  Df = numeric(),
  Sum_sq = numeric(),
  Mean_Sq = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each gene's summary, extract the values, and add them to the dataframe
for (i in 1:length(all_gene_summaries)) {
  gene <- names(all_gene_summaries)[i]
  summary_info <- all_gene_summaries[[i]]
  
  # Extract values from the summary object (example assumes F-value and p-value exist in the summary)
  Df <- summary_info[[1]]$Df # Extract the F-value
  p_value <- summary_info[[1]]$`Pr(>F)`  # Extract the p-value
  
  # Add the extracted values to the dataframe
  anova_results_df <- rbind(anova_results_df, 
                            data.frame(gene_symbol = gene, F_value = F_value, p_value = p_value))
}

# View the resulting dataframe
head(anova_results_df)


anova_results$model[1]
anova_results$summary[1]
