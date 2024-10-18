# With this code, I will identify genes of interest for further analysis with the imaging transcriptomics project.
#I have CSV files of differentially expressed genes from cortical organoids at 3 months development. I will read in the data and create a dataframe from it


# Loading packages, setting wd, reading in data ----
#Load the necessary libraries 
library(tidyverse)


#Set working directory
setwd("/Users/tammyray/Desktop/Aaron_16p_Imaging_Transcriptomics/CSV_data_sheets/transcriptomic_datasheets")

#Read in the data
DEG_iPSC_organoid_Urresti2021 <- read.csv("Urresti-2021_DEG-iPSC-cortical-organoid.csv")

DEG_1M_organoid_Urresti2021 <- read.csv("Urresti-2021_DEG-1M-cortical-organoid.csv")

DEG_3M_organoid_Urresti2021 <- read.csv("Urresti-2021_DEG-3M-cortical-organoid.csv")

DEG_hIPSC_neural_endodermMesoderm_Roth2020 <-  read.csv("Roth_2020_DEG-hIPSC-neural-endodermMesoderm.csv")

DEG_hLCL_Talkowski2014  <-  read.csv("Talkowski_2014_TableS3_Human_Del.csv")

chr16p11.2_gene_list_Kusenda2015 <-  read.csv("Kusenda_2015_TableS1_16p11.2_gene_list.csv")


# Editing dataframes ----

# Now, I want to add together the different dataframes from different conditions in these datasets. So, for the organoid Urresti data, I will
# need to  add in the iPSC and 1 month organoid data. I will convert them to csvs, then read in the data. Then, I will read in these files,
# add columns for the age of organoid, then combine them

# Add column for timepoint
DEG_iPSC_organoid_Urresti2021 <- DEG_iPSC_organoid_Urresti2021 %>%
  mutate(sample_type = "cortical_organoid_iPSC")

DEG_1M_organoid_Urresti2021 <- DEG_1M_organoid_Urresti2021 %>%
  mutate(sample_type = "cortical_organoid_1_month")

DEG_3M_organoid_Urresti2021 <- DEG_3M_organoid_Urresti2021 %>%
  mutate(sample_type = "cortical_organoid_3_month")

# Combine all of the cortical organoid info into 1 dataframe
DEG_all_timepoints_organoid_Urresti2021 <- rbind(DEG_iPSC_organoid_Urresti2021, DEG_1M_organoid_Urresti2021, DEG_3M_organoid_Urresti2021)


# I want to add these and a few other dataframes together. To do this, I will need to add a column to each dataframe that will allow me to identify the source
# of the data. I will need to use dplyr to change the dataframes so that the columns all match.
# Here I will list the column names of each dataframe.

DEG_all_timepoints_organoid_Urresti2021_vars <- names(DEG_3M_organoid_Urresti2021)
view(DEG_all_timepoints_organoid_Urresti2021_vars)
DEG_hIPSC_neural_endodermMesoderm_Roth2020_vars <- names(DEG_hIPSC_neural_endodermMesoderm_Roth2020)
view(DEG_hIPSC_neural_endodermMesoderm_Roth2020_vars)
DEG_hLCL_Talkowski2014_vars <- names(DEG_hLCL_Talkowski2014)
view(DEG_hLCL_Talkowski2014_vars)

#I need to identify similar variables in the columns, that may have different names but contain the same variable
#I think I will include columns that have variables that not all dataframes have in common, and I will figure out how to keep them blank
#I also, for now, want to exclude the dup data from the datasets... Hmm maybe IO can keep them in there, but just exclude them from analysis for now.
# To simplify things, because not all datasets have dup data.


#In the following line of code I will change column names so that they match across all dataframes
DEG_all_timepoints_organoid_Urresti2021_modified_columns <- DEG_all_timepoints_organoid_Urresti2021 %>%
  #remove columns that i do not need
  select(-c("Gene.Biotype", "log2.FC..DUP.vs.DEL", "P.value..DUP.vs.DEL", "FDR.adjusted.P.value..DUP.vs.DEL", "log2.FC..DUP.vs.CTL",
            "P.value..DUP.vs.CTL", "FDR.adjusted.P.value..DUP.vs.CTL", "FDR.adjusted.P.value..DEL.vs.CTL")) %>%
  
  #rename columns to the standardized names I decided on
  rename("gene_name" = "HGNC.Symbol",
         "log2_fold_change" = "log2.FC..DEL.vs.CTL",
         "p_value" = "P.value..DEL.vs.CTL",
         "gene_description" = "Description", 
         "ENSEMBL_ID" = "ENSEMBL.ID") %>%
  
  #add a column for the data source
  mutate("data_source" = "Urresti et al. 2021", "Present.in.the.16p11.2.Region" = "") %>%
  
  #reorder columns so that the ensemble_ID column, which not all dataframes will have, is at the end
  relocate("ENSEMBL_ID", .after = "data_source")
  
#Now I will do it for just the 3 month old organoids
DEG_3M_organoid_Urresti2021_modified_columns <- DEG_3M_organoid_Urresti2021 %>%
  #remove columns that i do not need
  select(-c("Gene.Biotype", "log2.FC..DUP.vs.DEL", "P.value..DUP.vs.DEL", "FDR.adjusted.P.value..DUP.vs.DEL", "log2.FC..DUP.vs.CTL",
            "P.value..DUP.vs.CTL", "FDR.adjusted.P.value..DUP.vs.CTL", "FDR.adjusted.P.value..DEL.vs.CTL")) %>%
  
  #rename columns to the standardized names I decided on
  rename("gene_name" = "HGNC.Symbol",
         "log2_fold_change" = "log2.FC..DEL.vs.CTL",
         "p_value" = "P.value..DEL.vs.CTL",
         "gene_description" = "Description", 
         "ENSEMBL_ID" = "ENSEMBL.ID") %>%
  
  #add a column for the data source
  mutate("data_source" = "Urresti et al. 2021", "Present.in.the.16p11.2.Region" = "") %>%
  
  #reorder columns so that the ensemble_ID column, which not all dataframes will have, is at the end
  relocate("ENSEMBL_ID", .after = "data_source")


DEG_hIPSC_neural_endodermMesoderm_Roth2020_modified_columns <- DEG_hIPSC_neural_endodermMesoderm_Roth2020 %>%
  # remove columns that I do not need
  select(-c("SFARI.Gene.Score", "SFARI.Gene.Score.Category", "https...gene.sfari.org.about.gene.scoring.criteria.")) %>%

  rename("gene_name" = "X16p11.2.DE.Genes",
         "log2_fold_change" = "Log2.Fold.Change..Positive.values.are.upregulated.in.DEL.",
         "p_value" = "Adjusted.P.Value") %>%
  
  #add columns for the data source, sample type, and extras to match the above dataset
  mutate("gene_description" = "", "sample_type" = "hIPSC", "data_source" = "Roth et al. 2020", "ENSEMBL_ID" = "") %>%
  
  #reorder columns to match the other dataframes
  relocate("gene_description", .after = "gene_name") %>%
  relocate("Present.in.the.16p11.2.Region", .after = "ENSEMBL_ID")

#editing the Talkowski dataframe

DEG_hLCL_Talkowski2014_modified_columns <-  DEG_hLCL_Talkowski2014 %>% 
  select(-c("GeneInfo", "Chr", "Start", "Stop")) %>%
  #select(-c("GeneInfo", "Chr", "Start", "Stop")) %>%
  
  mutate("FoldChange" = log2(FoldChange)) %>%
  
  rename("gene_name" = "GeneID",
         "log2_fold_change" = "FoldChange",
         "p_value" = "permPval_Del") %>%
  
#add columns for the data source, sample type, and extras to match the above dataset
  mutate("gene_description" = "", "sample_type" = "hLCL", "data_source" = "Talkowski et al. 2014", "ENSEMBL_ID" = "", "Present.in.the.16p11.2.Region" = "") %>%

  #reorder columns to match the other dataframes
  relocate("gene_description", .after = "gene_name") %>%
  relocate("p_value", .after = "log2_fold_change")

# Adding ensembl IDs for all genes ----
#Now, I want to add the ensembl IDs to the Talksowski2014 data, using biomaRt. I will first include the code for installing and loading biomaRt
 if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
 
 BiocManager::install("biomaRt")
library(biomaRt) 

#Now, I will define my mart as homo sapien
ensembl_mart <- useMart("ensembl", dataset = "hsapiens_gene_ensembl")

ensembl_code_from_website <- useEnsembl(biomart = "genes", dataset = "hsapiens_gene_ensembl")
listEnsembl()
ensembl_code_from_website <- useEnsembl(biomart = "genes")
datasets <- listDatasets(ensembl)
head(datasets)
searchDatasets(mart = ensembl_code_from_website, pattern = "hsapiens")
#selecting a database
ensembl_code_from_website <- useDataset(dataset = "hsapiens_gene_ensembl", mart = ensembl_code_from_website)
#listing filters from our ensembl mart
filters = listFilters(ensembl_code_from_website)
filters[1:5,]
#listing attributes from our ensembl mart
attributes = listAttributes(ensembl_code_from_website)
attributes[1:5,]

ensembl_IDs_Talkowski2014_TRY2 <- getBM(
  attributes = c('ensembl_gene_id', 'external_gene_name', "description", 'chromosome_name', 'start_position', 'end_position'),
  filters = 'chromosomal_region',
  values = chromosomal_region_Talkowski2014,
  mart = ensembl_code_from_website
)
#This still isn't working. I am inputting 908 observations for my values argument, but my resulting dataframe, ensembl_IDs_Talkowski2014_TRY2, has 5728 observations.

#filtering out blank "external_gene_name" values
ensembl_IDs_Talkowski2014_TRY2_no_blank_geneNames <- ensembl_IDs_Talkowski2014_TRY2 %>%
  filter(external_gene_name != "")
#now, i will return the number of unique gene names
unique(ensembl_IDs_Talkowski2014_TRY2_no_blank_geneNames)

# making a vector of gene names from talkowski 2014 data
Talkowsk2014_gene_names <- DEG_hLCL_Talkowski2014_modified_columns$gene_name

#filtering out opbservations from the ensebmle data that do NOT have gene names from the original Talkowski 2014 data
ensembl_IDs_Talkowski2014_TRY2_no_blank_geneNames_FILTERED <- ensembl_IDs_Talkowski2014_TRY2_no_blank_geneNames %>%
  filter(external_gene_name %in% Talkowsk2014_gene_names)
#This results in a dataframe with 205 observations. Weird. I will do this again, but with chromosome number and start and end positions. I hope I can 

#Ok, now I want to try to use my dataframe with the chromosome number, start and stop positions to filter the entire mart ensembl_code_from_website.
# But I guess first I need to make that into a datatable
# I will use my perviously created mart, ensembl_code_from_website
#Then, i will retrieve my desired attributes.
# Retrieve the desired attributes
gene_data_from_biomaRt <- getBM(
  attributes = c('external_gene_name', 'ensembl_gene_id', 'description', 
                 'chromosome_name', 'start_position', 'end_position'),
  mart = ensembl_code_from_website
)
#I want to decrease the size of this dataframe. Firt I will see if i can do this by chromosome name
#list of unique chromosome name values in the mart dataframe
unique_chromosome_names <- as.list(unique(gene_data_from_biomaRt$chromosome_name))
#list of unique chromosome name values in the Talkowski2014 dataframe
unique_chromosome_names_Talkowski2014 <- as.list(unique(DEG_hLCL_Talkowski2014$Chr))
#I see that the talkowski data chromosome info contains chr before each value. I want to remove that
#I will remove the "chr" from the chromosome names in the Talkowski2014 dataframe
DEG_hLCL_Talkowski2014$Chr <- gsub("chr", "", DEG_hLCL_Talkowski2014$Chr)
# Now, I will redo the unique chromosome names for the Talkowski2014 dataframe
unique_chromosome_names_Talkowski2014 <- as.list(unique(DEG_hLCL_Talkowski2014$Chr))

#Now, I will filter the gene_data_from_biomaRt dataframe to only include chromosomes that are in the Talkowski2014 dataframe
gene_data_from_biomaRt_filteredByChr <- gene_data_from_biomaRt %>%
  filter(chromosome_name %in% unique_chromosome_names_Talkowski2014)
#That resulted in a small decrease in the size of the dataframe. I will now filter by chromosome, start and end positions.
# I think the column names must be the same for me to do this. I will edit the column names of my Talkowski2014 dataframe to match the gene_data_from_biomaRt dataframe
DEG_hLCL_Talkowski2014_columnNameChange <- DEG_hLCL_Talkowski2014 %>%
  rename("chromosome_name" = "Chr",
         "start_position" = "Start",
         "end_position" = "Stop")


# Assuming dt1 and dt2 are your data.tables
MERGED_biomaRt_Talkowski2014 <- merge(gene_data_from_biomaRt_filteredByChr, DEG_hLCL_Talkowski2014_columnNameChange, by = c("chromosome_name", "start_position", "end_position"))
?merge()
#This did not work... is it because the  chromosome name, start position, and end position do not match up? I will check tmem gene from each database and see


# Hmm, I wonder if it could be because some of these variables do not have the same datatype... let me check
#Checking the class of the chromosome_name variable in the gene_data_from_biomaRt_filteredByChr dataframe
class(gene_data_from_biomaRt_filteredByChr$chromosome_name) #result: character
class(DEG_hLCL_Talkowski2014_columnNameChange$chromosome_name) #result: character
class(gene_data_from_biomaRt_filteredByChr$start_position) #result: integer 
class(DEG_hLCL_Talkowski2014_columnNameChange$start_position) #result: integer
class(gene_data_from_biomaRt_filteredByChr$end_position) #result: integer
class(DEG_hLCL_Talkowski2014_columnNameChange$end_position) #result: integer
#All datatypes match. So, must be something else
#It would be really unfortunate if I was working in the wrong mart this entire time. the wrong organism i mean...






# Now, I will extract the chromosome, start, and end position data from my dataframe
chrom_Talkowski2014 <- DEG_hLCL_Talkowski2014$Chr
start_Talkowski2014 <- DEG_hLCL_Talkowski2014$Start
end_Talkowski2014 <- DEG_hLCL_Talkowski2014$Stop
#Seeing what class the chrom_Talkowski2014 variable is
class(chrom_Talkowski2014)
mode(chrom_Talkowski2014)
typeof(chrom_Talkowski2014)
# I will probably need to remove the "chr" from the chrom_Talkowski2014 vector.
# In the following line of code, I want to remove the characters "chr" from each item in chrom_Talkowski2014
chrom_Talkowski2014 <- as.integer(gsub("chr", "", chrom_Talkowski2014))
#Some of the chromosomes are not numbers (for exmaple, the X chromosome) so when I convert this list to integers, the value is NA.
#So instead, I'll try to do this without converting to integers
chrom_Talkowski2014 <- gsub("chr", "", chrom_Talkowski2014)

# OK, i'm not sure exactly how that worked, but now chrom_Talkowski2014 is a vector of strings         
# Now, I just need to use getBM to get the ensembl ID for each gene, based off of the chrom, start, and end values

#creating a dataframe with the talkowski chrom number, start and stop position so that my getBM function will work properly
gene_position_Talkowski2014 <- data.frame(chromosome = chrom_Talkowski2014,
                                          start = start_Talkowski2014,
                                          end = end_Talkowski2014)
# Creating a list of chromosomal regions to search for ensembl IDs. chromosomal_region includes the chromosome number, start and step positions
# so that I will only have to have 1 query/value to get the ensembl ID
gene_position_Talkowski2014_chromreg <- gene_position_Talkowski2014 %>%
  mutate(chromosomal_region = paste(chromosome, start, end, sep = ":"))
chromosomal_region_Talkowski2014 <- gene_position_Talkowski2014_chromreg$chromosomal_region


chromosomal_region_Talkowski2014

ensembl_IDs_Talkowski2014 <- getBM(
  attributes = c('ensembl_gene_id', 'external_gene_name', "description", 'chromosome_name', 'start_position', 'end_position'),
  filters = 'chromosomal_region',
  values = chromosomal_region_Talkowski2014,
  mart = ensembl_mart
)
ensembl_mart_attributes <- listAttributes(ensembl_mart)
ensembl_mart_filters <- listFilters(ensembl_mart)
# Are my chromosomal_region values in the correct format? I will check the first 10 values
chromosomal_region_Talkowski2014[1:10]



ensembl_IDs_Talkowski2014_1_attribute <- getBM(
  attributes = 'ensembl_gene_id',
  filters = c('chromosome_name', 'start', 'end'),
  values = list(chrom_Talkowski2014, start_Talkowski2014, end_Talkowski2014),
  mart = ensembl_mart
)

# Adding the ensembl IDs created in the previous step to the DEG_hLCL_Talkowski2014_modified_columns dataframe
DEG_hLCL_Talkowski2014_modified_columns <- cbind(DEG_hLCL_Talkowski2014_modified_columns, ensembl_IDs_Talkowski2014)



sixteen_p_11.2_genes <- chr16p11.2_gene_list_Kusenda2015$Gene.symbol

#Now, I want to fill in the "Present.in.the.16p11.2.Region" column for the DEG_all_timepoints_organoid_Urresti2021_modified_columns dataframe. 
# The vector Sixteen_p_11.2_genes is a vector containing all genes located in the 16p11.2 chromosomal region
# I want to change the value of the "Present.in.the.16p11.2.Region" column to "Yes" if the gene name is in the vector, and "No" if it is not
#Here is the code: 

DEG_all_timepoints_organoid_Urresti2021_modified_columns$Present.in.the.16p11.2.Region <-
  ifelse(DEG_all_timepoints_organoid_Urresti2021_modified_columns$gene_name %in% sixteen_p_11.2_genes, "Yes", "No")

DEG_3M_organoid_Urresti2021_modified_columns$Present.in.the.16p11.2.Region <-
  ifelse(DEG_3M_organoid_Urresti2021_modified_columns$gene_name %in% sixteen_p_11.2_genes, "Yes", "No")

DEG_hLCL_Talkowski2014_modified_columns$Present.in.the.16p11.2.Region <-
  ifelse(DEG_hLCL_Talkowski2014_modified_columns$gene_name %in% sixteen_p_11.2_genes, "Yes", "No")

#Now, I want to combine these two, modified dataframes to make sure it works  

all_DEG_data <- rbind(DEG_3M_organoid_Urresti2021_modified_columns, DEG_hIPSC_neural_endodermMesoderm_Roth2020_modified_columns, DEG_hLCL_Talkowski2014_modified_columns)

# Now, I want to look at the genes in the 16p11.2 region only, from the all DEG data. So I will filter the all_DEG_data dataframe to only include those genes
all_DEG_data_16p_genes <- all_DEG_data %>%
  filter(Present.in.the.16p11.2.Region == "Yes")
#Looking through the data, it seems that all logfoldchanges are negative, which makes sense because these genes are deleted. So that makes sense.
#I'm wondering what all we're going to do with the fact that we have pretty much 5 readings per gene, from the various datasets/samples. 


# Jakob idea to use only 3 month cortical organoid data plus the other two datasets and do some fischers test to combine the p values for each gene...
# How do i handle instances where the gene is not in all datasets??

# OK, so I asked chatgpt how to do this. First, I need to calculate the average for log fold change
# they recommend a weighted average, computing that with the standard error, but don't have that data. 
# Chatgpt said the weights should be inversely proportional to the standard errors.
# Apparently, chatgpt says the ideal scenario here is to use the standard error to weight the different log fold changes,
# but I can use the p value as a substitute based on the idea that lower p-values indicate stronger evidence against the null
# hypothesis, and thus, a more reliable estimate
# Anyway, after that I will use fisher's or stouffer's methods to combine the p-values for each gene

library(metap)


meta_analysis_all_DEG_data <- all_DEG_data %>%
  group_by(gene_name) %>%
  summarize(combined_log2_fold_change = sum(log2_fold_change / (p_value + 1e-8)) / sum(1 / (p_value + 1e-8)),  # Weighted logFC using p-values.
            # A small constant (1e-8) is added to avoid division by zero if there are very small p-values.
            #in the next line of code I will do a fisher test to combine the p-values
            combined_p_value = 1 - pchisq(sum(qchisq(1 - p_value, 1, lower.tail = FALSE)), length(p_value), lower.tail = FALSE)) %>%
  #the above code will return a dataframe that only contains 3 of the columns. In the next line of code I will bring the rest of the columns back to this new dataframe
  left_join(all_DEG_data, by = "gene_name")


#Maybe it would be a good idea to keep track of if the gene combined logfoldcahnge and p-value are from multiple data sources
meta_analysis_all_DEG_data$multiple_data_sources <- ifelse(duplicated(meta_analysis_all_DEG_data$gene_name) | duplicated(meta_analysis_all_DEG_data$gene_name, fromLast = TRUE), "Yes", "No")

#Now, I want to delete rows that have the same gene name and remove the columns that have the original log2_fold_change, p_value, sample_type, and data_source
meta_analysis_all_DEG_data <- meta_analysis_all_DEG_data %>%
  distinct(gene_name, .keep_all = TRUE) %>%
  select(-c(log2_fold_change, p_value, sample_type, data_source))



#Now I want to get a summary of the combined log2_fold_change and combined p_value, including the mean, median, and range of the data
summary_meta_analysis_all_DEG_data <- meta_analysis_all_DEG_data %>%
  summarize(mean_combined_log2_fold_change = mean(combined_log2_fold_change, na.rm = TRUE),
            median_combined_log2_fold_change = median(combined_log2_fold_change, na.rm = TRUE),
            range_combined_log2_fold_change = range(combined_log2_fold_change, na.rm = TRUE),
            mean_combined_p_value = mean(combined_p_value, na.rm = TRUE),
            median_combined_p_value = median(combined_p_value, na.rm = TRUE),
            range_combined_p_value = range(combined_p_value, na.rm = TRUE))

# Now, I want to add new columns to my meta_analysis dataset to indicate how significant each point is
#Smrithi did it all in one column, but Jakob suggested having separate columns for each significance level
# I will do both, because right now I can't imagine any pros/cons that would make one better than the other

#This code adds a column called significance that will indicate the degree of significance, just like Smrithi's code
meta_analysis_all_DEG_data <- meta_analysis_all_DEG_data %>%
  mutate("significance" = case_when(
    combined_p_value < 0.001 ~ "***",
    combined_p_value < 0.01 ~ "**",
    combined_p_value < 0.05 ~ "*",
    TRUE ~ " "
  ))





#Now i will create a new dataframe with only the genes that are significantly differentially expressed at combined p-value < 0.001
sig_genes_p0.001_DEG_data <- meta_analysis_all_DEG_data %>%
  filter(significance == "***")

#Now I will create my first gene list, which will be all the genes that are significant at the p < 0.001 level.
# I will create an array out of all gene names
#For clarity, gene_list_1_p_0.001 is an experimental gene list, containing 132 genes from the meta-analysis of the 3 month old cortical
# organoid data, the hIPSC data, and the hLCL data, after they have been combined using fisher for p values and weighted logfoldchange with p values.
# These genes combined p-value is less than 0.001
exp_gene_list_1_p_0.001 <- sig_genes_p0.001_DEG_data$gene_name
# I think we can start with these genes and see what we get



#exp_gene_list_2_16p_genes is an experimental gene list containing the 34 genes that are expressed in the 16p11.2 locus
#I SHOULD GO BACK IN THE APPER AND CHECKL, I THINK SOME OF THE GENES IN THIS LIST MAY BE OUTSIDE OF THE LOCUS, BUT VERY NEARBY
exp_gene_list_2_16p_genes <- sixteen_p_11.2_genes


#Then, I want to make some null gene lists. The weakest null gene list would be to randomly select genes from the entire AHBA
# Jakob recommended that I talk to Aaron about it
# I want to do my own research and propose an idea to Aaron, rather than just asking him what he thinks is best
#I NEED TO READ THE AARON 2018 PAPER

# GO Analysis using gprofiler2 ----
#Now, I want to do a functional enrichment analysis on the gene list I generated
# I will use the gprofiler2 R package to do this
library(gprofiler2)
?gost
GO_results_exp_gene_list_p0.001 = gost(query = exp_gene_list_1_p_0.001,
               organism = "hsapiens",
               correction_method = "fdr")
GO_all_results_p0.001 <- GO_results_exp_gene_list_p0.001$result %>%
  select(-parents)

#putting metadata from GO analysis into a dataframe
GO_meta_p0.001 <- GO_results_exp_gene_list_p0.001$meta


#Creating gene lists for upregulated and downregulated genes
UPregulated_sig_genes_p0.001_DEG_data <- sig_genes_p0.001_DEG_data %>%
  filter(combined_log2_fold_change > 0)
UPregulated_exp_gene_list_1_p_0.001 <- UPregulated_sig_genes_p0.001_DEG_data$gene_name

DOWNregulated_sig_genes_p0.001_DEG_data <- sig_genes_p0.001_DEG_data %>%
  filter(combined_log2_fold_change < 0)
DOWNregulated_sig_genes_p0.001_DEG_data <- DOWNregulated_sig_genes_p0.001_DEG_data$gene_name


# GO analysis of upregulated and downregulated genes
gostres_UPregulated <- gost(query = UPregulated_exp_gene_list_1_p_0.001,
               organism = "hsapiens",
               correction_method = "fdr")

gostres_UPregulated <- gostres_UPregulated$result


gostres_DOWNregulated <- gost(query = DOWNregulated_sig_genes_p0.001_DEG_data,
                            organism = "hsapiens",
                            correction_method = "fdr")

gostres_DOWNregulated <- gostres_DOWNregulated$result





is.data.frame(gostres_results)
sapply(gostres_results, class)
nrow(gostres_results)

#Exporting gene ontology results to a csv file
write.csv(gostres_results, file="/Users/tammyray/Desktop/Aaron_16p_Imaging_Transcriptomics/output/GO_16p11.2-DEGs_p0.001_fdr-corrections.csv", row.names = FALSE)
#exporting all DEG data to a csv file
write.csv(all_DEG_data, file="/Users/tammyray/Desktop/Aaron_16p_Imaging_Transcriptomics/output/all_DEG_data.csv", row.names = FALSE)
#exporting meta analysis data to a csv file
write.csv(meta_analysis_all_DEG_data, file="/Users/tammyray/Desktop/Aaron_16p_Imaging_Transcriptomics/output/meta_analysis_all_DEG_data.csv", row.names = FALSE)
#exporting significant p=0.001 gene list dataframe to a csv file
write.csv(sig_genes_p0.001_DEG_data, file="/Users/tammyray/Desktop/Aaron_16p_Imaging_Transcriptomics/output/sig_genes_p0.001_DEG_data.csv", row.names = FALSE)




#Ok, it would be interesting to see if thre's a difference in enrishment of genes that are upregulated vs downregulated... let me test that now





# UNfinished attempt to add chromosomal position values to the meta_analysis_all_DEG_data dataframe ----
#Now i would like to use the biomart library to add chromosomal position values to my meta_analysis_all_DEG_data dataframe
#I had to change my version of R from 4.4.1 to 4.4.0 in order to use biomart. The R switch thing isn't looking like it's working like I want it to, so I won't be
# able to easily switch ebtween 4.4.0 and 4.4.1
# https://rud.is/rswitch/guide/
#Margaret said a singularity container might work for switching r versions if this r switcher thing didn't work, I can ask her for future directions
R.version
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("biomaRt")
library(biomaRt) # an alternative for annotation

# # Connect to the Ensembl database (human genes)
ensembl <- useMart("ensembl", dataset = "hsapiens_gene_ensembl")
# the following code is from Covid_Project_TR.R, which i made in my diytranscriptomcis class. the code is in dir-2

human_anno <- listAttributes(ensembl)

human_gene_info <- getBM(attributes=c('ensembl_gene_id',
                                        'external_gene_name',
                                       'chromosome_name',
                                       'start_position',
                                       'end_position'),
                           mart = ensembl)

# Check for duplicate gene names in human_gene_info
human_gene_info_duplicates -> human_gene_info %>%
  group_by(external_gene_name) %>%
  filter(n() > 1) %>%
  summarise(count = n())
#I'm not sure why this code didn't work, but I did order the human_gene_info dataframe by external_gene_name and then looked through it manually and
# I did see duplicate entries for the same gene name, but with different ensemble gene ids. Weird. I want to change my methods now. I want to join 
# the dataframes by ensembl id. 

# first I will create a vector of all gene names in my meta analysis all DEG dataframe
meta_analysis_all_DEG_data_GENENAMES <- as.vector(meta_analysis_all_DEG_data$gene_name)

#Now I will filter the human_gene_info dataframe to only include the genes that are in my meta analysis all DEG dataframe
human_gene_info_DEGs <- as_tibble(human_gene_names) %>%
  filter(external_gene_name %in% meta_analysis_all_DEG_data_GENENAMES)
#I see that there are several genes that have duplicates, and the chromosomal position values are different for each duplicate
#I have to use ensembl id to join the dataframes, because the gene names are not unique
#I have the ensembl id for some of my genes in my meta analysis all DEG dataframe. First, I need to add the ensembl IDs for the genes that do not have ensebl IDs.
#Maybe I can first filter my dataframe to only include the genes that do not have ensembl IDs, then I can add the ensembl IDs to those genes
meta_analysis_all_DEG_data_all_no_ensembl_ids <- meta_analysis_all_DEG_data %>%
  filter(is.na(ENSEMBL_ID))

ensembl_data <- getBM(
    attributes = c('external_gene_name', 'ensembl_gene_id'),
    filters = 'external_gene_name',
    values = meta_analysis_all_DEG_data$gene_name,
    mart = ensembl)

?getBM


#Now, I want to add the chromosomal position values to my meta_analysis_all_DEG_data dataframe
meta_analysis_all_DEG_data_anno <- meta_analysis_all_DEG_data %>%
  left_join(human_gene_info, by = c("gene_name" = "external_gene_name"))








