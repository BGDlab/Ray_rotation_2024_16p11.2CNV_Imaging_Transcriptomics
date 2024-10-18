#In this script I plan to do correlation analysis between the AHBA transcriptpmic data from my hihgly significant DEG list
# and Smrithi's correlation coefficients for the MRI brain regions

#Importing necessary packages----
library(tidyverse)


#Importing relevant datasheets----
setwd("/Users/tammyray/Desktop/Aaron_16p_Imaging_Transcriptomics/output")

# Importing normed AHBdataset
normed_AHBA_sigDEG <- read_csv("Normed_AHBA_sig_genes_ONLY.csv")

# Importing Smrithi's MRI dataset
MRI <- read_csv("16panalysis_BH_adjusted.csv")

# Importing 16p gene list
setwd("/Users/tammyray/Desktop/Aaron_16p_Imaging_Transcriptomics/CSV_data_sheets/transcriptomic_datasheets")
chr16p11.2_gene_list_Kusenda2015 <-  read.csv("Kusenda_2015_TableS1_16p11.2_gene_list.csv")
sixteen_p_11.2_genes <- chr16p11.2_gene_list_Kusenda2015$Gene.symbol
save(sixteen_p_11.2_genes, file = "16p11.2_gene_list.csv")



# Tidying datasets----
# We decided that we only want to use model 3, Spline TCV model (decided w Smrithi meeting 8/24/24)
# We are only using the deletion data for now
MRI_model3 <- MRI %>%
  filter(Genotype == "Genotype16pDeletion...5")
#Now I need to remove non-cerebral values


# For the normed_AHBA_sigDEG dataset, I want to get a list of all unique values in the region column
# I will then use this list to filter the MRI dataset
AHBA_regions <- unique(normed_AHBA_sigDEG$region)
MRI_regions <- unique(MRI_model3$VolumetricComponent)

# Outside of R, I have made a CSV of corresponding AHBA and SMrithi MRI data region names. Firt column is AHBA names, second column is MRI names.
# Importing the data
setwd("/Users/tammyray/Desktop/Aaron_16p_Imaging_Transcriptomics/CSV_data_sheets")
standard_region_names <- read_csv("AHBA_MRI_region_names.csv")

#MRI regions to remove. I made this list manually using google sheets and reformatted it for R using chatgpt
MRI_regions_to_remove <- c("L.Cerebral_WM", "L.cerebral_cortex", "L.lateral_ventricle", "L.inferior_lateral_ventricle", "L.cerebellum_WM", "L.cerebellum_cortex", 
                           "L.thalamus", "L.caudate", "L.putamen", "L.pallidum", "L.accumbens", "L.hippocampus", "L.amygdala", "L.ventral_DC", "Third_Ventricle", 
                           "Fourth_Ventricle", "Brainstem", "CSF", "R.Cerebral_WM", "R.cerebral_cortex", "R.lateral_ventricle", "R.inferior_lateral_ventricle", 
                           "R.cerebellum_WM", "R.cerebellum_cortex", "R.thalamus", "R.caudate", "R.putamen", "R.pallidum", "R.hippocampus", "R.amygdala", "R.accumbens", 
                           "R.ventral_DC", "R.ctx_bankssts", "R.ctx_caudalanteriorcingulate", "R.ctx_caudalmiddlefrontal", "R.ctx_cuneus", "R.ctx_entorhinal", "R.ctx_fusiform", 
                           "R.ctx_inferiorparietal", "R.ctx_inferiortemporal", "R.ctx_isthmuscingulate", "R.ctx_lateraloccipital", "R.ctx_lateralorbitofrontal", "R.ctx_lingual", 
                           "R.ctx_medialorbitofrontal", "R.ctx_middletemporal", "R.ctx_parahippocampal", "R.ctx_paracentral", "R.ctx_parsopercularis", "R.ctx_parsorbitalis", 
                           "R.ctx_parstriangularis", "R.ctx_pericalcarine", "R.ctx_postcentral", "R.ctx_posteriorcingulate", "R.ctx_precentral", "R.ctx_precuneus", 
                           "R.ctx_rostralanteriorcingulate", "R.ctx_rostralmiddlefrontal", "R.ctx_superiorfrontal", "R.ctx_superiorparietal", "R.ctx_superiortemporal", 
                           "R.ctx_supramarginal", "R.ctx_frontalpole", "R.ctx_temporalpole", "R.ctx_transversetemporal", "R.ctx_insula", "cGMV", "WMV", "sGMV", "Ventricles", 
                           "Cerebellum", "Accumbens", "Ventral_Diencephalon", "Pallidum", "Hippocampus", "Caudate", "Cerebral_White_Matter", "Lateral_Ventricle", "Cerebral_Cortex", 
                           "Thalamus", "Putamen", "Amygdala", "Cerebellar_White_Matter", "Cerebellar_Cortex", "Inferior_Lateral_Ventricle", "ctx_bankssts", "ctx_caudalanteriorcingulate", 
                           "ctx_caudalmiddlefrontal", "ctx_cuneus", "ctx_entorhinal", "ctx_fusiform", "ctx_inferiorparietal", "ctx_inferiortemporal", "ctx_isthmuscingulate", 
                           "ctx_lateraloccipital", "ctx_lateralorbitofrontal", "ctx_lingual", "ctx_medialorbitofrontal", "ctx_middletemporal", "ctx_parahippocampal", 
                           "ctx_paracentral", "ctx_parsopercularis", "ctx_parsorbitalis", "ctx_parstriangularis", "ctx_pericalcarine", "ctx_postcentral", "ctx_posteriorcingulate", 
                           "ctx_precentral", "ctx_precuneus", "ctx_rostralanteriorcingulate", "ctx_rostralmiddlefrontal", "ctx_superiorfrontal", "ctx_superiorparietal", 
                           "ctx_superiortemporal", "ctx_supramarginal", "ctx_frontalpole", "ctx_temporalpole", "ctx_transversetemporal", "ctx_insula")

# removing rows for the regions we aren't including
MRI_model3_leftHem <- MRI_model3 %>%
  filter(VolumetricComponent %in% MRI_regions_to_remove == FALSE) %>%
  rename(region = VolumetricComponent)

# Now, I want to use the standard region

standard_region_names <- standard_region_names %>%
  rename(region = "left hemisphere Smrithi regions",
         new_region = "AHBA regions")


# Use left_join to map the new names based on the region variable
MRI_model3_leftHem_new_regionNames <- MRI_model3_leftHem %>%
  left_join(standard_region_names, by = "region") %>%   # Join based on the region variable
  mutate(region = ifelse(!is.na(new_region), new_region, region)) %>%   # Replace region names with new ones
  select(-new_region)  # Remove the extra column if needed

# Yay!!!! Now I have the dataset I need, i think! Now I want to merge this data with the AHAB dataset. I'm not sure how exactly to do this.
# Maybe I could keep them in separate dataframes?
# But maybe it would be easier to combine them into one dataframe?
# I want to try combining them
# I will use the region column to merge the datasets

AHBA_MRI_merged <- full_join(normed_AHBA_sigDEG, MRI_model3_leftHem_new_regionNames, by = "region", multiple = "all")

colnames(AHBA_MRI_merged)[1] <- "number"


AHBA_MRI_merged_organized <- AHBA_MRI_merged %>%
  select(-c(number, Genotype))

colnames(AHBA_MRI_merged_organized)[3] <- "AHBA_expression"
colnames(AHBA_MRI_merged_organized)[8] <- "t_value"

#Exporting as csv AHBA_MRI_merged_organized
setwd("/Users/tammyray/Desktop/Aaron_16p_Imaging_Transcriptomics/CSV_data_sheets")
write_csv(AHBA_MRI_merged_organized, "AHBA_MRI_merged.csv")

# Pearson correlation analysis ----
# I want to do a correlation analysis between the AHBA expression and the MRI data

# Function to compute correlation and p-value, created by chat-gpt
correlation_with_pvalue_pearson <- function(x, y) {
  test <- cor.test(x, y, method = "pearson")
  return(data.frame(correlation = test$estimate, p_value = test$p.value))
}

correlation_with_pvalue_spearman <- function(x, y) {
  test <- cor.test(x, y, method = "spearman")
  return(data.frame(correlation = test$estimate, p_value = test$p.value))
}



# Group by gene_symbol and compute correlation between AHBA_expression and Estimate
cor_results_Estimate_pearson <- AHBA_MRI_merged_organized %>%
  group_by(gene_symbol) %>%
  summarize(correlation = cor(AHBA_expression, Estimate, method = "pearson"),
            p_value = correlation_with_pvalue_pearson(AHBA_expression, Estimate)$p_value) %>%
  mutate(in_16p_locus = ifelse(gene_symbol %in% sixteen_p_11.2_genes, "Yes", "No"))


#Trying a spearman correlation analysis instead
cor_results_Estimate_spearman <- AHBA_MRI_merged_organized %>%
  group_by(gene_symbol) %>%
  summarize(correlation = cor(AHBA_expression, Estimate, method = "spearman"),
            p_value = cor.test(AHBA_expression, Estimate, method = "spearman")$p.value) %>%
  mutate(in_16p_locus = ifelse(gene_symbol %in% sixteen_p_11.2_genes, "Yes", "No"))



# Doing the same thing for the t value
# Pearson correlation
cor_results_t_value_pearson <- AHBA_MRI_merged_organized %>%
  group_by(gene_symbol) %>%
  summarize(correlation = cor(AHBA_expression, t_value, method = "pearson"),
            p_value = correlation_with_pvalue_pearson(AHBA_expression, t_value)$p_value) %>%
  mutate(in_16p_locus = ifelse(gene_symbol %in% sixteen_p_11.2_genes, "Yes", "No"))

#Spearman correlation
cor_results_t_value_spearman <- AHBA_MRI_merged_organized %>%
  group_by(gene_symbol) %>%
  summarize(correlation = cor(AHBA_expression, t_value, method = "spearman"),
            p_value = cor.test(AHBA_expression, t_value, method = "spearman")$p.value) %>%
  mutate(in_16p_locus = ifelse(gene_symbol %in% sixteen_p_11.2_genes, "Yes", "No"))



#Exporting these dataframes
setwd("/Users/tammyray/Desktop/Aaron_16p_Imaging_Transcriptomics/CSV_data_sheets")
write_csv(cor_results_Estimate_pearson, "AHBA_MRI_correlation_Estimate_Pearson.csv")
write_csv(cor_results_Estimate_spearman, "AHBA_MRI_correlation_Estimate_Spearman.csv")
write_csv(cor_results_t_value_pearson, "AHBA_MRI_correlation_t_value_Pearson.csv")
write_csv(cor_results_t_value_spearman, "AHBA_MRI_correlation_t_value_Spearman.csv")

#Now I want to plot the correlation coefficients and p values for the genes in the 16p locus and the genes not in the 16p locus

#Scatrerplot showing relationship between p value and correlation, not super useful but educational for me!
ggplot(cor_results_t_value_spearman, aes(x=p_value, y=correlation, color=in_16p_locus)) +
  geom_point() +
  labs(title = "Spearman correlation of t-value",
       x = "p value",
       y = "correlation") +
  theme_minimal() + 
  geom_vline(aes(xintercept = 0.05))

#making a density curve instead?
ggplot(cor_results_t_value_spearman, aes(x=correlation, color=in_16p_locus)) +
  geom_density() +
  labs(title = "Spearman correlation of t-value",
       x = "Correlation Coefficient",
       y = "Density") +
  theme_minimal()

# making a histogram instead
ggplot(cor_results_t_value_spearman, aes(x=correlation, fill=in_16p_locus)) +
  geom_histogram(position = "identity", alpha = 0.6) +
  labs(title = "Spearman correlation of t-value",
       x = "Correlation Coefficient",
       y = "Frequency") +
  theme_minimal() +
  xlim(-0.6, 0.6) + ylim(0, 8)



ggplot(cor_results_Estimate_spearman, aes(x=correlation, fill=in_16p_locus)) +
  geom_histogram(position = "identity", alpha = 0.6) +
  labs(title = "Spearman correlation of Estimate(~effect size)",
       x = "Correlation Coefficient",
       y = "Frequency") +
  theme_minimal() +
  xlim(-0.6, 0.6) + ylim(0, 8)




# Plotting the data ----
library("ggpubr")
library(plotly)

#First, making 1 plot for 1 gene: let's do LGALS1 since in the cor_results, it has the alrgest p vlaue
LGALS1_data <- AHBA_MRI_merged_organized %>% filter(gene_symbol == "LGALS1")

ggplot(LGALS1_data, aes(x = AHBA_expression, y = "t value", color = region)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, color = "blue", linewidth = 0.5) +  # Add a line of best fit (linear regression)
  labs(title = "LGALS1 Expression by Region",
       x = "AHBA Expression",
       y = "Estimate") +
  theme_minimal()

#Making a huge plot with a lot of little plots inside (courtesy of facet_wrap) of all genes
scatter_plot_estimate <- ggplot(AHBA_MRI_merged_organized, aes(x = AHBA_expression, y = Estimate, color = region)) +
  geom_point(size = 0.1) + 
  geom_smooth(method = "lm", se = FALSE, color = "blue", linewidth = 0.5) +  # Add a line of best fit (linear regression)
  labs(title = "Scatter plots of AHBA Expression vs Estimate for all Genes",
       x = "AHBA Expression",
       y = "Estimate") +
  facet_wrap(~ gene_symbol, scales = "free") +  # Separate plots for each gene
  theme_minimal() +
  xlim(0, 1) + ylim(-0.7, 0.7) #+
  stat_cor(aes(label = paste(..r.label.., sep = "~`,`~")), 
         method = "pearson", label.x = 3, label.y = 0)  # Add correlation coefficients to the plot

# Convert to a plotly object
interactive_plot_estimate <- ggplotly(scatter_plot_estimate, tooltip = c("text"))
interactive_plot_estimate # Display the interactive plot

#Doing the same plot, but for t-value
scatter_plot_estimate <- ggplot(AHBA_MRI_merged_organized, aes(x = AHBA_expression, y = Estimate, color = region)) +
  geom_point(size = 0.1) + 
  geom_smooth(method = "lm", se = FALSE, color = "blue", linewidth = 0.5) +  # Add a line of best fit (linear regression)
  labs(title = "Scatter plots of AHBA Expression vs Estimate for all Genes",
       x = "AHBA Expression",
       y = "Estimate") +
  facet_wrap(~ gene_symbol, scales = "free") +  # Separate plots for each gene
  theme_minimal() +
  stat_cor(aes(label = paste(..r.label.., sep = "~`,`~")), 
           method = "pearson", label.x = 3, label.y = 0)  # Add correlation coefficients to the plot

# Convert to a plotly object
interactive_plot_estimate <- ggplotly(scatter_plot, tooltip = c("text"))
interactive_plot # Display the interactive plot




# Doing the above plotting for the 16p genes ----
AHBA_MRI_merged_organized_IN_16p_locus <- AHBA_MRI_merged_organized %>% filter(in_16p_region == "Yes")



scatter_plot_16p_genes <- ggplot(AHBA_MRI_merged_organized_IN_16p_locus, aes(x = AHBA_expression, y = Estimate, color = region)) +
  geom_point(size = 0.1) + 
  geom_smooth(method = "lm", se = FALSE, color = "blue", linewidth = 0.5) +  # Add a line of best fit (linear regression)
  labs(title = "Scatter plots of AHBA Expression vs Estimate for 16p Genes",
       x = "AHBA Expression",
       y = "Estimate") +
  facet_wrap(~ gene_symbol, scales = "free") +  # Separate plots for each gene
  theme_minimal() +
  stat_cor(aes(label = paste(..r.label.., sep = "~`,`~")), 
           method = "pearson", label.x = 3, label.y = 0)  # Add correlation coefficients to the plot

# Convert to a plotly object
interactive_plot_16p_genes <- ggplotly(scatter_plot_16p_genes, tooltip = c("text"))

# Display the interactive plot
interactive_plot_16p_genes

# Doing the above plotting for the NON 16p genes ----
AHBA_MRI_merged_organized_NOT_IN_16p_locus <- AHBA_MRI_merged_organized %>% filter(in_16p_region == "No")



scatter_plot_NOT_16p_genes <- ggplot(AHBA_MRI_merged_organized_NOT_IN_16p_locus, aes(x = AHBA_expression, y = Estimate, color = region)) +
  geom_point(size = 0.1) + 
  geom_smooth(method = "lm", se = FALSE, color = "blue", linewidth = 0.5) +  # Add a line of best fit (linear regression)
  labs(title = "Scatter plots of AHBA Expression vs Estimate for Genes NOT in 16p locus",
       x = "AHBA Expression",
       y = "Estimate") +
  facet_wrap(~ gene_symbol, scales = "free") +  # Separate plots for each gene
  theme_minimal() +
  stat_cor(aes(label = paste(..r.label.., sep = "~`,`~")), 
           method = "pearson", label.x = 3, label.y = 0)  # Add correlation coefficients to the plot

# Convert to a plotly object
interactive_plot_NOT_16p_genes <- ggplotly(scatter_plot_NOT_16p_genes, tooltip = c("text"))

# Display the interactive plot
interactive_plot_NOT_16p_genes
scatter_plot




# Exporting AHBA_MRI_merged_organized dataset




# Adding correlation coefficiencies and p values to plots!! ----


# Assume cor_results contains gene_symbol, correlation, and p_value
# Merge main data with correlation results
plot_data_t_value_spearman <- AHBA_MRI_merged_organized %>%
  left_join(cor_results_t_value_spearman, by = "gene_symbol")  # Merge by gene_symbol

# Create a custom facet label that includes the gene name, correlation, and p-value
plot_data_t_value_spearman <- plot_data_t_value_spearman %>%
  mutate(facet_label = paste0(gene_symbol, "\n", 
                              "| R: ", round(correlation, 2), 
                              ", p: ", round(p_value, 3)))

# Create the plot with custom facet labels
ggplot(plot_data_t_value_spearman, aes(x = AHBA_expression, y = t_value, color = region)) +
  geom_point(size = 0.1) + 
  geom_smooth(method = "lm", se = FALSE, color = "blue", linewidth = 0.5) +  # Add a line of best fit (linear regression)
  labs(title = "Scatter plots of AHBA Expression vs t-value for all Genes",
       x = "AHBA Expression",
       y = "t-value") +
  facet_wrap(~ facet_label, scales = "free") +  # Use the custom facet label
  theme_minimal() #+
  xlim(0, 1) + ylim(-0.7, 0.7)  # Adjust axis limits





scatter_plot_estimate <- ggplot(AHBA_MRI_merged_organized, aes(x = AHBA_expression, y = Estimate, color = region)) +
  geom_point(size = 0.1) + 
  geom_smooth(method = "lm", se = FALSE, color = "blue", linewidth = 0.5) +  # Add a line of best fit (linear regression)
  labs(title = "Scatter plots of AHBA Expression vs Estimate for all Genes",
       x = "AHBA Expression",
       y = "Estimate") +
  facet_wrap(~ gene_symbol, scales = "free") +  # Separate plots for each gene
  theme_minimal() +
  xlim(0, 1) + ylim(-0.7, 0.7) #+
stat_cor(aes(label = paste(..r.label.., sep = "~`,`~")), 
         method = "pearson", label.x = 3, label.y = 0)  # Add correlation coefficients to the plot

# Convert to a plotly object
interactive_plot_estimate <- ggplotly(scatter_plot_estimate, tooltip = c("text"))
interactive_plot_estimate # Display the interactive plot

