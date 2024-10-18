#I am going to try and work with Smrithi's 16p data

#First, I will import the data
SixteenP_data <- read.csv("SynthSegQC2.csv")

#I do not know what these values mean... what do these variable names refer to?
#First, what is SynthSeg?
#From the paper: SynthSeg: Segmentation of brain MRI scans of any contrast and resolution without retraining, 2023 Medical Image Analysis
#the first segmentation CNN robust against changes in contrast and resolution. SynthSeg is trained with synthetic data sampled from a
#generative model conditioned on segmentations. 

#SynthSeg can segment real scans from a wide range of target domains without retraining or fine-tuning, which enables straightforward
#analysis of huge amounts of heterogeneous clinical data.

#CNN: convolutional neural networks
#From IBM: https://www.ibm.com/topics/convolutional-neural-networks
#Neural Networks are a subset of machine learning and are at the core of deep learning algorithms. Their name and structure are inspired
#by the human brain, mimicking the way that biological neurons signal to one another. They are comprised of node layers, containing an input layer,
#one or more hidden layers, and an output layer. Each node connects to another and has an associated weight and threshold. If the output of any
#individual node is above the specified threshold value, that node is activated, sending data to the next layer of the network. Otherwise, no data
#is passed along to the next layer of the network.
#Convolutional neural networks use three-dimensional data for image classification and object recognition tasks.


#OK, I am going to change
#TIV = total intracranial volume. Everything enclosed in the skull





# Smrithi's r script ----


library(tidyverse)
library(car)
library(MASS)
library(modelr)
library(broom)
library(splines)
library(lmtest)
library(purrr)
library(readr)

# Set working directory
setwd("/Users/tammyray/Desktop/Aaron_16p_Imaging_Transcriptomics/CSV_data_sheets")

# Load data
Full16pF <- read.csv("SynthSegQC2.csv")

# Manually list the known non-region columns
non_region_columns <- c("study_id", "subject_id", "session_id", "Sex", "Age_In_Days", 
                        "Age_in_Years", "Genotype", "Autism", "scan_id", "Processing", 
                        "QCAvg", "WM_qc","Thalamus_qc", "Hippocampus_qc","Putamen_qc", "Brainstem_qc", "TCV", "TIV", "GM_qc", "CSF_qc", "Cerebellum_qc", "GM_qc",
                        "SeriesDescription", "Site", "Manufacturer")

# Identify region columns by excluding non-region columns
regions <- setdiff(names(Full16pF), non_region_columns)


# Function to perform analysis for a given region
analyze_region <- function(data, region) {
  region_var <- sym(region)  # Convert the region name to a symbol for dplyr
  
  clean_data <- data %>% 
    dplyr::select(subject_id, Age_in_Years, Sex, Genotype, TCV, !!region_var, QCAvg, Site) %>% 
    filter(complete.cases(.)) %>% 
    mutate(Sex = factor(trimws(Sex), levels = c("Male", "Female")),
           Genotype = factor(Genotype, levels = c("Control", "16pDeletion", "16pDuplication")),
           Site = factor(Site)) %>% 
    mutate(across(c(Sex, Genotype, Site), ~ factor(.))) %>%
    mutate(z_var = scale(!!region_var, center = TRUE, scale = TRUE),
           z_TCV = scale(TCV, center = TRUE, scale = TRUE),
           z_Age_in_Years = scale(Age_in_Years, center = TRUE, scale = TRUE),
           z_QCAvg = scale(QCAvg, center = TRUE, scale = TRUE))
  
  z_spline_Age <- ns(clean_data$z_Age_in_Years, df = 3)
  z_spline_TCV <- ns(clean_data$z_TCV, df = 3)
  
  z_model1 <- lm(z_var ~ z_spline_Age + Sex + Genotype + Site + z_QCAvg, data = clean_data)
  z_model2 <- lm(z_var ~ z_spline_Age + Sex + z_TCV + Genotype + Site + z_QCAvg, data = clean_data)
  z_model3 <- lm(z_var ~ z_spline_Age + Sex + z_spline_TCV + Genotype + Site + z_QCAvg, data = clean_data)
  
  clean_data <- clean_data %>%
    mutate(log_var = log(!!region_var),
           log_TCV = log(TCV),
           z_log_var = scale(log_var, center = TRUE, scale = TRUE),
           z_log_TCV = scale(log_TCV, center = TRUE, scale = TRUE))
  
  z_model4 <- lm(z_log_var ~ z_spline_Age + Sex + z_log_TCV + Site + z_QCAvg + Genotype, data = clean_data)
  
  model_summaries <- list(
    model1 = summary(z_model1)$coefficients[c("Genotype16pDeletion", "Genotype16pDuplication"),],
    model2 = summary(z_model2)$coefficients[c("Genotype16pDeletion", "Genotype16pDuplication"),],
    model3 = summary(z_model3)$coefficients[c("Genotype16pDeletion", "Genotype16pDuplication"),],
    model4 = summary(z_model4)$coefficients[c("Genotype16pDeletion", "Genotype16pDuplication"),]
  )
  
  models_df <- bind_rows(lapply(model_summaries, as.data.frame), .id = "Model") %>%
    rownames_to_column(var = "Genotype") %>%
    mutate(VolumetricComponent = region,
           Significance = ifelse(`Pr(>|t|)` < 0.001, "***",
                                 ifelse(`Pr(>|t|)` < 0.01, "**",
                                        ifelse(`Pr(>|t|)` < 0.05, "*", " "))),
           SigPos = ifelse(Estimate > 0, Estimate + `Std. Error` + 0.1, Estimate - `Std. Error` - 0.1))
  
  return(models_df)
}


all_results <- map_dfr(regions, ~ analyze_region(Full16pF, .x))

# Apply BH correction within each model
all_results <- all_results %>%
  group_by(Model) %>%
  mutate(adjusted_p = p.adjust(`Pr(>|t|)`, method = "BH")) %>%
  ungroup()

# Save the results with adjusted p-values to a CSV file
write_csv(all_results, "16panalysis_BH_adjusted.csv")

# Looking at significant values for deletions from each different model
all_model1_3X_sig_results <- all_results %>%
  filter(Significance == "***", Model == "model1", Genotype == "Genotype16pDeletion...1")

all_model2_3X_sig_results <- all_results %>%
  filter(Significance == "***", Model == "model2", Genotype == "Genotype16pDeletion...3")

all_model3_3X_sig_results <- all_results %>%
  filter(Significance == "***", Model == "model3", Genotype == "Genotype16pDeletion...5")

all_model4_3X_sig_results <- all_results %>%
  filter(Significance == "***", Model == "model4", Genotype == "Genotype16pDeletion...7")
# From looking at the results, there are a lot more significant results in model 1 than in the other models...

#Now I want to see the differences in what regions are significant between sides of the brain
#I think I can do this by making a vector or a string out of the VolumetricComponent column, and then comparing the lists or vectors to see what values are diferent...
#But I only want to do this for one model... maybe I just pull out strings containing "insula", for example, and then I can see which of the right insula, left insula,
# and combined insula are significant. 

?lm
?scale
?ns
?p.adjust

