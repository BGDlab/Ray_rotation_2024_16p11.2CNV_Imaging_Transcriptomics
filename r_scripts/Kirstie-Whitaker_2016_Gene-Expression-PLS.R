#Gene expression PLS:
# This code was written by Dr Petra Vértes and is taken from Whitaker and Vértes, PNAS 2016, please cite that paper if you this code in your own work.
# I used chatgpt to translate the code, originally in matlab, to r
# So it may need some troubleshooting

# Part 1: I'm not sure what this does yet ----
# Chatgpt said ere’s an equivalent R version of your MATLAB code, using the pls package in R for Partial Least Squares (PLS) regression and related plotting:
# pls::plsr() is used for PLS regression.
# scale() normalizes the data (z-score transformation).
# explvar() extracts the percentage variance explained by each component.
# cor() computes the correlation.
# The permutation test reshuffles the response matrix Y and re-runs the PLS model, comparing the real variance explained to the distribution under random permutations.
# Make sure that the .mat file (gene_regional_expression_zscored.mat) is converted into an R-readable format (such as .RData or .csv) before loading
# it into R. You can use the R.matlab package to read .mat files directly in R if needed.



# Load necessary libraries
library(pls)
library(ggplot2)

# Load the data (assumes the file has been converted to R format, e.g., .RData or .csv) 
load('gene_regional_expression_zscored.RData')

# Define parameters
nregs <- 308
nregs_lh <- 152

# Extract predictors and response variables
X <- gene_regional_expression[1:nregs_lh, ]  # Predictors
Y <- cbind(mytstat_Maast_lh, mytstat_Dublin_lh, mytstat_Cobre_lh)  # Response variables

# Z-score normalization
X <- scale(X)
Y <- scale(Y)
?scale
# Perform full PLS and plot variance in Y explained by top 15 components
pls_model <- plsr(Y ~ X, ncomp = 15, scale = TRUE, validation = "none")

# Explained variance
explained_variance <- cumsum(100 * explvar(pls_model))

# Plot cumulative explained variance
dim <- 15
ggplot(data = data.frame(Comp = 1:dim, Variance = explained_variance[1:dim]), aes(x = Comp, y = Variance)) +
  geom_line(color = "red", size = 1.5) +
  geom_point(shape = 21, color = "red", fill = "white", size = 3) +
  labs(x = "Number of PLS components", y = "Percent Variance Explained in Y") +
  theme_minimal(base_size = 14)

# Re-run PLS with 2 components for consistency
dim <- 2
pls_model_dim2 <- plsr(Y ~ X, ncomp = dim, scale = TRUE, validation = "none")

# Plot correlation of PLS component 1 with Cobre t-statistic
XS_scores <- scores(pls_model_dim2)[, 1]
correlation_result <- cor(XS_scores, mytstat_Cobre_lh)
ggplot(data = data.frame(XS = XS_scores, Cobre = mytstat_Cobre_lh), aes(x = XS, y = Cobre)) +
  geom_point(color = "red") +
  labs(x = "XS scores for PLS component 1", y = "Cobre t-statistic - lh") +
  theme_minimal(base_size = 14)

# Permutation testing to assess significance of PLS results
rep <- 1000
R <- numeric(8)
p_values <- numeric(8)

for (dim in 1:8) {
  pls_model <- plsr(Y ~ X, ncomp = dim, scale = TRUE, validation = "none")
  explained_variance <- cumsum(100 * explvar(pls_model))[dim]
  Rsquared <- explained_variance
  
  Rsq <- numeric(rep)
  for (j in 1:rep) {
    Y_permuted <- Y[sample(1:nrow(Y)), ]
    pls_model_perm <- plsr(Y_permuted ~ X, ncomp = dim, scale = TRUE, validation = "none")
    Rsq[j] <- cumsum(100 * explvar(pls_model_perm))[dim]
  }
  
  R[dim] <- Rsquared
  p_values[dim] <- length(which(Rsq >= Rsquared)) / rep
}

# Plot p-values for each dimension
ggplot(data = data.frame(Comp = 1:8, p_val = p_values), aes(x = Comp, y = p_val)) +
  geom_point(shape = 21, color = "black", fill = "red", size = 4) +
  labs(x = "Number of PLS components", y = "p-value") +
  theme_minimal(base_size = 14)











# Part 2: Bootstrap to get the gene list: ----
#ChatGPT says Here’s an equivalent R code version of the provided MATLAB code, which uses
# Partial Least Squares (PLS) regression and bootstrapping for two components. The pls package
# and randsample functions in R are used for the PLS and bootstrap steps:
# The PLS model is created using the plsr() function from the pls package.
# Bootstrapping resamples the data and re-runs the PLS regression to check the stability of the PLS weights.
# We compute the standard deviation of weights over the bootstrap runs to calculate Z-scores.
# The results are saved in CSV files for further analysis, including gene names, indices, and Z-scores.



# Load necessary libraries
library(pls)
library(ggplot2)

# Load gene data (assumes genes20647 is loaded as 'genes' in R)
genes <- genes20647
geneindex <- 1:20647

# Number of bootstrap iterations
bootnum <- 1000

# Perform PLS with 2 components
dim <- 2
pls_model <- plsr(Y ~ X, ncomp = dim, scale = TRUE, validation = "none")

# Store regions' IDs and weights in descending order for both components
XS <- scores(pls_model)
stats <- pls_model$loading.weights  # Equivalent of MATLAB's 'stats.W'

# Correlate the PLS components with Dublin t-statistic
R1 <- cor(XS[, 1:2], mytstat_Dublin_lh)

# Align PLS components with the desired direction for interpretability
if (R1[1, 1] < 0) {
  stats[, 1] <- -stats[, 1]
  XS[, 1] <- -XS[, 1]
}
if (R1[2, 1] < 0) {
  stats[, 2] <- -stats[, 2]
  XS[, 2] <- -XS[, 2]
}

# Sort weights for PLS components 1 and 2
PLS1w <- sort(stats[, 1], decreasing = TRUE, index.return = TRUE)
x1 <- PLS1w$ix
PLS1ids <- genes[x1]
geneindex1 <- geneindex[x1]

PLS2w <- sort(stats[, 2], decreasing = TRUE, index.return = TRUE)
x2 <- PLS2w$ix
PLS2ids <- genes[x2]
geneindex2 <- geneindex[x2]

# Save ROI scores (PLS components) to CSV files
write.csv(XS[, 1], 'PLS1_ROIscores.csv', row.names = FALSE)
write.csv(XS[, 2], 'PLS2_ROIscores.csv', row.names = FALSE)

# Initialize variables to store weights from all bootstrap runs
PLS1weights <- matrix(, nrow = length(PLS1w$x), ncol = bootnum)
PLS2weights <- matrix(, nrow = length(PLS2w$x), ncol = bootnum)

# Start bootstrap
set.seed(123)  # For reproducibility
for (i in 1:bootnum) {
  myresample <- sample(1:nrow(X), nrow(X), replace = TRUE)
  
  Xr <- X[myresample, ]
  Yr <- Y[myresample, ]
  
  pls_model_boot <- plsr(Yr ~ Xr, ncomp = dim, scale = TRUE, validation = "none")
  stats_boot <- pls_model_boot$loading.weights
  
  # Extract and align PLS1 weights
  newW <- stats_boot[, 1][x1]
  if (cor(PLS1w$x, newW) < 0) {
    newW <- -newW
  }
  PLS1weights[, i] <- newW
  
  # Extract and align PLS2 weights
  newW <- stats_boot[, 2][x2]
  if (cor(PLS2w$x, newW) < 0) {
    newW <- -newW
  }
  PLS2weights[, i] <- newW
}

# Get standard deviation of weights from bootstrap runs
PLS1sw <- apply(PLS1weights, 1, sd)
PLS2sw <- apply(PLS2weights, 1, sd)

# Get bootstrap weights (Z-scores)
temp1 <- PLS1w$x / PLS1sw
temp2 <- PLS2w$x / PLS2sw

# Order bootstrap weights (Z-scores) and region names
Z1 <- sort(temp1, decreasing = TRUE, index.return = TRUE)
PLS1 <- PLS1ids[Z1$ix]
geneindex1 <- geneindex1[Z1$ix]

Z2 <- sort(temp2, decreasing = TRUE, index.return = TRUE)
PLS2 <- PLS2ids[Z2$ix]
geneindex2 <- geneindex2[Z2$ix]

# Print out results to CSV for GOrilla (for bootstrapped ordered list of genes)
fid1 <- file("PLS1_geneWeights.csv", "w")
for (i in 1:length(genes)) {
  writeLines(sprintf("%s, %d, %f", PLS1[i], geneindex1[i], Z1$x[i]), fid1)
}
close(fid1)

fid2 <- file("PLS2_geneWeights.csv", "w")
for (i in 1:length(genes)) {
  writeLines(sprintf("%s, %d, %f", PLS2[i], geneindex2[i], Z2$x[i]), fid2)
}
close(fid2)






# Part 3: Gene enrichments: ----
# This code calculates enrichments of the known gene lists (used for Gandal, DISEASES and GAD).
# The input gene list must be a list of entrez IDs.
# ChatGPT says Here’s the equivalent R code that matches the functionality of your MATLAB code
# for determining if known genes are enriched among a list of PLS-identified genes. This version
# uses which() for finding indices, median() for computing medians, and a randomization procedure
# to perform an enrichment test:
# Known gene positions (gene_pos): The loop searches for the position of each known gene (knowngenes)
# in the list of genes from the PLS1 component (names). If found, the position is recorded; otherwise, it's
# marked as NA.
# # Median calculation: The median position of the known genes (gene_pos_median) is calculated, ignoring NA values.
# # Randomization: A random sample of genes from the background is selected, and the median of their positions is
# compared to the observed gene_pos_median.
# # Enrichment test (p_pos_median): The fraction of random medians that are less than or equal to the observed median
# is computed, providing a p-value for enrichment.



# Load necessary libraries
library(dplyr)

# known genes (Entrez IDs) and names from PLS analysis
knowngenes <- gandal_up_entrez  # List of known genes (Entrez IDs)
names <- pls1_entrez  # Gene list from PLS component 1

# Initialize the position of known genes in the names list
gene_pos <- rep(NA, length(knowngenes))

# Find positions of known genes in the PLS1 gene list
for (index in 1:length(knowngenes)) {
  result <- which(names == knowngenes[index])
  if (length(result) > 0) {
    gene_pos[index] <- result
  }
}

# Calculate the median position of known genes (ignoring NA values)
gene_pos_median <- median(gene_pos, na.rm = TRUE)

# Check whether enriched:
lengthnonan <- sum(!is.na(gene_pos))  # Number of known genes found in the list
bground <- probeInfo_maxi_EntrezID  # Background gene list
bground_red <- intersect(bground, pls1_entrez)  # Reduced background intersecting with PLS1 genes

randno <- 10000  # Number of randomizations

# Randomization procedure
myRposmedian <- numeric(randno)

set.seed(123)  # For reproducibility
for (rand in 1:randno) {
  # Randomly pick `lengthnonan` genes from the background list
  randgenes <- sample(bground_red, lengthnonan, replace = FALSE)
  
  # Find the positions of these random genes in the PLS1 gene list
  positionR <- numeric(lengthnonan)
  for (index in 1:lengthnonan) {
    positionR[index] <- which(names == randgenes[index])
  }
  
  # Store the median of the positions
  myRposmedian[rand] <- median(positionR)
}

# Sort random medians in descending order
myRposmedian_sort <- sort(myRposmedian, decreasing = TRUE)

# Calculate p-value for observed gene_pos_median compared to random medians
p_pos_median <- sum(myRposmedian_sort <= gene_pos_median) / randno

# Output the results
gene_pos_median
p_pos_median
