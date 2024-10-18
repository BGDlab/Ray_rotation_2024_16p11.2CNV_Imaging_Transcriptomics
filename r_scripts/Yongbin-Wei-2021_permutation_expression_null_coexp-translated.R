#This script will create a function to performs permutation testing to examine in which brain regions the input
# gene set is differentially expressed, in comparison to random genes with
# similar level of coexpression conserved

# INPUT
# %   geneset -- a cell array of gene symbols of the genes of interest
# %   
# %   OPTIONAL:
#   %   expressions -- a NxK matrix of gene expressions of all genes. N is the 
# %       number of genes, K is the number of genes. if not available, 
# %       default gene expression data in the DK114 atlas will be loaded.
# %   gene_symbols -- a cell array of gene symbols of all genes. This must be
# %       provided if EXPRESSIONS is provided. If not available, gene symbols
# %       will be loaded from the default gene expression data.

# % OUTPUT
# %   res.p -- two-tailed p-value in permutation testing
# %   res.mean_expressions -- regional mean expressions of the input GOI
# %   res.null_expressions -- regional mean expressions of random BRAIN genes
# %   res.difference -- difference between mean_expressions and the mean of
# %       null_expressions, indicating the effect direction.
# %   res.coexp_mean -- mean coexpression level among the input GOI
# %   res.permut_coexp_mean -- mean coexpression level among random genes
# %
# % REFERENCE
# %   Wei Y. et al., (2021) Statistical testing and annotation of gene 
# %   transcriptomic-neuroimaging associations, bioRxiv



permutation_expression_null_coexp <- function(geneset, expressions = NULL, gene_symbols = NULL) {
  # Check input data and set default expressions and gene symbols if not provided
  message('Running null-coexpression model')
  # function is used to send diagnostic messages to the standard error stream (stderr). These messages
  #are informational and are typically used to inform the user about the progress of a script or function.
  if (is.null(expressions) && is.null(gene_symbols)) {
    message('## Loading default gene expression data in DK114 atlas ...')
    # Load default gene expression data
    data_ge <- load('gene_expression.RData')
    expressions <- data_ge$mDataGEctx
    gene_symbols <- data_ge$gene_symbols
    regionDescriptions <- data_ge$regionDescriptionCtx
  } else if (is.null(gene_symbols)) {
    stop('Please provide gene symbols of all genes included in the expression data.')
  }
  
  N <- nrow(expressions)
  K <- ncol(expressions)
  message('## ', K, ' genes detected totally.')
  message('## ', N, ' brain regions detected.')
  
  NG <- length(geneset)
  message('## ', NG, ' gene(s) of the GOI detected.')
  if (NG == 1) {
    stop('Only 1 gene included in the GOI. Coexpression cannot be computed.')
  }
  
  NGA <- length(gene_symbols)
  if (NGA != K) {
    stop('The number of gene symbols is different from the number of genes in the expression data.')
  }
  
  II <- gene_symbols %in% geneset
  if (sum(II) == 0) {
    stop('None of the genes in the input gene set found in gene data.')
  }
  message('## ', sum(II), '/', NG, ' genes with gene expression data available.')
  
  # Perform permutation
  nPerm <- 1000
  
  # Raw mean expressions
  mGE <- rowMeans(expressions[, II, drop = FALSE], na.rm = TRUE)
  res <- list(mean_expressions = mGE)
  
  # Compute coexpression of the input GOI
  G <- expressions[, II, drop = FALSE]
  coexp_mat <- cor(G, use = 'pairwise.complete.obs')
  mask_tril <- lower.tri(coexp_mat)
  coexp <- mean(coexp_mat[mask_tril], na.rm = TRUE)
  message('## Mean coexpression: ', coexp)
  res$coexp_mean <- coexp
  
  # Initialize variables for permutations
  coexp_null <- numeric(nPerm)
  idx_rand_genes <- matrix(NA, nPerm, sum(II))
  tmpGE <- matrix(NA, N, nPerm)
  
  message('## Progress:     ')
  for (kk in 1:nPerm) {
    tmp_status <- FALSE
    while (!tmp_status) {
      res_rand <- y_rand_gs_coexp(expressions, coexp, sum(II))
      rid <- res_rand[[1]]
      coexp_null[kk] <- res_rand[[2]]
      tmp_status <- res_rand[[3]]
    }
    cat(sprintf('\r%.3d%%', round(kk/nPerm * 100)))
    
    idx_rand_genes[kk, ] <- rid
    tmpGE[, kk] <- rowMeans(expressions[, rid, drop = FALSE], na.rm = TRUE)
  }
  
  res$permut_gene_idx <- idx_rand_genes
  res$permut_coexp_mean <- coexp_null
  res$null_expressions <- tmpGE
  res$difference <- res$mean_expressions - rowMeans(res$null_expressions, na.rm = TRUE)
  
  # Compute p-value
  res$p <- sapply(1:N, function(ii) {
    P <- mean(tmpGE[ii, ] > mGE[ii])
    if (P > 0.5) {
      return((1 - P) * 2)
    } else {
      return(P * 2)
    }
  })
  
  if (exists('regionDescriptions')) {
    res$regionDescriptions <- regionDescriptions
  }
  
  message(' >> finished without errors')
  
  return(res)
}

# Helper function y_rand_gs_coexp to be defined in R
y_rand_gs_coexp <- function(expressions, target_coexp, num_genes) {
  # This function randomly selects `num_genes` genes and computes their coexpression,
  # returning the random gene indices, their coexpression, and a status indicating
  # whether the coexpression is within some range of the target.
  
  # Select random genes
  rid <- sample(ncol(expressions), num_genes, replace = FALSE)
  random_genes <- expressions[, rid]
  
  # Compute coexpression matrix
  coexp_mat <- cor(random_genes, use = 'pairwise.complete.obs')
  mask_tril <- lower.tri(coexp_mat)
  coexp <- mean(coexp_mat[mask_tril], na.rm = TRUE)
  
  # Set a status threshold for coexpression
  tmp_status <- abs(coexp - target_coexp) < 0.01  # Example threshold, adjust as needed
  
  return(list(rid, coexp, tmp_status))
}
