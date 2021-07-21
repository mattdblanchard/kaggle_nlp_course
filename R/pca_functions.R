# this script contains functions for PCA

# variance explained
var_table <- function() {
  if (n_comp == 1) {
    a <- data.frame(component = 1:length(fit$communality),
                    eigen = round(fit$values,2),
                    prop_var = c(fit$Vaccounted[2,c(1:n_comp)], rnorm(length(fit$communality)-n_comp, 0, 0)),
                    cum_var = c(fit$Vaccounted[2,c(1:n_comp)], rnorm(length(fit$communality)-n_comp, 0, 0)),
                    rotation_SS_load = c(fit$Vaccounted[1,c(1:n_comp)], rnorm(length(fit$communality)-n_comp, 0, 0))) %>% 
      round(2)
  } else {
    a <- data.frame(component = 1:length(fit$communality),
                    eigen = round(fit$values,2),
                    prop_var = c(fit$Vaccounted[2,c(1:n_comp)], rnorm(length(fit$communality)-n_comp, 0, 0)),
                    cum_var = c(fit$Vaccounted[3,c(1:n_comp)], rnorm(length(fit$communality)-n_comp, 0, 0)),
                    rotation_SS_load = c(fit$Vaccounted[1,c(1:n_comp)], rnorm(length(fit$communality)-n_comp, 0, 0))) %>% 
      round(2)
  
  }
  fit$Vaccounted[2,c(1:n_comp)]
  a[a == 0] <- ""
  
  a %>% 
    knitr::kable(booktabs = T, caption = "Variance accounted for by components") %>%
    kableExtra::kable_styling(font_size = 10, latex_options = "HOLD_position")
}

# pattern matrix
pattern_matrix <- function() {
  
  load <- data.frame(var = rownames(fit$loadings),
                     PC1 = round(fit$loadings[1:length(fit$communality)], 2),
                     PC2 = round(fit$loadings[(1+length(fit$communality)):(length(fit$communality)*2)], 2),
                     PC3 = round(fit$loadings[(1+length(fit$communality)*2):(length(fit$communality)*3)], 2),
                     PC4 = round(fit$loadings[(1+length(fit$communality)*3):(length(fit$communality)*4)], 2),
                     PC5 = round(fit$loadings[(1+length(fit$communality)*4):(length(fit$communality)*5)], 2),
                     PC6 = round(fit$loadings[(1+length(fit$communality)*5):(length(fit$communality)*6)], 2),
                     PC7 = round(fit$loadings[(1+length(fit$communality)*6):(length(fit$communality)*7)], 2),
                     PC8 = round(fit$loadings[(1+length(fit$communality)*7):(length(fit$communality)*8)], 2),
                     PC9 = round(fit$loadings[(1+length(fit$communality)*8):(length(fit$communality)*9)], 2),
                     h2 = round(fit$communality, 2)) %>% 
    pivot_longer(starts_with("PC"), names_to = "pc", values_to = "val") %>% 
    mutate(val = ifelse(abs(val) < .3, "", val)) %>% 
    pivot_wider(names_from = pc, values_from = val) %>% 
    arrange(desc(PC1), desc(PC2), desc(PC3), desc(PC4), desc(PC5), desc(PC6), desc(PC7), desc(PC8), desc(PC9)) %>%
    select(var, PC1:paste0("PC", n_comp), h2)
  
  load %>% 
    knitr::kable(booktabs = T, caption = "Pattern Matrix") %>%
    kableExtra::kable_styling(font_size = 10, latex_options = "HOLD_position")
}


# correlations between components
pca_cor <- function() {
round(fit$r.scores,2) %>% 
  knitr::kable(booktabs = T, caption = "Correlations between components") %>%
  kableExtra::kable_styling(font_size = 10, latex_options = "HOLD_position")
}


# correlation matrix with stars to indicate sig.
corstarsl <- function(x){ 
  require(Hmisc) 
  require(tidyverse)
  x <- as.matrix(x) 
  R <- rcorr(x)$r 
  p <- rcorr(x)$P 
  
  ## define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "* ", " ")))
  
  ## trunctuate the matrix that holds the correlations to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1] 
  
  ## build a new matrix that includes the correlations with their apropriate stars 
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x)) 
  diag(Rnew) <- paste(diag(R), " ", sep="") 
  rownames(Rnew) <- colnames(x) 
  colnames(Rnew) <- paste(colnames(x), "", sep="") 
  
  ## remove lower triangle
  Rnew <- as.matrix(Rnew)
  Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
  Rnew <- as.data.frame(Rnew) 
  
  ## remove last column and return the matrix (which is now a data frame)
  Rnew <- cbind(Rnew[2:length(Rnew)])
  
  Rnew <- Rnew %>% 
    rownames_to_column() %>% 
    mutate_if(is.factor, as.character) %>% 
    mutate_all(list(~str_replace(., "0.", ".")))
  
  rownames(Rnew) <- Rnew$rowname
  
  Rnew <- Rnew %>% 
    select(-rowname)
  
  # rownames(Rnew) <- renamevarnames(rownames(Rnew))
  rownames(Rnew) <- c(paste0(1:nrow(Rnew), ". ", rownames(Rnew)))
  colnames(Rnew) <- 2:(ncol(Rnew)+1)
  
  return(Rnew) 
}
