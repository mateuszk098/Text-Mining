# Author: Mateusz Kowalczyk

# Text mining
library(tidytext)
library(tm)

# Similarity
library(proxy)

# Variables conditioning
library(tidyr)
library(dplyr)

# Plots
library(ggplot2)
library(grid)
library(gridExtra)
                         
# We choose pairs from matrix with the highest score (for cosine or scalar)
top_data <- function(matrix, n){
        
        data <- data.frame(matrix) %>% 
                gather(key = "key", value = "value") %>% 
                top_n(2 * n, wt = value) %>% 
                arrange(desc(value)) # sort by value
        
        # Replace pairs to one expression doc-doc
        data <- aggregate(data[1], data[-1], FUN = function(x) paste(unique(x), collapse = "-"))
        
        return(data)
}

# Data treatment for scalar
data_scalar <- function(method, n){
        
        data(acq)
        
        tdm <- TermDocumentMatrix(acq, control = list(removePunctuation = TRUE, removeNumbers = TRUE, 
                                                      stopwords = TRUE, stemming = TRUE, weighting = method))
        tdm <- as.matrix(tdm)
        
        scalar_dot <- function(x, k) x[,k] %*% x
        
        k <- seq(1, length(acq))
        data_scalar <- as.matrix(sapply(k, function(k) scalar_dot(tdm, k)))
        rownames(data_scalar) <- labels(acq)
        colnames(data_scalar) <- labels(acq)
        
        # Data from from exactly the same docs are not interesting
        diag(data_scalar) <- 0
        
        data <- top_data(data_scalar, n)
        
        return(data)
}

# Data treatment for cosine
data_cosine <- function(method, n){
        
        data(acq)
        
        tdm <- TermDocumentMatrix(acq, control = list(removePunctuation = TRUE, removeNumbers = TRUE, 
                                                      stopwords = TRUE, stemming = TRUE, weighting = method))
        tdm <- as.matrix(tdm)
        
        # Cosine similarity beetwen every document pair
        cosine_simil <- as.matrix(simil(t(tdm), method = "cosine"))
        
        # Values from exactly the same docs are not interesting
        diag(cosine_simil) <- 0
        
        data <- top_data(cosine_simil, n)
        
        return(data)
}

# Histogram 
hist_func <- function(data, title){
        
        hist <- ggplot() + 
                geom_col(aes(x = data$value, y = reorder(data$key, data$value), fill = data$value), width = 0.7) +
                ggtitle(title) +
                theme_minimal() + 
                theme(plot.title = element_text(hjust = 0.5)) + 
                labs(x = "Similarity", y = "Document Pairs", fill = "Similarity") +
                scale_colour_brewer(palette = "Blues")
        
        return(hist)
}

# Call the functions

# Cosine similarity
cosine_tf <- data_cosine(method = weightTf, n = 10)
cosine_tf_idf <- data_cosine(method = weightTfIdf, n = 10)

# Scalar dot product 
scalar_tf <- data_scalar(method = weightTf, n = 10)
scalar_tf_idf <- data_scalar(method = weightTfIdf, n = 10)

# Cosine similarity Histogram 
hist_cosine_tf <- hist_func(data = cosine_tf, title = "Method - TF")
hist_cosine_tf_idf <- hist_func(data = cosine_tf_idf, title = "Method - TF-IDF")

# Scalar dot product Histogram 
hist_scalar_tf <- hist_func(data = scalar_tf, title = "Method - TF")
hist_scalar_tf_idf <- hist_func(data = scalar_tf_idf, title = "Method - TF-IDF")

# Plot cosine         
grid.arrange(hist_cosine_tf, hist_cosine_tf_idf, nrow = 1, top = textGrob("Most Similar Reuters Documents - Cosine Similarity", gp = gpar(fontsize = 16, font = 3))) 

# Plot scalar           
grid.arrange(hist_scalar_tf, hist_scalar_tf_idf, nrow = 1, top = textGrob("Most Similar Reuters Documents - Scalar Dot Product", gp = gpar(fontsize = 16, font = 3))) 

# According to Cosine Similarity, the most similar docs are 110 and 362 for TF method and 379 and 478 for TF-IDF # method. 
# According to Scalar Dot Product, the most similar docs are 110 and 362 for TF method and 379 and 478 for TF-IDF # method. 