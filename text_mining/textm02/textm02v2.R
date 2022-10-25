# Author: Mateusz Kowalczyk

# Text mining
library(tidytext)
library(tm)

# Similarity
library(proxy)

# Variables conditioning
library(tidyr)
library(dplyr)
library(otuSummary)

# Plots
library(ggplot2)
library(grid)
library(gridExtra)

# Data treatment for scalar
data_scalar <- function(method){
   
   data(acq)
   
   tdm <- TermDocumentMatrix(acq, control = list(removePunctuation = TRUE, removeNumbers = TRUE, 
                                                 stopwords = TRUE, stemming = TRUE, weighting = method))
   tdm <- as.matrix(tdm)
   
   scalar_dot <- function(x, k) x[,k] %*% x
   
   k <- seq(1, length(acq))
   data_scalar <- as.matrix(sapply(k, function(k) scalar_dot(tdm, k)))
   rownames(data_scalar) <- labels(acq)
   colnames(data_scalar) <- labels(acq)

   # This function will convert lower triangular distance matrix into a 3-column, long-format data frame.
   data_scalar <- matrixConvert(data_scalar, colname = c("ID1", "ID2", "value"))
   
   return(data_scalar)
}

# Data treatment for cosine
data_cosine <- function(method){
   
   data(acq)
   
   tdm <- TermDocumentMatrix(acq, control = list(removePunctuation = TRUE, removeNumbers = TRUE, 
                                                 stopwords = TRUE, stemming = TRUE, weighting = method))
   tdm <- as.matrix(tdm)
   
   # Cosine similarity beetwen every document pair
   cosine_simil <- as.matrix(simil(t(tdm), method = "cosine"))

   # This function will convert lower triangular distance matrix into a 3-column, long-format data frame.
   cosine_simil <- matrixConvert(cosine_simil, colname = c("ID1", "ID2", "value"))

   return(cosine_simil)
}

# Histogram 
hist_func <- function(data, title){
   
   hist <- ggplot(data, aes(x = value)) + 
      geom_density(color="darkblue", size = 1, adjust=2) +
      ggtitle(title) +
      theme_minimal() + 
      theme(plot.title = element_text(hjust = 0.5)) + 
      labs(x = "Similarity", y = "Probability Density") +
      scale_colour_brewer(palette = "Blues")
   
      return(hist)
}

# Cosine similarity
cosine_tf <- data_cosine(method = weightTf)
cosine_tf_idf <- data_cosine(method = weightTfIdf)

# Scalar dot product 
scalar_tf <- data_scalar(method = weightTf)
scalar_tf_idf <- data_scalar(method = weightTfIdf)

# Cosine similarity Histogram 
hist_cosine_tf <- hist_func(cosine_tf, "Cosine Method - TF")
hist_cosine_tf_idf <- hist_func(cosine_tf_idf, "Cosine Method - TF-IDF")

# Scalar dot product Histogram 
hist_scalar_tf <- hist_func(scalar_tf, "Scalar Method - TF")
hist_scalar_tf_idf <- hist_func(scalar_tf_idf, "Scalar Method - TF-IDF")

# Plot
grid.arrange(hist_cosine_tf, hist_cosine_tf_idf, hist_scalar_tf, hist_scalar_tf_idf, nrow = 2, ncol = 2, top = textGrob("Cosine and Scalar Similarity - Density", gp = gpar(fontsize = 16, font = 3))) 

# Ciekawie wyglada jedynie metoda tf dla podobienstwa cosinusowego,
# w pozostalych metodach mamy bardzo male wyniki podobienstwa dokumentow
# W metodzie iloczynu skalarnego tf trzeba zwrocic uwage ze podobienstwo jest
# w stylu ilosci slow, dlatego nie jest to stricte podobienstwo jak bysmy
# mysleli ze jest, tzn nie zawiera sie w przedziale [0;1].
# Trzeba tez zwrocic uwage na wartosci osi-y rozkladow. To na pierwszy rzut oka troche
# dziwne ze gestosc prawdopodobienstwa jest powyzej wartosci 1 ale zauwazmy,
# ze os-x opisuje podobienstwo z zakresu [0;1] wiec pole pod taka krzywa i tak 
# wyniesie 1.

# Max values
cosine_tf[which.max(cosine_tf$value),]
cosine_tf_idf[which.max(cosine_tf_idf$value),]
scalar_tf[which.max(scalar_tf$value),]
scalar_tf_idf[which.max(scalar_tf_idf$value),]

# Widzimy, ze w metodzie tf cosinusa najblizej sa dokumenty 110-362
# W metodzie tf-idf cosinusa dokumenty 379-478
# W metodzie tf iloczynu dokumenty 110-362
# W metodzie tf-idf iloczynu dokumenty 379-478