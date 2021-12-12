library(dplyr)
library(tidytext)
library(magrittr)
library(tm)
library(caret)

# Read rada
data.bbc <- as_tibble(read.table("https://jsienkiewicz.pl/TEXT/lab/data_bbc.csv", stringsAsFactors = F))

# Replace emotion's mark 1-pos or -1-neg to one 2-pos-neg
# Objective mark remains the same - 0 
data.bbc$emo <- ifelse(data.bbc$emo %in% c(1,-1), "2", "0")

# Grouping by emotions and counting
data.bbc %>%
   group_by(emo) %>% 
   summarise(n = n())

# Sort and cut 
data.bbc %<>% 
   arrange(desc(emo)) %>% 
   slice(-c(501:(n()-500)))

# Change coding to utf-8
data.bbc$text <- sapply(data.bbc$text, enc2utf8)
# Change emotions to factor
data.bbc$emo <- as.factor(data.bbc$emo)
# Sampling data
data.bbc <- data.bbc[sample(1:nrow(data.bbc)),]
# Add id to the individual term
data.bbc$doc_id <- 1:nrow(data.bbc)

# Create corpus
source <- DataframeSource(as.data.frame(data.bbc))
corpus <- VCorpus(source)
corpus %<>%
   tm_map(content_transformer(tolower)) %>%
   tm_map(removePunctuation) %>%
   tm_map(stripWhitespace) %>%
   tm_map(removeNumbers)



# This function prepare term document matrix to using in svm method
prepare_tdm <- function(corpus, stemming)
{
   # Create term-document matrix with stemming or not
   tdm <- DocumentTermMatrix(corpus, control=list(stemming=stemming))
   # Consider words which have occurred at least 5 times
   tdm <- tdm[, apply(tdm, 2, sum) > 4]
   # Change to matrix
   tdm <- as.matrix(tdm)
   # Remove docs where is only one word or which are empty
   ind <- apply(tdm, 1, sum) > 1
   tdm <- tdm[ind, ]
   # Assign emotions to variable
   class <- data.bbc$emo[ind]
   
   return(list(tdm = tdm, class = class))
}

# Linear SVM method
svm_method <- function(tdm, class)
{
   levels(class) <- c("obj", "sub")
   data <- cbind(as.data.frame(tdm), class.out = class)
   
   fit <- trainControl(method = "cv", number = 10)
   model <- train(class.out ~ ., data = data, method = "svmLinear", trControl = fit)
   
   ACC <- model$results$Accuracy
   ACCSD <- model$results$AccuracySD
   K <- model$results$Kappa
   KSD <- model$results$KappaSD
   
   return(c(ACC = round(ACC,4), ACCSD = round(ACCSD, 4), K = round(K, 4), 
            KSD = round(KSD, 4), row.names = NULL))
}

# Stemming - true or false
my_list <- prepare_tdm(corpus, TRUE)
tdm <- my_list$tdm
class <- my_list$class

# Let's do some statistics
n <- 10
svm_values <- replicate(n, svm_method(tdm = tdm, class = class), simplify = FALSE)
svm_values = as.data.frame(t(as.data.frame(svm_values)))
rownames(svm_values) <- NULL
round(colMeans(svm_values), 4)

# It would be good to check how looks accuracy when we change number of terms 
# from both classes. Here I choose 500 terms from both classes and I wanted 
# do some statistics to average results. But seems that the calculation take 
# some time.
# In any case, calling the svm method 10 times with stemming in term
# document matrix gave me the result of accuracy = 0.7693. In the case, when 
# stemming was turning off I got result of accuracy = 0.7746.
# In turn when I choose 50 terms from both classes, I got accuracy = 0.7686
# with stemming and accuracy = 0.7061 without it. 


