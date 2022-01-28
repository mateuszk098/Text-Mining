library(dplyr)
library(magrittr)
library(quanteda)
library(quanteda.textstats)
library(caret)

# This file weighs a lot so you have to download it individually
# https://www.kaggle.com/c/fake-news/data?select=train.csv

# Read data
df <- read.csv("train.csv", encoding="UTF-8")
# Data frame shuffle and cut-off
df <- df[sample(1:nrow(df)),] %>%
   slice(c(1:2000))
# Change label name to class name
colnames(df)[colnames(df) == "label"] <- "class"
# Use class true or false (0 is true and 1 is false)
df$class <- ifelse(df$class %in% 0, "true", "false")
# Change class type to factor
df$class <- factor(df$class)

# Create corpus
df.corp <- corpus(df)
# Create document-feature matrix
df.mat <- df.corp %>% 
   tokens(remove_punct = T) %>% 
   dfm %>% dfm_remove(stopwords("english")) %>% 
   dfm_wordstem()

# Evaluate simple statistics in docs
df.s <- df.corp %>% textstat_summary()
# Join class column
df.s <- df.s %>% mutate(class = df$class)
# Move class column to first position
df.s <- df.s[, c(ncol(df.s), 1:ncol(df.s)-1)]
# Evaluate TTR and C indexes
df.lex <- df.mat %>% textstat_lexdiv(measure = c("TTR", "C"))
# Evaluate FOG index
df.read <- df.corp %>% textstat_readability(measure = "FOG")

# Join 
data <- df.s %>%
   mutate(TTR = df.lex$TTR) %>%
   mutate(C = df.lex$C) %>%
   mutate(FOG = df.read$FOG) %>%
   # Remove unnecessary columns
   subset(select = -c(document, numbers, symbols, urls, tags, emojis)) %>%
   na.omit()

# Now we have appropriate data frame (data) to use in classifiers
# Classifiers
fit <- trainControl(method = "cv", number = 10)
model_svm <- train(class ~ ., data = data, method = "svmLinear", trControl = fit)
model_lda <- train(class ~ ., data = data, method = "lda", trControl = fit)

# Results
results <- data.frame(SVM = c(model_svm$results$Accuracy, model_svm$results$AccuracySD,
                               model_svm$results$Kappa, model_svm$results$KappaSD),
                      LDA = c(model_lda$results$Accuracy, model_lda$results$AccuracySD,
                              model_lda$results$Kappa, model_lda$results$KappaSD))
rownames(results) <- c("Accuracy", "Accuracy SD", "Kappa", "Kappa SD")
print(results, digits = 3)
