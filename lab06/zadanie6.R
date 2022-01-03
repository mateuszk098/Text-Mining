library(dplyr)
library(tidytext)
library(tm)
library(gutenbergr)
library(topicmodels)
library(ggplot2)

# 103 - Around the World in Eighty Days         (Verne)
# 164 - Twenty Thousand Leagues under the Sea   (Verne)
# 1268 - The Mysterious Island                  (Verne)
# 105 - Persuasion                              (Austen)
# 121 - Northanger Abbey                        (Austen)
# 141 - Mansfield Park                          (Austen)


removeSpecialChars <- function(x) gsub("[^0-9A-Za-z///' ]"," ",x)

g <- gutenberg_works(languages = "en")

# Jules Verne
id <- c(103, 164, 1268)

# Jane Austen
# id <- c(105, 121, 141)

# Jules Verne and Jane Austen
# id <- c(103, 164, 1268, 105, 121, 141)

books <- gutenberg_download(id)
# It must be doc_id (?)
colnames(books)[colnames(books) == "gutenberg_id"] <- "doc_id"

books <- books %>%
   group_by(doc_id) %>%
   summarise(text = paste(text, collapse = " "))

# Prepare corpus
corpus <- VCorpus(DataframeSource(as.data.frame(books)))
corpus <- corpus %>% tm_map(removeWords, stopwords("english")) %>% 
   tm_map(removePunctuation) %>%
   tm_map(content_transformer(tolower)) %>%
   tm_map(content_transformer(removeSpecialChars)) %>%
   tm_map(stripWhitespace) %>%
   tm_map(stemDocument)

doc <- DocumentTermMatrix(corpus)

# LDA 
doc_lda <- LDA(doc, k = 2, control = list(seed = 1234))
terms(doc_lda, 10)
topics(doc_lda)

ap_topics <- tidy(doc_lda, matrix = "beta")

ap_top_terms <- ap_topics %>%
   group_by(topic) %>%
   top_n(10, beta) %>%
   ungroup() %>%
   arrange(topic, -beta)

ap_top_terms %>%
   mutate(term = reorder(term, beta)) %>%
   ggplot(aes(term, beta, fill = factor(topic))) +
   geom_col(show.legend = FALSE) +
   facet_wrap(~ topic, scales = "free") +
   coord_flip() +
   theme_minimal() + 
   labs(x = "Term", y = "Beta") + 
   ggtitle("LDA Topic Model: 3 books by Jules Verne") + 
   theme(plot.title = element_text(hjust = 0.5)) + 
   scale_fill_manual(values=c("#FFAF42", "#56B4E9"))

# In my case I see that when I combine Verne and Austen books to 3 topics,
# then topic 3 will be assign to id-103, topic 1 to id-105, id-121, id-141
# and topic 2 to id-164 and id-1268. So we have two topics assigned to Verne 
# books and one topic assigned to Austen books.

# In case when we have only Austen's books and two topics. Topic 1 is assigned
# to id-121 and topic 2 to id-105 and id-141.

# In case when we have only Verne's books and two topics. Topic 1 is assigned 
# to id-164 and id-1268 and topic 2 to id-103.
