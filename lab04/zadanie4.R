# Author: Mateusz Kowalczyk

library(gutenbergr)
library(dplyr)
library(tidytext)
library(tidyr)
library(magrittr)
library(ggplot2)
library(igraph)

# Function top_n works a little weird, it returns different values than
# the values I want, for example I want top 20 tf_idf from both books but I get 
# for example 70 or 80 values, I think it's because tf_idf has a lot
# of the same values, so I will write the function for top_n myself
top_bigrams <- function(data, k){
   
   x <- split(data, data$title)
   
   sonety <- x$`Sonety Adama Mickiewicza`
   sklepy <- x$`Sklepy cynamonowe`
   
   sonety <- sonety[order(sonety$tf_idf, decreasing = TRUE),] %>%
      slice(1:k)
   sklepy <- sklepy[order(sklepy$tf_idf, decreasing = TRUE),] %>%
      slice(1:k)
   
   y <- rbind(sklepy, sonety) %>%
      mutate(bigram = reorder(bigram, tf_idf))
   
   return(y)
}

# Download what we need
g <- gutenberg_works(languages = "pl")
v <- gutenberg_download(c(8119, 27081))
books <- g[g$gutenberg_id %in% c(8119, 27081), c("gutenberg_id","title")]

# We should remove english phrases about gutenberg
v <- v %>%
   slice(c(75:5130, 5868:6988))

# Bigrams
bigrams <- v %>%
   left_join(books) %>%
   mutate(gutenberg_id = NULL) %>%
   unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
   drop_na()

# Bigram with separate words to columns and removed stop words
bigrams_sep <- bigrams %>%
   separate(bigram, c("word1", "word2"), sep = " ") %>%
   filter(!word1 %in% stop_words$word) %>%
   filter(!word2 %in% stop_words$word)

# Combine bigrams to one column but with removed stop words
bigrams <- bigrams_sep %>%
   unite(bigram, word1, word2, sep = " ")

# Calculate tf, idf and tf_idf indexes
bigrams_tf_idf <- bigrams %>%
   count(title, bigram) %>%
   bind_tf_idf(bigram, title, n) %>%
   arrange(desc(tf_idf))

# Selection only top values, k - how many bigrams from each document 
bigrams_tf_idf_top <- top_bigrams(data = bigrams_tf_idf, k = 20)

# Top bigrams plot
ggplot(bigrams_tf_idf_top) +
   geom_col(aes(bigram, tf_idf, fill = title), width = 0.75, alpha=0.75) +
   coord_flip() +
   facet_wrap(~title, nrow = 2, scales = "free") +
   theme_minimal() + 
   labs(x = "BIGRAM", y = "TF-IDF", fill = "Title") + 
   ggtitle("Comparison of TF-IDF bigram values") + 
   theme(plot.title = element_text(hjust = 0.5)) + 
   scale_fill_manual(values=c("#FFAF42", "#56B4E9"))

# We see "Sklepy cynamonowe" has several different levels of TF-IDF values but 
# what's interesting, bigram "wś ród" which has highest tf-idf index doesn't exist
# in polish language, it should be one word "wśród"
# "Sonety Adama Mickiewicza" have a lot of the same tf-idf values,
# this is probably because they are very short 

# Words network
# n > 4 seems to be good 
bigram_graph <- bigrams_sep %>% 
   count(word1, word2, sort = TRUE) %>%
   filter(n > 4) %>%
   graph_from_data_frame()

plot.graph <- function(g) {
   plot(g, vertex.size = 2, edge.arrow.size = 0.5, edge.color = "black")
}

plot.graph(bigram_graph)
