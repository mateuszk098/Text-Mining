# Author: Mateusz Kowalczyk

# Wykresy 
library(ggplot2)
library(grid)
library(gridExtra)
library(wordcloud)

# Warunkowanie zmiennych
library(dplyr)

# Analiza tekstu
library(tidytext)
library(gutenbergr)




# Funkcja do pobierania ksiazek
book_download <- function(autor, tytul){
     
     # Tibble
     book_tibble <- gutenberg_works(languages = "en") %>% 
          filter(author == autor, title == tytul)
     
     # ID
     gutenberg_id <- book_tibble$gutenberg_id
     
     # Download
     book <- gutenberg_download(gutenberg_id)
     
     return(book)
}

# Funkcja do analizy tekstu
book_analyze <- function(book, how_many, stopwords){
     
     # Zamieniamy strukture ksiazki w strukture tabeli
     words <- unnest_tokens(book, word, text)
     
     # Usuwamy slowa funkcyjne
     if (stopwords){
        words <- anti_join(words, stop_words)
     }
     
     # Zliczamy n najczesciej wystepujacych slow wliczajac funkcyjne lub nie
     words %>%
        count(word, sort = TRUE) %>%
        mutate(word = reorder(word, n)) %>%
        top_n(how_many) %>%
     
     return(words)
}

# Wykres
words_hist <- function(words_true, words_false, title){
        
     hist_true <- ggplot() + 
        geom_col(aes(x = words_true$n, y = words_true$word), fill = "deepskyblue", colour = "black") + 
        ggtitle("Most common true words") +
        theme_bw() + 
        theme(plot.title = element_text(hjust=0.5)) + 
        labs(x = "Counts", y = "Word")
     
     hist_false <- ggplot() + 
        geom_col(aes(x = words_false$n, y = words_false$word), fill = "darkorange", colour = "black") + 
        ggtitle("Most common false words") + 
        theme_bw() + 
        theme(plot.title = element_text(hjust = 0.5)) + 
        labs(x = "Counts", y = "Word")
      
     grid.arrange(hist_true, hist_false, nrow = 1, top = textGrob(title, gp = gpar(fontsize = 16, font = 3)))
}

# Pobieramy ksiazki
book_verne <- book_download("Verne, Jules", "Twenty Thousand Leagues under the Sea")
book_joyce <- book_download("Joyce, James", "Ulysses")

# Najczesciej wystepujace slowa w ksiazkach 
# 1 argument - ksiazka, 2 argument - ile slow na histogramie, 3 - wliczamy slowa funkcyjne czy nie
words_verne_true <- book_analyze(book_verne, 10, TRUE)
words_verne_false <- book_analyze(book_verne, 10, FALSE)
words_joyce_true <- book_analyze(book_joyce, 10, TRUE)
words_joyce_false <- book_analyze(book_joyce, 10, FALSE)

# Histogream "Twenty Thousand Leagues under the Sea"
words_hist(words_verne_true, words_verne_false, "Twenty Thousand Leagues under the Sea")

# Histogram "Ulysses"
words_hist(words_joyce_true, words_joyce_false, "Ulysses")

# Jeszcze ciekawy wykres
words_verne_true <- book_analyze(book_verne, 100, TRUE)
wordcloud(words_verne_true$word, words_verne_true$n)

# Wnioski: Mozemy wnioskowac ze praktycznie w wiekszosci ksiazek, wykres niedofiltrowanych slow bedzie taki sam,
# dopiero po odrzuceniu slow funkcyjnych bedziemy miec faktyczny obraz danej ksiazki.
