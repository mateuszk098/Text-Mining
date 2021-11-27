# Author: Mateusz Kowalczyk

library(gutenbergr)
library(dplyr)
library(tidytext)
library(magrittr)
library(ggplot2)
library(ggrepel)
library(grid)
library(gridExtra)

# Around the World in Eighty Days
# Five Weeks in a Balloon Or
# Twenty Thousand Leagues under the Sea
# From the Earth to the Moon
# The Mysterious Island
# A Journey to the Center of the Earth
# All Around the Moon
# Facing the Flag
# Robur the Conqueror
# The Voyages and Adventures of Captain Hatteras

# Download the books 
g_en <- gutenberg_works(languages = "en")
g_fr <- gutenberg_works(languages = "fr")

id_en <- c(103, 3526, 164, 83, 1268, 18857, 16457, 11556, 3808, 29413)
id_fr <- c(800, 4548, 5097, 799, 14287, 4791, 4717, 16826, 5126, 11927)

verne_en <- gutenberg_download(id_en)
verne_fr <- gutenberg_download(id_fr)

# We have to provide utf coding
verne_fr$text <- iconv(verne_fr$text, to = "UTF-8")

books_en <- g_en[g_en$gutenberg_id %in% id_en ,c("gutenberg_id","title")]
books_fr <- g_fr[g_fr$gutenberg_id %in% id_fr ,c("gutenberg_id","title")]

# We have to fit titles, I don't know how to do it automatically
books_en$title[1] <- "From the Earth to the Moon"
books_en$title[5] <- "Five Weeks in a Balloon Or"

books_fr$title[1] <- "From the Earth to the Moon"
books_fr$title[2] <- "Around the World in Eighty Days"
books_fr$title[3] <- "Five Weeks in a Balloon Or"
books_fr$title[4] <- "All Around the Moon"
books_fr$title[5] <- "A Journey to the Centre of the Earth"
books_fr$title[6] <- "Twenty Thousand Leagues under the Sea"
books_fr$title[7] <- "Robur the Conqueror"
books_fr$title[8] <- "The Voyages and Adventures of Captain Hatteras"
books_fr$title[9] <- "The Mysterious Island"
books_fr$title[10] <- "Facing the Flag"

# Function to calculate coeffs with std
coeffs <- function(x,y) {
   lm.xy <- lm(log10(y) ~ log10(x))
   z <- summary(lm.xy)$coefficients
   return(data.frame(a = z[1], asigma = z[3], b = z[2], bsigma = z[4]))
}

# Function to calculate books coeffs
book_treatment <- function(books, verne, random){
   
   if(random){
      verne <- verne[sample(1:nrow(verne)),]
      verne <- verne[order(verne$gutenberg_id),]
   }
   
   verne %<>% left_join(books) %>%
      mutate(gutenberg_id = NULL)
   
   verne_heaps <- verne %>% 
      unnest_tokens(word, text) %>% 
      group_by(title) %>% 
      mutate(M = row_number(), V = cumsum(!duplicated(word))) %>%
      summarise(coeffs(M, V))
   
   return(verne_heaps)
}

# Plot
plt <- function(data, tytul){
   coeff_plot <- ggplot(data, aes(x=beta_en, y=beta_fr)) + 
      geom_point(size = 2, color = "dodgerblue3") +
      geom_errorbar(aes(xmin = beta_en - beta_sigma_en, xmax = beta_en + beta_sigma_en), colour="black") +
      geom_errorbar(aes(ymin = beta_fr - beta_sigma_fr, ymax = beta_fr + beta_sigma_fr), colour="black") +
      xlim(0.45, 0.73) +
      ylim(0.55, 0.73) +
      theme(plot.title = element_text(hjust = 0.5)) + 
      ggtitle(tytul) +
      labs(x = "Beta Index for English", y = "Beta Index for French") +
      geom_label_repel(aes(label = title),color = "dodgerblue4")
   return(coeff_plot)
}


# Coefficients in individual books
verne_coeff_en <- book_treatment(books = books_en, verne = verne_en, random = FALSE)
verne_coeff_fr <- book_treatment(books = books_fr, verne = verne_fr, random = FALSE)
# Merge en and fr version 
total <- merge(verne_coeff_en, verne_coeff_fr, by = "title")
colnames(total) <- c("title", "a_en", "a_sigma_en", "beta_en", "beta_sigma_en", "a_fr", "a_sigma_fr","beta_fr", "beta_sigma_fr")
# p-value 
p_value <- cor.test(total$beta_en, total$beta_fr)$p.value
# Plot
plot_total <- plt(data = total, "English vs French Beta Index")
plot_total

# Coefficients in individual books with shuffle rows in books
verne_coeff_en_sh <- book_treatment(books = books_en, verne = verne_en, random = TRUE)
verne_coeff_fr_sh <- book_treatment(books = books_fr, verne = verne_fr, random = TRUE)
# Merge shuffle en and fr version 
total_sh <- merge(verne_coeff_en_sh, verne_coeff_fr_sh, by = "title")
colnames(total_sh) <- c("title", "a_en", "a_sigma_en", "beta_en", "beta_sigma_en", "a_fr", "a_sigma_fr","beta_fr", "beta_sigma_fr")
# p-value
p_value_sh <- cor.test(total_sh$beta_en, total_sh$beta_fr)$p.value
# Plot
plot_total_sh <- plt(data = total_sh, "English vs French Beta Index with shuffle")
plot_total_sh

# We see beta index for french is slightly higher
# Atmost all books are located close together 
# We have one distinguish book, it's The Mysterious Island,
# which has the least en and fr beta index
# p-value = 0,01
# p-value for shuffle rows = 0,004
