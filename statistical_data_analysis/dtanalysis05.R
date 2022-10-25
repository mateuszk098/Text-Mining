library(MASS)
library(rpart)
library(rpart.plot)
library(Hmisc)

# Podzial PU/PT
values <- c(train = 0.8, test = 0.2)
rand <- sample(cut(seq(nrow(iris)), nrow(iris)*cumsum(c(0, values)), labels = names(values)))
data <- split(iris, rand)

# Dzielimy na zbior treningowy i testowy
data.train <- data$train
data.test <- data$test

# Zmiana nazw na class
colnames(data.train)[colnames(data.train) == "Species"] <- "class"
colnames(data.test)[colnames(data.test) == "Species"] <- "class"

# Zamiana na typ wyliczeniowy
data.train$class <- factor(data.train$class)
data.test$class <- factor(data.test$class)










# Wyznaczanie bledu klasyfikatora
err.rate <- function(org.class, pred.class) {
  
  CM <- table(org.class, pred.class)
  
  return(1 - sum(diag(CM)) / sum(CM))
}

# Funkcja do przeprowadzanie procedury bagging metoda drzew
bagging.own <- function(data, N) {
  
  # Dane: losowanie z oryginalnej proby
  dane <- replicate(N, sample(1:nrow(data), rep = T))
  
  # tworzenie drzew
  trees <- lapply(1:N, function(i) rpart(class ~ ., data = data[dane[,i],], maxdepth = 1))
  
  tmp <- list(dane = dane)
  
  tmp$N <- N
  tmp$data <- data
  tmp$trees <- trees
  
  tmp1 <- bagging.own.pred(tmp, data)
  
  tmp$trees.class <- tmp1$trees.class
  tmp$votes <- tmp1$votes
  tmp$class <- tmp1$class
  
  tmp$err <- tmp1$err
  
  return(tmp)
}

# Funkcja do przeprowadzania przewidywania za pomoca baggingu metoda drzew
bagging.own.pred <- function(bag, data) {
  
  tmp <- list()
  
  trees.class <- sapply(1:bag$N, function(i) predict(bag$trees[[i]], newdata = data, type = "class"))
  votes <- t(sapply(1:nrow(trees.class), function(i) table(factor(trees.class[i,], levels = levels(data$class)))))
  class <- factor(levels(data$class)[apply(votes, 1, which.max)], levels = levels(data$class))
  
  tmp$trees.class <- trees.class
  tmp$votes <- votes
  tmp$class <- class
  
  tmp$err <- err.rate(data$class, tmp$class)
  
  return(tmp)
} 








# Funkcja do przeprowadzanie procedury bagging metoda LDA
bagging.own.lda <- function(data, N) {
  
  # Dane: losowanie z oryginalnej proby
  dane <- replicate(N, sample(1:nrow(data), rep = T))
  
  # metoda lda
  lda <- lapply(1:N, function(i) lda(class ~ ., data = data[dane[,i],], maxdepth = 1))
  tmp <- list(dane = dane)
  
  tmp$N <- N
  tmp$data <- data
  tmp$lda <- lda
  
  tmp1 <- bagging.own.pred.lda(tmp, data)
  
  tmp$lda.class <- tmp1$lda.class
  tmp$votes <- tmp1$votes
  tmp$class <- tmp1$class
  
  tmp$err <- tmp1$err
  
  return(tmp)
}


# Funkcja do przeprowadzania przewidywania za pomoca baggingu metoda LDA
bagging.own.pred.lda <- function(bag, data) {
  
  tmp <- list()
  lda.class <- sapply(1:bag$N, function(i) predict(bag$lda[[i]], newdata = data, type = "class")$class)
  votes <- t(sapply(1:nrow(lda.class), function(i) table(factor(lda.class[i,], levels = levels(data$class)))))
  class <- factor(levels(data$class)[apply(votes, 1, which.max)], levels = levels(data$class))
  
  tmp$lda.class <- lda.class
  tmp$votes <- votes
  tmp$class <- class
  
  tmp$err <- err.rate(data$class, tmp$class)
  
  return(tmp)
} 





# Liczba klasyfikatorow
vals <- c(1, 5, 10, 20, 50)

# Drzewa
# Wywolanie algorytmu bagging dla roznej liczby drzew dla PU (10 realizacji)
tab <- sapply(vals, function(v) replicate(10, bagging.own(data.train, v)$err))

# Wywolanie algorytmu bagging dla roznej liczby drzew dla PT (10 realizacji)
tab.new <- sapply(vals, function(v) replicate(10, bagging.own.pred(bagging.own(data.train, v), data.test)$err))

# Wartosci srednie i odchylenie PU
tab.m <- apply(tab, 2, mean)
tab.s <- apply(tab, 2, sd)

# Wartosci srednie i odchylenie PT
tab.new.m <- apply(tab.new, 2, mean)
tab.new.s <- apply(tab.new, 2, sd)






# LDA

# Wywolanie algorytmu bagging dla roznej liczby drzew dla PU (10 realizacji)
tab.lda <- sapply(vals, function(v) replicate(10, bagging.own.lda(data.train, v)$err))

# Wywolanie algorytmu bagging dla roznej liczby drzew dla PT (10 realizacji)
tab.new.lda <- sapply(vals, function(v) replicate(10, bagging.own.pred.lda(bagging.own.lda(data.train, v), data.test)$err))

# Wartosci srednie i odchylenie PU
tab.m.lda <- apply(tab.lda, 2, mean)
tab.s.lda <- apply(tab.lda, 2, sd)

# Wartosci srednie i odchylenie PT
tab.new.m.lda <- apply(tab.new.lda, 2, mean)
tab.new.s.lda <- apply(tab.new.lda, 2, sd)






# Wykres bagging drzewa
errbar(vals, tab.m, tab.m + tab.s, tab.m - tab.s, ylim = c(0, 0.5), xlab = "Liczba klasyfikatorow",
       ylab = "Blad klasyfikatora", main = "Metoda Bagging Drzewa", pch = 19, font = 2,
       font.lab = 4, font.main = 3, lwd = 1, cex = 1)
# Tytul wykresu nie dziala??

errbar(vals, tab.new.m, tab.new.m + tab.new.s, tab.new.m - tab.new.s, add = T, col = "red", errbar.col = "red")

# Wykres bagging LDA
errbar(vals, tab.m.lda, tab.m.lda + tab.s.lda, tab.m.lda - tab.s.lda, ylim = c(0, 0.08), xlab = "Liczba klasyfikatorow",
       ylab = "Blad klasyfikatora", main = "Metoda Bagging LDA", pch = 19, font = 2,
       font.lab = 4, font.main = 3, lwd = 1, cex = 1)

errbar(vals, tab.new.m.lda, tab.new.m.lda + tab.new.s.lda, tab.new.m.lda - tab.new.s.lda, add = T, col = "red", errbar.col = "red")


# WNIOSEK: Metoda LDA daj zdecydowanie mniejszy blad klasyfikatora w porownaniu z drzewem decyzyjnym 
