library(e1071)
library(klaR)
library(MASS)
library(Hmisc)




# Punkt 1 i 2
# Wczytanie danych z pliku i nazwanie kolumn
data <- read.table("wine.data", sep = ",", col.names=c("class", "Alcohol", "Malic acid", "Ash", "Alcalinity of ash",
                                           "Magnesium", "Total phenols", "Flavanoids", "Nonflavanoid phenols",
                                           "Proanthocyanins", "Color intensity", "Hue", "OD280/OD315 of diluted wines",
                                           "Proline"))




# Punkt 3 i 4
# Sprawdzanie skutecznosci klasyfikatorow
# Funkcja do wyznaczania macierzy pomylek, skutecznosci klasyfikatora i parametrow
CM.large <- function(org.class, pred.class) {
  
  CM <- table(org.class, pred.class)
  
  # Skutecznosc klasyfikatora (Accuracy)
  ACC <- sum(diag(CM)) / sum(CM)
  
  # Nie wiem czy mozna to uznac za jakiekolwiek parametry ale tak zostawilem
  # Liczba bledow (Mistake number)
  MN <- sum(CM) - sum(diag(CM))
  
  # Liczba poprawnie sklasyfikowanych (Correct number)
  CN <- sum(diag(CM))
  
  # Stosunek bledow do poprawnych (mistake/correct)
  MC <- MN / CN
   
  return(c(ACC = round(ACC,4), MN = MN, CN = CN, MC = round(MC,4) ,row.names = NULL))
} 


# Ograniczenie do pierwszych k skladowych 
k <- 3

# Trenowanie klasyfikatorow na zbiorze
class.lda <- lda(class ~ ., data[1:k])
class.qda <- qda(class ~ ., data[1:k])
class.nb <- naiveBayes(class ~ ., data[1:k])

# Powtorne podstawienie
data.lda.old <- predict(class.lda, data[1:k])
data.qda.old <- predict(class.qda, data[1:k])
data.nb.old <- predict(class.nb, data[1:k])

# Glowne wartosci z macierzy pomylek dla powtornego podstawienia
res.old <- CM.large(data$class, data.lda.old$class)
res.old <- rbind(res.old, CM.large(data$class, data.qda.old$class))
res.old <- rbind(res.old, CM.large(data$class, data.nb.old))

rownames(res.old) <- c("LDA", "QDA", "NB")
res.old

# W przypadku 2 pierwszych skladowych ACC = 0.8090 (LDA), ACC = 0.8146 (QDA), ACC = 0.8090 (NB)   
# W przypadku 5 pierwszych skladowych ACC = 0.8764 (LDA), ACC = 0.8876 (QDA), ACC = 0.8539 (NB)  
# W przypadku 10 pierwszych skladowych ACC = 0.9888 (LDA), ACC = 0.9944 (QDA), ACC = 0.9607 (NB)  
# W kazdym z tych przypadkow mamy najwieksza skutecznosc dla QDA, dopiero na calym zbiorze danych LDA zwycieza bo ma ACC = 1.0000




# Punkt 5
# https://stackoverflow.com/questions/45777247/how-to-split-a-data-frame-into-training-validation-and-test-sets-dependent-on
# Podzial zbioru na PU/PW/PT -> 50/25/25
coeff = c(PU = 0.5, PW = 0.25, PT = 0.25)
rand = sample(cut(seq(nrow(data)), nrow(data)*cumsum(c(0,coeff)), labels = names(coeff)))
values = split(data, rand)

# Testujemy klasyfikatory na l ppierwszych zmiennych
l <- 3

# Trenowanie klasyfikatorow na PU na l pierwszych zmiennych
class.lda <- lda(class ~ ., values$PU[1:l])
class.qda <- qda(class ~ ., values$PU[1:l])
class.nb <- naiveBayes(class ~ ., values$PU[1:l])

# Predykcja na zbiorze walidacyjnym PW
data.lda.pw <- predict(class.lda, values$PW[1:l])
data.qda.pw <- predict(class.qda, values$PW[1:l])
data.nb.pw <- predict(class.nb, values$PW[1:l])

# Skutecznosc klasyfikatorow
res.pw <- CM.large(values$PW$class, data.lda.pw$class)
res.pw <- rbind(res.pw, CM.large(values$PW$class, data.qda.pw$class))
res.pw <- rbind(res.pw, CM.large(values$PW$class, data.nb.pw))
rownames(res.pw) <- c("LDA", "QDA", "NB")
res.pw

# Wyglada na to, ze najczeciej najwieksza skutecznosc ma qda
# Predykcja na zbiorze testowym PT 
data.qda.pt <- predict(class.qda, values$PT[1:l])

# Skutecznosc klasyfikatora qda na zbiorze testowym
res.pt <- CM.large(values$PT$class, data.qda.pt$class)
res.pt 





# Punkt 6
# Kroswalidacja na 2 pierwszych skladowych dla LDA
# Funkcja do wyznaczania liczby pomylek
CV <- function(data, K) {
  
  N <- nrow(data)
  
  # Dane przetasowane
  data.rnd <- data[sample(1:N),]
  
  # Tworzenie K pseudoprob
  sets <- sapply(1:K, function(i) ((i-1) * (N/K) + 1):(i * (N/K)))
  
  # Przypadek K = 1
  if(is.vector(sets)) sets <- t(as.matrix(sets))
  
  # Dla kazdej pseudoproby wyznaczamy liczba pomylek
  res <- t(sapply(1:K, function(k) CV.main(data.rnd[-c(sets[,k]),], data.rnd[sets[,k],])))
  
  res
}

# Glowna funkcja odpowiedzialna za CV
# przyjmuje PU (jedna z pseudoprob) oraz PT
CV.main <- function(learn, test) {
  
  learn.classifier <- lda(class ~ ., data = learn)
  test.pred <- predict(learn.classifier, newdata = test)
  
  # Macierz pomylek
  CM <- table(test$class, test.pred$class)
  
  # Liczba bledow
  sum(CM) - sum(diag(CM))
}

N <- nrow(data)

# Dzielniki N
div <- which(!(N %% 1:N))

# Wykonanie wielokrotnej CV dla ronych dzielnikow
mat <- sapply(div[-1], function(d) replicate(10, sum(CV(data[1:3], d)) / N))

# Wyznaczanie statystyk
cv.res <- as.data.frame(t(apply(mat, 2, function(x) c(mean(x), sd(x)))))
colnames(cv.res) <- c("mean", "sd")
cv.res$K <- div[-1]
cv.res

with(cv.res, errbar(K, mean, mean + sd, mean - sd)) 





# Powtorne podstawienie
res.old
# Testowanie na zbiorze walidacyjnym
res.pw
# Testowanie na zbiorze testowym
res.pt
# Kroswalidacja lda
cv.res
