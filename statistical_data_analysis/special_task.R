# MATEUSZ KOWALCZYK 

library(MASS)
library(rpart)
library(rpart.plot)
library(pROC)
library(caret)



# DANE

# Czytamy dane
data <- read.csv("http://www.if.pw.edu.pl/~julas/LSED/file_pogoda.txt", header = TRUE, sep = " ")

# Stosujemy klasy 1 i 2 
data$percip <- ifelse(data$percip %in% 0, "1", "2")

# Zmmiana nazwy na klase
colnames(data)[colnames(data) == "percip"] <- "class"

# Przeniesienie kolumny z klasami na 1 pozycje
data <- data[, c(ncol(data), 1:ncol(data)-1)]

# Zamiana na typ wyliczeniowy
data$class <- factor(data$class)

# Tasowanie danych (tak dla zasady)
data <- data[sample(1:nrow(data)),]




# LDA

# Funckcja do skutecznosci, tn, tp, tpr, fpr
CM.large <- function(org.class, pred.class) {
  
  CM <- table(org.class, pred.class)
  
  # Skutecznosc klasyfikatora
  ACC <- sum(diag(CM)) / sum(CM)
  
  # Wartosci true positive i true negative
  # zakladamy, ze klasa "2" jest "pozytywna"
  TP <- CM[2,2]
  TN <- CM[1,1]
  
  # TPR i FPR
  sums <- apply(CM, 1, sum)
  
  TPR <- TP / sums[2]
  FPR <- 1 - TN / sums[1]
  
  return(c(ACC = round(ACC,4), TPR = round(TPR, 4), FPR = round(FPR, 4), row.names = NULL))
}


# Funkcja do metody lda
method.lda <- function(data.learn, data.test, k){
  
  class.lda <- lda(class ~ ., data.learn[1:(k+1)])
  class.predict <- predict(class.lda, data.test[1:(k+1)])
  
  result <- CM.large(data.learn$class, class.predict$class)
}

# Liczba zmiennych do analizy w ramce danych 
k <- seq(2, ncol(data) - 1, length.out = ncol(data) - 2)

# Wywolanie metody lda wraz z liczeniem glownych wartosci z macierzy pomylek dla kolejnej liczby zmiennych
lda.values <- sapply(k, function(k) method.lda(data, data, k))
colnames(lda.values) <- k

# Krzywa ROC (nie wiem czy dobrze rozumiem ten podpunkt :/)
var <- ncol(data)
fit <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
model <- train(class ~ ., data = data[1:var], method = "lda", trControl = fit)
model.pred <- predict(model, data[1:var], type = "prob")

png("roc.png")
plot.roc(data$class, model.pred$'1', percent = T, ci = TRUE, of="thresholds",
         thresholds="best", print.thres="best", print.auc = TRUE)
dev.off()





# DRZEWKA

# Wyznaczanie optymalnej wartosci CP zgodnie z regula 1-SE
choose.cp <- function(tree) {
  
  n <- which.min(tree$cptable[, 4])
  n.min <- min(which(tree$cptable[, 4] < tree$cptable[n, 4] + tree$cptable[n, 5]))
  
  return(tree$cptable[n.min, 1])
}

# Skutecznosc drzewa - powtorne podstawienie
Accuracy <- function(data, tree, k){
  
  # Skutecznosc drzewa - powtorne podstawienie
  CM <- table(data$class, predict(tree, data[1:(k+1)], type = "class"))
  ACC <- sum(diag(CM)) / sum(CM)
  return(ACC)
}

# Glowna funkcja drzewa klasyfikujacego, k - liczba zmiennych
method.tree <- function(data, k){
  
  # Tworzenie drzewa dla k - zmiennych
  tree <- rpart(class ~ ., data[1:(k+1)], minsplit = 1, minbucket = 1, cp = 0)
  
  # Wybieramy optymalne drzewo
  CP <- choose.cp(tree)
  tree.opt <- prune(tree, cp = CP)
  
  # Roznica rozmiaru drzewa (liczby lisci)
  DIFF <- sum(tree$frame$var == "<leaf>") - sum(tree.opt$frame$var == "<leaf>")
  
  # Skutecznosc drzewa przycietego dla powtornego podstawienia
  ACC.PREDICT.OPT <- Accuracy(data, tree.opt, k)
  
  png("tree.png")
  par(mfrow=c(2, 1))
  # Rysowanie calego drzewa
  rpart.plot(tree, type = 1, extra = 1) 
  # Rysowanie optymalnego drzewa
  rpart.plot(tree.opt, type = 1, extra = 1)
  dev.off()
  
  return(c(ACC.PREDICT.OPT = round(ACC.PREDICT.OPT, 4), CP = round(CP, 4), DIFF = round(DIFF, 4), row.names = NULL))
}

# Wywolujemy method.tree dla wszystkich zmiennych
tree.values <- method.tree(data, ncol(data)-1)





# PODSUMOWANIE

# Wyniki lda i drzewa

lda.values
tree.values

# Widzimy, ze dla wszystkich zmiennych minimalnie lepsza okazuje sie metoda lda (ale nie zawsze)
# Mozemy jeszcze sprawdzic jak to sie prezentuje dla kolejnej liczby zmiennnych

# Wywolanie tree.func dla kolejnej liczby zmiennych
tree.values.full <- sapply(k, function(k) method.tree(data, k))
colnames(tree.values.full) <- k

lda.values
tree.values.full

# Widzimy, ze jednak w ogolnosci lepsza skutecznosc daje metoda lda (nie zawsze)