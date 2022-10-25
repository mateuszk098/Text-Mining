library(MASS)
library(rpart)
library(rpart.plot)
library(caret)


# Wczytanie danych z pliku i nazwanie kolumn
data <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data", header = FALSE)
colnames(data) <- c("class", "Alcohol", "Malic acid", "Ash", "Alcalinity of ash",
                    "Magnesium", "Total phenols", "Flavanoids", "Nonflavanoid phenols",
                    "Proanthocyanins", "Color intensity", "Hue", "OD280/OD315 of diluted wines", "Proline")

# Zamiana na typ wyliczeniowy
data$class <- factor(data$class)

# Tasowanie danych
data <- data[sample(1:nrow(data)),]

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
tree.func <- function(data, k){
  
  # Tworzenie drzewa dla k - zmiennych
  tree <- rpart(class ~ ., data[1:(k+1)], minsplit = 1, minbucket = 1, cp = 0)

  # Wybieramy optymalne drzewo
  CP <- choose.cp(tree)
  tree.opt <- prune(tree, cp = CP)
  
  # Roznica rozmiaru drzewa (liczby lisci)
  DIFF <- sum(tree$frame$var == "<leaf>") - sum(tree.opt$frame$var == "<leaf>")
  
  # Skutecznosc drzewa dla powtornego podstawienia
  ACC.PREDICT <- Accuracy(data, tree, k)
  
  # Skutecznosc drzewa przycietegodla powtornego podstawienia
  ACC.PREDICT.OPT <- Accuracy(data, tree.opt, k)
  
  # Kroswalidacja z pakietu caret
  fit <- trainControl(method = "cv", number = 10)
  
  # Kroswalidacja
  model.tree <- train(class ~ ., data = data[1:(k+1)], method = "lda", trControl = fit)
  ACC.CV <- model.tree$results$Accuracy
  
  par(mfrow=c(2, 1))
  # Rysowanie calego drzewa
  rpart.plot(tree, type = 1, extra = 1) 
   # Rysowanie optymalnego drzewa
  rpart.plot(tree.opt, type = 1, extra = 1)
  
  return(c(ACC.PREDICT = ACC.PREDICT, ACC.PREDICT.OPT = ACC.PREDICT.OPT,
           ACC.CV = ACC.CV, CP = CP, DIFF = DIFF, row.names = NULL))
}

# Liczba zmiennych 
k <- seq(2, ncol(data) - 1, length.out = ncol(data) - 2)

# Wywolanie tree.func dla kolejnej liczby zmiennych
acc.matrix <- sapply(k, function(k) tree.func(data, k))
colnames(acc.matrix) <- k
acc.matrix


# Wykres skutecznosci drzewa dla powtornego podstawienia dla roznej 
# liczby zmiennych i skutecznosci kroswalidacji
dev.off()
plot(k, acc.matrix[2,], col = "blue", type = 'o', lwd = 2, main = 'Predict and Cross-validation Accuracy',
     ylab = 'Accuray', xlab = "Number of variables - k", ylim = c(0.5, 1.0), 
     pch = 19, font = 2, font.lab = 4, font.main = 3, cex = 1)

points(k, acc.matrix[3,], col="red", type = "o", lwd = 2, pch = 19)
legend("right", legend=c("Predict", "Cross-validation"), col=c("blue", "red"),
       pch = c(19, 19), bty = "n", text.col = "black", )

# Wykres roznicy rozmiaru drzewa pelnego i optymalnego
plot(k, acc.matrix[5,], col = "blue", type = 'o', lwd = 2, main = 'Leaf difference',
     ylab = 'Leaf difference', xlab = "Number of variables - k", pch = 19, font = 2, font.lab = 4, font.main = 3, cex = 1)

