library(e1071)
library(MASS)

# Generowanie
draw.data.gauss <- function(S1, S2, m1, m2, n1, n2) {
  
  X1 <- mvrnorm(n1, m1, S1)
  X2 <- mvrnorm(n2, m2, S2)
  
  X1 <- data.frame(X1); colnames(X1) <- c("x", "y")
  X2 <- data.frame(X2); colnames(X2) <- c("x", "y")
  
  X1$class <- 1; X2$class <- 2
  
  data <- rbind(X1, X2)
  data <- data[sample(nrow(data)),]
  data$class <- factor(data$class)
  
  return(data)
}

# Parametry danych z rozkladu Gaussa
S1 <- matrix(c(4, 2, 2, 4), 2, 2)
S2 <- matrix(c(4, 2, 2, 2), 2, 2)

m1 <- c(-1, -1)
m2 <- c(2, 2)

n1 <- 30
n2 <- 20

# Ustawienie ziarna dla losowania danych
set.seed(128)

# Generowanie obserwacji
PU <- draw.data.gauss(S1, S2, m1, m2, n1, n2)
PT <- draw.data.gauss(S1, S2, m1, m2, n1, n2)








# Sprawnosc klasyfikatora
ACC <- function(org.class, pred.class) {
  
  CM <- table(org.class, pred.class)
  
  return(sum(diag(CM)) / sum(CM))
}

# Sprawnosc metody SVM lub LDA 
method <- function(data.learn, data.test, C, method = "svm"){
  
  tmp.org <- data.test$class
  
  # Metoda SVM na PU
  if(method == "svm") tmp <- svm(class ~ x + y, type = "C-classification", data = data.learn,
                                    cost = C, scale = F, kernel = "linear")
  
  # Trenowanie LDA na PU
  if(method == "lda") tmp <- lda(class ~ x + y, data = data.learn)
  
  # Predykcja SVM na PT
  tmp.predict <- predict(tmp, data.test)
  
  if(method == "svm") ACC(tmp.org, tmp.predict)
  else ACC(tmp.org, tmp.predict$class)
}

# Parametry c do metody SVM
C <- exp(seq(log(0.001), log(10), length.out = 20))

# Skutecznosc metody SVM uczonej i testowanej na PU
method.svm.predict <- lapply(C, function(i) method(PU, PU, i, "svm"))

# Skutecznosc metody SVM uczonej na PU i testowanej na PT
method.svm.test <- lapply(C, function(i) method(PU, PT, i, "svm"))

# Skutecznosc klasyfikatora LDA uczonego i testowanego na PU
method.lda.predict <- method(PU, PU, NULL, "lda")

# Skutecznosc klasyfikatora LDA uczonego na PU i testowanego na PT
method.lda.test <- method(PU, PT, NULL, "lda")


# Wykres metody SVM dla roznych parametrow c
plot(C, method.svm.predict, col = "blue", type = 'p', main = 'Linear SVM Method Accuracy', ylab = 'Accuray',
     xlab = "C parameter", log = "x", ylim = c(0.5, 1.0), pch = 19, font = 2, font.lab = 4, font.main = 3, lwd = 1, cex = 1)
points(C, method.svm.test, col="red", pch = 19)
legend("right", legend=c("SVM (PU-PU)", "SVM (PU-PT)"), col=c("blue", "red"), pch = c(19, 19), bty = "n", text.col = "black", )


