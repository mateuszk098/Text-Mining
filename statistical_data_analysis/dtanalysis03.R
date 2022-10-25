library(MASS)
library(class)



# Generowanie danych z wielowymiarowego rozkladu Gaussa
draw.data.gauss <- function(S1, S2, m1, m2, n1, n2){
  
  X1 <- mvrnorm(n1, m1, S1)
  X2 <- mvrnorm(n2, m2, S2)
  
  X1 <- data.frame(X1); colnames(X1) <- c("x", "y")
  X2 <- data.frame(X2); colnames(X2) <- c("x", "y")
  
  X1$class <- 1; X2$class <- 2
  
  data <- rbind(X1, X2); data$class <- factor(data$class)
  
  return(data)
}

# Parametry rozkladow
S1 <- matrix(c(4, 2, 2, 4), 2, 2)
S2 <- matrix(c(4, 2, 2, 2), 2, 2)

m1 <- c(-1, 1)
m2 <- c(2, 2)

n1 <- 30
n2 <- 20

# Generowanie zbioru treningowego
data.train <- draw.data.gauss(S1, S2, m1, m2, n1, n2)




# Funkcja do wyznaczania podstawowych parametrow 
CM.large <- function(data.train, data.test, k) {
  
  # Funkcja knn
  class.knn <- knn(data.train[,1:2], data.test[,1:2], data.train$class, k)
  
  # Macierz pomy�ek
  CM <- table(class.knn, data.test$class)
  
  # Skuteczno�� klasyfikatora
  ACC <- sum(diag(CM)) / sum(CM)
  
  # Warto�ci true positive i true negative
  # zak�adamy, �e klasa "2" jest "pozytywna"
  TP <- CM[2,2]
  TN <- CM[1,1]
  
  return(c(ACC = round(ACC,4), TP = TP, TN = TN, row.names = NULL))
}

# Powtarzamy CM.large k razy
k <- 1:21

# Wywolujemy CM.large k razy
VAL.train <- sapply(k, function(k) CM.large(data.train, data.train, k))

# Wykres skutecznosci klasyfikatora knn w zaleznosci od liczby sasiadow
plot(VAL.train[1,], xlab = "knn", ylab = "Accuracy", main = "K-NN Method Accuracy", col = "blue",
     pch = 19, font = 2, font.lab = 4, font.main = 3, type = "o", lwd = 1, cex = 1)

# Wykres TP klasyfikatora knn w zaleznosci od liczby sasiadow
plot(VAL.train[2,], xlab = "knn", ylab = "True Positive", main = "K-NN Method True Positive", col = "blue",
     pch = 19, font = 2, font.lab = 4, font.main = 3, type = "o", lwd = 1, cex = 1)

# Wykres TN klasyfikatora knn w zaleznosci od liczby sasiadow
plot(VAL.train[3,], xlab = "knn", ylab = "True Negative", main = "K-NN Method True Negative", col = "blue",
     pch = 19, font = 2, font.lab = 4, font.main = 3, type = "o", lwd = 1, cex = 1)






n1 <- 10
n2 <- 5

# Generowanie zbioru testowego
data.test <- draw.data.gauss(S1, S2, m1, m2, n1, n2)

# Wywolujemy CM.large k razy
VAL.test <- sapply(k, function(k) CM.large(data.train, data.test, k))

# Wykres skutecznosci klasyfikatora knn w zaleznosci od liczby sasiadow
plot(VAL.test[1,], xlab = "knn", ylab = "Accuracy", main = "K-NN Method Accuracy", col = "blue",
     pch = 19, font = 2, font.lab = 4, font.main = 3, type = "o", lwd = 1, cex = 1)

# Wykres TP klasyfikatora knn w zaleznosci od liczby sasiadow
plot(VAL.test[2,], xlab = "knn", ylab = "True Positive", main = "K-NN Method True Positive", col = "blue",
     pch = 19, font = 2, font.lab = 4, font.main = 3, type = "o", lwd = 1, cex = 1)

# Wykres TN klasyfikatora knn w zaleznosci od liczby sasiadow
plot(VAL.test[3,], xlab = "knn", ylab = "True Negative", main = "K-NN Method True Negative", col = "blue",
     pch = 19, font = 2, font.lab = 4, font.main = 3, type = "o", lwd = 1, cex = 1)

# Wniosek: Skutecznosc klasyfikatora knn jest zdecydowanie wieksza na zbiorze treningowym ale to 
# nic dziwnego bo jest zarowno trenowany jak i testowany na tym zbiorze, w przypadku zbioru testowego
# skutecznosc maleje ale tez nie zawsze






