library(MASS)
library(mvtnorm)
library(klaR)
library(e1071)

# Część z nanoszeniem klas na wykres

# Macierz kowariancji dla klas
S <- matrix(c(4,0,0,4),2,2)

# Wartości oczekiwane
mt1 <- c(-3,-1)
mt2 <- c(2,2)

# Liczba punktów w klasach
n1 <- 40
n2 <- 30
n <- n1 + n2

# Generowanie rozkładów
X1 <- mvrnorm(n1, mt1, S)
X2 <- mvrnorm(n2, mt2, S)

# Zamiana na ramki danych
X1 <- data.frame(X1); colnames(X1) <- c("x", "y")
X2 <- data.frame(X2); colnames(X2) <- c("x", "y")

# Nanoszenie punktów na wykres
plot(X1$x, X1$y, ylim = c(-8,8), xlim = c(-8,8), xlab = "X", ylab = "Y", pch = 19,
     col = "blue", font = 2)
abline(v = 0, h = 0, col = "gray")
points(X2$x, X2$y, pch = 19, col = "orange")



# Część z metodą naiwnego Bayesa

# Przypisanie klas
X1$class <- 1
X2$class <- 2

# "Sklejenie" danych do jednej ramki
# oraz przekształcenie typu zmiennej przechowującej klasy
data <- rbind(X1, X2)
data$class <- factor(data$class) 

# Klasyfikator naiwnego Bayesa
data.nb <- naiveBayes(class ~ x + y, data)

# Przewidywanie klas za pomocą klasyfikatora naiwnego Bayesa
# domyślnie zwraca klasy
data.nb.pred <- predict(data.nb, data)

# opcja method = "raw" daje prawdopodobieństwa a posteriori
data.nb.pred <- predict(data.nb, data, type = "raw")

# Rysowanie prostej rozdzielającej, punktów etc
with(data, drawparti(class, x, y, method = "naiveBayes", xlab = "X", ylab = "Y", font = 2)) 



# Część z ręcznym szukaniem prawdopodobieństw

# Definiowanie zakresów współprzędnych
xp <- seq(-10, 10, 0.1)
yp <- seq(-10, 10, 0.1)

# Rozpięcie siatki na współprzędnych
gr <- expand.grid(x = xp, y = yp)

# Estymowane wartości oczekiwane
me1 <- apply(X1[,1:2], 2, mean)
me2 <- apply(X2[,1:2], 2, mean)

# Estymowane macierze kowariancji
Se1 <- cov(X1[,1:2])
Se2 <- cov(X2[,1:2])

# Prawdopodbieństwa a priori
pi1 <- n1 / (n1 + n2)
pi2 <- n2 / (n1 + n2)

# Funkcja do wyznaczania prawdopodobieństw a posteriori w metodzie naiwnego Bayesa
f.nb <- function(X, m1, m2, S1, S2, pi1, pi2) {
  return(1 / (1 + (pi2 * dnorm(X$x, m2[1], sqrt(Se2[1,1])) * dnorm(X$y, m2[2], sqrt(Se2[2,2]))) / (pi1 * dnorm(X$x, m1[1], sqrt(Se1[1,1])) * dnorm(X$y, m1[2], sqrt(Se1[2,2])))))
}

# Porównanie z wartościami otrzymanymi z rozkładów
contour(xp, yp, matrix(f.nb(gr, me1, me2, Se1, Se2, pi1, pi2), length(xp)), add = T, levels = 0.5, lwd = 2, lty = 2, col = "blue") 
