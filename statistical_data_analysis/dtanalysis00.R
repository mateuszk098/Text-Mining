# biblioteka do odwracania macierzy ginv() oraz losowania z wielowymiarowego rozkladu Gaussa mvrnorm()
library(MASS)

# zadana macierz kowariancji
S <- matrix(c(1,0,0,1),2,2)

# zadane wartosci srednie w danych klasach
m1 <- c(-1,1)
m2 <- c(2,4)
m3 <- c(-2,2) # jezeli m3 byloby np. (2,-2) to wtedy mozna juz w miare dobrze rozdzielic 

# licznosc klas 
n1 <- 30
n2 <- 30
n3 <- 30
n <- n1 + n2 + n3

# liczba klas
g <- 3

# losowanie wartosci macierzowych z rozkladu gaussa
X1 <- mvrnorm(n1, m1, S)
X2 <- mvrnorm(n2, m2, S)
X3 <- mvrnorm(n3, m3, S)

# wyznaczanie sredniej dla wszystkich punktow
m <- apply(rbind(X1, X2, X3), 2, mean)

# wyznaczanie macierzy zmiennosci miedzygrupowej
B <- (n1 * (m1 - m) %*% t(m1 - m) + n2 * (m2 - m) %*% t(m2 - m) + n3 * (m3 - m) %*% t(m3 - m)) / (g-1)

# wyznaczanie macierzy zmiennosci wewnatrzgrupowej
W <- ((n1 - 1) * S + (n2 - 1) * S + (n3 - 1) * S) / (n - g)

# macierz pomocnicza 
U <- ginv(W) %*% B

# wyznaczanie wartosci wlasnych i wektorow wlasnych macierzy pomocniczej
lambda <- eigen(U)

# wektor wlasny odpowiadajacy najwiekszej wartosci wlasnej
a <- lambda$vectors[,which.max(lambda$values)]

# nanoszenie punktow na wykres
plot(X1, xlim = c(-2,4), ylim = c(-2,7), pch = 19, col = "blue", xlab = "X", ylab = "Y", font = 2, asp = 1)
abline(v = 0, h = 0, col = "gray")
points(X2, pch = 19, col = "orange") 
points(X3, pch = 19, col = "red") 

# rysowanie kierunku a
abline(0, a[2] / a[1], col = "green", lwd = 2) 

# funkcja do rzutowania obserwacji na kierunek a
rzutowanie <- function(X, A) {
  Xz <- (X[,2] * A + X[,1]) / (A**2 + 1)
  Yz <- A * Xz
  data.frame(x = Xz, y = Yz)
}

# Wyznaczenie współczynnika kierunkowego prostej
A <- a[2] / a[1]

# Wykreślanie zrzutowanych punktów
points(rzutowanie(X1, A), col = "blue", pch = 19)
points(rzutowanie(X2, A), col = "orange", pch = 19)
points(rzutowanie(X3, A), col = "red", pch = 19)

# W ogolnosci przy takich postaciach wartosci srednich w tych klasach, nie da sie ich optymalnie rozdzielic
