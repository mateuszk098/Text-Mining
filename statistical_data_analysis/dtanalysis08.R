# Wczytanie danych
data <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data", header = FALSE)

# Same dane bez klas
data.val <- data[, 2:ncol(data)]

cumulated.std <- function(data, k, test.pc){
  
  # Skumulowane odchylenie standardowe
  std <- sum(test.pc$sdev[1:k]) / sum(test.pc$sdev[1:ncol(data)])
  
  return(std)
}

# Wykonanie analizy skladowych glownych
test.pc <- princomp(~., cor = T, data = data.val)

# Sekwencja liczby zmienych
k <- 1:ncol(data.val)

# Wywolanie cumulated.std dla kolejnej liczby zmiennnych
std <- lapply(k, function(k) cumulated.std(data.val, k, test.pc))

# Wykres skumulowanego odchylenia od liczby zmiennych
plot(k, std, col = "blue", type = 'p', main = 'Cumulated standard deviation',
     ylab = 'Sdev', xlab = "Number of variables - k", ylim = c(0, 1), 
     pch = 19, font = 2, font.lab = 4, font.main = 3, cex = 1)

# Wykres w nowych zmiennych dla 1 i 2 skladowej
plot(test.pc$scores[,1], test.pc$scores[,2], col = as.factor(data[,1]),
     main="First vs second main component", xlab = "First main component", ylab = "Second main component",
     pch = 19, font = 2, font.lab = 4, font.main = 3, cex = 1)

# Wykres w nowych zmiennych dla 2 i 3 skladowej
plot(test.pc$scores[,2], test.pc$scores[,3], col = as.factor(data[,1]),
     main="Second vs third main component ", xlab = "Second main component", ylab = "Third main component",
     pch = 19, font = 2, font.lab = 4, font.main = 3, cex = 1)
     
