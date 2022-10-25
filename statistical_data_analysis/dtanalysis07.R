# Zadanie 1

# Wczytanie danych (20 zwierzat opisanych przez 6 roznych charakterystyk)
animals <- cluster::animals
colnames(animals) <- c("warm-blooded", 
                       "can fly",
                       "vertebrate",
                       "endangered",
                       "live in groups",
                       "have hair")

# Tworzenie mapy cieplnej
heatmap(as.matrix(animals), col = c("red", "green")) 

# Widzimy, ze istnieja ogolnie 4 grupy zwierzat 
# - stalocieplne kregowce zyjace w grupach, majace wlosy, nie potrafiace latac i ktore nie sa zagrozone,
# - zmiennocieplne bezkregowce, ktore nie sa zagrozone,
# - zmiennocieplne kregowce, nie majace wlosow, nie potrafiace latac i niezagrozone,
# - stalocieplne kregowce, nie majace wlosow, niektore mogace latac i niektore zagrozone




# Zadanie 2

# Wczytanie danych i nazwanie kolumn
data <- iris
colnames(data) <- c("1", "2", "3", "4", "class")

# Macierz kombinacji zmiennych
M <- matrix(list(), nrow = 11, ncol = 1)
M[[1]] <- c(1,2)
M[[2]] <- c(1,3)
M[[3]] <- c(1,4)
M[[4]] <- c(2,3)
M[[5]] <- c(2,4)
M[[6]] <- c(3,4)
M[[7]] <- c(1,2,3)
M[[8]] <- c(1,3,4)
M[[9]] <- c(2,3,4)
M[[10]] <- c(1,2,4)
M[[11]] <- c(1,2,3,4) # Chyba wszystkie

# Wyznaczamy parametr W, to nam powie dla jakich kombinacji zmiennych mamy optymalny podzial 
# kmeans dla wartosci k = 3
W <- lapply(M, function(i) kmeans(data[,i], 3)$tot.withinss)

# Wykres parametru W dla roznych kombinacji zmienncyh zbioru iris
plot(1:length(W), W, xaxt = "n", col = "blue", type = 'o', main = 'Parametr W analizy skupie�',
     ylab = 'Parametr W', xlab = "Kombinacja zmiennych", pch = 19, font = 2, font.lab = 4,
     font.main = 3, cex = 1, lwd = 2)

axis(1, at = 1:length(W), font = 2,
     labels = c("1,2", "1,3", "1,4", "2,3", "2,4", "3,4", "1,2,3", "1,3,4", "2,3,4", "1,2,4", "1,2,3,4"))

legend("topleft", legend=c(
  paste("1 - ", colnames(iris)[1]),
  paste("2 - ", colnames(iris)[2]),
  paste("3 - ", colnames(iris)[3]),
  paste("4 - ", colnames(iris)[4]))
)


# Parametry W czesto bardzo sie roznia ale najczesciej najmniejsza wartosc mamy dla 
# kombinacji 2,4 i 3,4 skladowej