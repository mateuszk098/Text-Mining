library(geosphere, quietly = T, warn.conflicts = F) 
library(jsonlite)
library(ggmap)
library(dplyr)

register_google("AIzaSyDUSXYLw5k4xSw-j_au65Gi_YnPYWuV75I")

# Function to sampling trolley road every minute
single_entry <- function(){
   Sys.sleep(60)
   um.waw.api <- "9c9a80e9-68d1-4f74-8d49-c241f3f5649f"
   um.waw.url <- "https://api.um.warszawa.pl/api/action/busestrams_get/?resource_id=f2e5503e-927d-4ad3-9500-4ab9e55deb59&apikey="
   url.api <- paste(um.waw.url, um.waw.api, "&type=2", sep = "")
   trams <- fromJSON(url.api)
   tram_10 <- trams$result[(trams$result$Lines == "10") & (trams$result$Brigade == "021"),]
   return(tram_10)
}

# How many minutes
n <- 40
tram_10_road <- replicate(n+1, single_entry(), simplify = FALSE)
tram_10_road <- bind_rows(tram_10_road, .id = "step")

# For some reason I have to do this 
colnames(tram_10_road)[colnames(tram_10_road) == "Lon"] <- "lon"
colnames(tram_10_road)[colnames(tram_10_road) == "Lat"] <- "lat"
tram_10_road$step <- as.numeric(tram_10_road$step)

# Drawing map 
map <- get_googlemap(center = c(lon = 21.00, lat = 52.25), zoom = 12, maptype = "road",
                     color="bw", language = "PL")
ggmap(map) + 
   geom_path(data = tram_10_road, aes(color = step, group = 1), size = 2, lineend = "round") +
   scale_color_gradient(low="blue", high="cyan", name= "Minutes") +
   theme_void()

# We have to got distances between each next pair of points
distance <- mutate(tram_10_road[c(3, 6)], 
                   Distance = distHaversine(cbind(lon,lat), cbind(lag(lon),lag(lat))))/1000
distance <- distance$Distance
# First value is distance between same points
distance <- distance[2:(n+1)]
# Velocity in each sector
velocity <- distance/(1/60) 
# Mean velocity in the sum of consecutive sectors
step <- seq(1,n,1)
mean_velocity <- cumsum(distance)/(step/60)

# Drawing plot
plot(step, velocity, pch = 18, col = "blue", type = "b", lty = 2, xlab = "Minute", ylab = "Velocity (km/h)",
     main = "Trolley 10: Sector and Mean Velocity",font = 2, font.lab = 4, font.main = 3, cex = 1.25, lwd = 1)
lines(step, mean_velocity, col = "red", type = "l", lwd = 3)
legend("topright", legend=c("Sector Velocity", "Mean Velocity"), col=c("blue", "red"), lty = 2:1, cex = 1)
grid(lty = 6, col = "cornsilk2", lwd = 1) 
