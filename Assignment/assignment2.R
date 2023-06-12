
############################ Question A #######################################
library(imager)
data(iris)
box_iris <- boxplot(Sepal.Length + Sepal.Width + Petal.Length +
                      Petal.Width ~ Species,data = iris, col = c("red", "blue", "green"), main = "Boxplots by Species")

scat_iris <- plot(iris$Sepal.Length, iris$Petal.Length, col = iris$Species,
                  pch = 19, xlab = "Sepal.Length", ylab = "Petal.Length", main = "Scatterplot by Species")
legend("topright", legend = unique(iris$Species), col = unique(iris$Species), pch = 19)

#We can conclude that Sepal length and Petal length of
#Setosa are smaller and that of Versicolor and Virginica are in increasing order respectively
#Also, The variable used are informative in case of distinguishing Setosa from 
#the other two but Versicolor and Veiginica are not distinguishable clearly.



###########################  Question B  ######################################

flip <- function(image) {
  image.mat <- as.array(image[,,1,])
  dims <- dim(image.mat)
  flipped_image <- array(0, dim = dims)
  
  for (i in 1:dims[1]) {
    for (j in 1:dims[2]) {
      flipped_image[i, j, ] <- image.mat[dims[1] - i + 1, j, ]
    }
  }
  
  return(flipped_image)
}

#Taking example of the image dog.jpeg
dog <- load.image("dog.jpeg")
par(mfrow = c(1,2))
plot(dog, main = "Original Image")
flipped_dog <- flip(dog)
plot(as.cimg(flipped_dog), main = "Flipped Image")


########################### Question C #######################################
library(MASS)
data(ships)
#####################################################
plot(ships$type,ships$incidents, 
     xlab = "Type of ship", ylab = "Accidents",
     main = "Scatterplot of Ship Type vs. Period")
#####################################################
incident_table <- table(ships$type,ships$incidents)
prop_table <- prop.table(incident_table, margin = 1)
par(mfrow = c(2, 3)) 
for (i in 1:nrow(prop_table)) {
  pie(prop_table[i, ], labels = colnames(prop_table), 
      main = paste("Ship Type:", rownames(prop_table)[i]))
}

#From the Above graph, it is justified that Ship Type B is more prone to Accidents
########################## Question D ##########################################

library(rvest)
url <- "https://stats.stackexchange.com/questions?tab=votes&pagesize=50"
page <- read_html(url)
Questions <- page %>% html_nodes("#questions .s-link") %>% html_text()
Views <- page %>% html_nodes(".s-post-summary--stats-item:nth-child(3) .s-post-summary--stats-item-number") %>% html_text()
Text <- page %>% html_nodes(".s-post-summary--stats-item-number") %>% html_text()
positions <- seq(from = 2, to = length(Text), by = 3)
Answers <- Text[positions]
pos_votes <- seq(from = 1, to = length(Text), by = 3)
Votes <- Text[pos_votes]

Site.data <- data.frame(Questions,Views,Answers,Votes, stringsAsFactors = FALSE)

############################# Question E ##################################
# Number of simulations
num_simulations <- 10000

# Vector to store the number of days for each simulation
num_days <- numeric(num_simulations)

# Perform the simulations
for (i in 1:num_simulations) {
  bottle <- rep("whole", 100)
  days <- 0
  
  while (TRUE) {
    
    selected_tablet <- sample(bottle, 1)
    if (selected_tablet == "half") {
      break
    }
    bottle <- c(bottle, "half")
    days <- days + 1
  }
  num_days[i] <- days
}
average_days <- mean(num_days)
print(average_days)

