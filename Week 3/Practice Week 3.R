library(imager)

dog <- load.image("dog.jpeg")


col.mat <- as.array(land[,,1,])
dist <- apply(col.mat, c(1,2), function(s) norm(s - c(0,1,0),"2"))
indices <- which(dist == min(dist), arr.ind = TRUE)
plot(dog)
points(indices, col = "red")





col1 <- load.image("col1.png")
col2 <- load.image("col2.png")
col3 <- load.image("col3.png")

dcol <- function(image,col){
  
  col.mat <- as.array(image[,,1,])
  dist <- apply(col.matrix,c(1,2),function(s) norm(s - col,"2"))
  return(dist)
  
}

which_col <- function(image){
  dist.col <- numeric(length = 3)
  col.matrix <- diag(3)
    for(k in 1:3){
      dist.col[k] <- mean(dcol(image, col = col.matrix[,k]))
    }
  indices <- which.min(dist.col)
  guess <- c("red","green","blue")[indices]
  return(guess)
}

which_col(col1)

#########################

land1 <- load.image("land1.jpeg")
land2 <- load.image("land2.jpeg")
plot(land2)

dist_land1 <- mean(dcol(land1,c(1,1,1)))
dist_land2 <- mean(dcol(land2,c(1,1,1)))

ifelse(dist_land1 < dist_land2, "Land1", "Land2")

##################################
col.mat <- as.array(dog[,,1,])
dims <- dim(dog)
Rot <- array(0,dim = dims)



#################################
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
