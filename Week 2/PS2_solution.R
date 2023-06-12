library(tidyverse)
library(rvest)
library(dplyr)
library(imager)
library(stringr)
#-------------------------------#
# Ques 1
html <- read_html("https://iitk.ac.in/math/visitors-post-doctoral-fellow")
# the names of the post-docs are in head2 class
names <- html_elements(html, ".head2")
names <- html_text(names)
# instead of reassigning names very time we can use pipes (%>%)
 #syntax is : variable = variable %>% func1() %>% func2() %>% func3() ...
#names <- html %>% html_elements(".head2") %>% html_text()

#-----------------------------------#
# ques 2
html2 <- read_html("https://www.imdb.com/chart/top/")
movie_names <- html2 %>% html_elements(".titleColumn a") %>% html_text()
# movie names are in the a tag of the class ,
#if we do not include a in the command we will get the text of all tags in the class

#---------------------------------#
#ques 3
movie_year <- html2 %>% html_elements(".secondaryInfo") %>% html_text() %>%
                    substr(2, 5) %>% as.numeric()
movie_rating <- html2 %>% html_elements(".ratingColumn.imdbRating strong") %>% html_text() %>% as.numeric()

movie_votes <- html2 %>% html_elements(".ratingColumn.imdbRating strong") %>% html_attr("title")
for(i in 1:250){
    movie_votes[i] <-  strsplit(movie_votes[i]," ")[[1]][4]
}



# we need to remove ',' and to be able to change it to numeric
movie_votes <- str_remove_all(movie_votes, "[,]") %>% as.numeric()

movie_data <- data.frame("name" = movie_names, "year" = movie_year,
                         "rating" = movie_rating, "votes" = movie_votes)

#------------------------------------#
# Ques 4
urls_codes <- html2 %>% html_elements(".wlb_ribbon") %>% html_attr("data-tconst")
for(i in 1:250){
    urls_codes[i] <- paste("https://www.imdb.com/title/", urls_codes[i], "/ratings",sep = '')
}

movie_html <- read_html(urls_codes[1])
temp <- html_table(movie_html)

#----------------------------------#
# Question 5
poster_links <- html2 %>% html_elements(".posterColumn a img") %>% html_attr("src")
img <- load.image(poster_links[1])