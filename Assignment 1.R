library(tidyverse)
library(rvest)
library(dplyr)
library(stringr)

#### Question A ####

Nifty50 <- read_html("https://www.moneyworks4me.com/best-index/nse-stocks/top-nifty50-companies-list/")

###### Nifty50_table ############################################################################
Nifty50_table <- Nifty50 %>% html_nodes("table") %>% .[1] %>% html_table() %>% .[[1]]
  Nifty50_table <- Nifty50_table[,-c(1,14,15)]
View(Nifty50_table)

################################ Question B #######################################################################################

# For Mahindra Motor crops #
Mahindra_page <- read_html("https://www.moneyworks4me.com/indianstocks/large-cap/automobiles/automobiles-passenger-cars/mahindra-mahindra/company-info")

    body_mahindra1 <- Mahindra_page %>% html_nodes("tbody") %>% .[2] %>% html_table() %>% .[[1]]
    body_mahindra2 <- Mahindra_page %>% html_nodes("tbody") %>% .[4] %>% html_table() %>% .[[1]]
    head_mahindra1 <- Mahindra_page %>% html_nodes("thead") %>% .[4] %>% html_table() %>% .[[1]]    

    Mahindra_table <- rbind(body_mahindra1,body_mahindra2)    
    mahindra_colnames <- c(head_mahindra1[1,])
    mahindra_colnames <- unlist(mahindra_colnames)    
    colnames(Mahindra_table) <- mahindra_colnames
Mahindra_table <- Mahindra_table[,-12]
  
View(Mahindra_table)
############## for Wipro ###############################################################
 wipro_page <- read_html("https://www.moneyworks4me.com/indianstocks/large-cap/it-ites/it-software/wipro/company-info")
  
    body_wipro1 <- wipro_page %>% html_nodes("tbody") %>% .[2] %>% html_table() %>% .[[1]]
    body_wipro2 <- wipro_page %>% html_nodes("tbody") %>% .[4] %>% html_table() %>% .[[1]]
    wipro_table <- rbind(body_wipro1,body_wipro2)
    head_wipro1 <- wipro_page %>% html_nodes("thead") %>% .[4] %>% html_table() %>% .[[1]]
    col_wipro <- c(head_wipro1[1,])
    col_wipro <- unlist(col_wipro)
    col_wipro[1] <- c(" ")
  
    colnames(wipro_table) <- col_wipro
  wipro_table <- wipro_table[,-12]
 
View(wipro_table)
########### FOR INFOSYS ############################################## 
  
Infosys_page <- read_html("https://www.moneyworks4me.com/indianstocks/large-cap/it-ites/it-software/infosys/company-info")
  
      body_Infosys1 <- Infosys_page %>% html_nodes("tbody") %>% .[2] %>% html_table() %>% .[[1]]    
      body_Infosys2 <- Infosys_page %>% html_nodes("tbody") %>% .[4] %>% html_table() %>% .[[1]]     
      head_Infosys <-  Infosys_page %>% html_nodes("thead") %>% .[4] %>% html_table() %>% .[[1]]      
  
    Infosys_table <- rbind(body_Infosys1,body_Infosys2)      
      Infosys_table <- Infosys_table[,-12]    
    Infosys_colnames <- c(head_Infosys[1,])
      Infosys_colnames <- unlist(Infosys_colnames)    
    colnames(Infosys_table) <- Infosys_colnames
View(Infosys_table)    
################ FOR RELAINCE #############################################
Reliance_page <- read_html("https://www.moneyworks4me.com/indianstocks/large-cap/oil-gas/refineries/reliance-industries/company-info")
    
        body_Reliance1 <- Reliance_page %>% html_nodes("tbody") %>% .[2] %>% html_table() %>% .[[1]]
        body_Reliance2 <- Reliance_page %>% html_nodes("tbody") %>% .[4] %>% html_table() %>% .[[1]]      
        head_Reliance <-  Reliance_page %>% html_nodes("thead") %>% .[4] %>% html_table() %>% .[[1]]
    
      Reliance_table <- rbind(body_Reliance1,body_Reliance2)     
        Reliance_table <- Reliance_table[,-12]    
      Reliance_colnames <- c(head_Reliance[1,])
        Reliance_colnames <- unlist(Reliance_colnames)
      colnames(Reliance_table) <- Reliance_colnames  
View(Reliance_table)    
#################################### For TITAN #############################
Titan_Page <- read_html("https://www.moneyworks4me.com/indianstocks/large-cap/consumer-durables/diamond-jewellery/titan-co/company-info")
    
        body_Titan1 <- Titan_Page %>% html_nodes("tbody") %>% .[2] %>% html_table() %>% .[[1]]
        body_Titan2 <- Titan_Page %>% html_nodes("tbody") %>% .[4] %>% html_table() %>% .[[1]]
        head_Titan <-  Titan_Page %>% html_nodes("thead") %>% .[4] %>% html_table() %>% .[[1]]      
    
      Titan_table <- rbind(body_Titan1,body_Titan2)
        Titan_table <- Titan_table[,-12]    
      Titan_colnames <- c(head_Titan[1,])      
        Titan_colnames <- unlist(Titan_colnames)    
      colnames(Titan_table) <- Titan_colnames  
View(Titan_table)
########### Question C ######################################################################################
tennis <- function(p) {
  # Simulate the tennis match
  win_sequence <- sample(c("A", "B"), size = 5, replace = TRUE, prob = c(p, 1 - p))
  
  # Determine the number of sets played
  x <- ifelse(any(win_sequence[1:4] == win_sequence[5]), which.max(win_sequence[1:4] == win_sequence[5]), 5)
  
  return(x)
}
matches <- numeric(1000)
for (i in 1:1000) {
  matches[i] <- tennis(0.70)
}

ans <- mean(matches)



############## Question D ###############################################################################
MontyHall <- function() {
  doors <- sample(c("car", "goat", "goat"))
  choice <- sample(1:3, size = 1)
  opened_door <- which(doors != "car" & seq_along(doors) != choice)
  new_choice <- which(seq_along(doors) != choice & seq_along(doors) != opened_door)
  
  # Check if the contestant won the car
  win <- ifelse(doors[new_choice] == "car", 1, 0)
  
  return(win)
}

# Simulate the Monty Hall game show 1000 times 
num_simulations <- 1000
wins <- replicate(num_simulations, MontyHall())
probability <- mean(wins)

# Display the estimated probability of winning when the contestant switches
probability


#################### Question E #################################################################################
Ranking_page <- read_html("https://editorial.rottentomatoes.com/guide/best-netflix-movies-to-watch-right-now/")

  ranks <- Ranking_page %>% html_elements(".countdown-index") %>% html_text()
  Movie_name <- Ranking_page %>% html_elements("h2 a") %>% html_text()
  Movie_name <- Movie_name[-c(101,102)]
  tomato_score <- Ranking_page %>% html_elements(".tMeterScore") %>% html_text()
  movie_year <- Ranking_page %>% html_elements(".start-year") %>% html_text()


  movie_data <- data.frame(`Ranking` = ranks, `Name of Movie` = Movie_name,
                         `Tomato % score` = tomato_score, `Year of movie` = movie_year, check.names = FALSE)
  View(movie_data)