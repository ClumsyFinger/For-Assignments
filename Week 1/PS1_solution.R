my_factotial <- function(n){
  if(n < 0 ){
    print("Invalid Input")
  }
  if(n<=1){
    return(1)
  }
  return(prod(1:n))
}


#################
func <- function(n){
  return ((1+1/n)^n)
}


data <- read.csv("SOC171A_Survey_Responses.csv")
head(data) # to see the top of the data
str(data) # structure of the data
dim(data) 

k = (data$Gender == "Male")
Male_data = data[k,]

indeces <- which(data$Marriage.preference == 'Love Marriage' & data$Category == 'OBC-NCL')
new_data <- data[indeces, ]


###########

#########################
primes <- 2
count <- 1
m <- 3
while(count<= 1000){
  m.is.prime <- T
  for(p in primes){
    if(m%%p==0){
      m.is.prime = F
      break
    }
  }
  if(m.is.prime){
    primes <- c(primes, m)
    count <- count + 1 
  }
  m = m + 2
}
primes

#################
fib <- numeric(length = 500)
fib[1] = 1
fib[2] = 1
for(i in 3:500){
  fib[i] <- fib[i-1] + fib[i-2]
}
fib


##########################
cricket <- read.csv("https://dvats.github.io/assets/course/mth208/battingbowling.csv")
allRounders  <- cricket[which(cricket$Bowling<40 & cricket$Batting>25), ]                  
allRounders$Team <- as.factor(allRounders$Team)
teams <- unique(allRounders$Team)

for(t in teams){
  count <- sum(allRounders$Team == t)
  print(paste(t, count))
}


# Simulating Experiments

#Question 1

tosses <- rbinom(n = 1e3, size = 1, prob = 0.50)
mean(tosses)

sum(tosses)/1000
#Question 2

#(a)

sample(1:3, size = 1, prob = c(3,2,2)/7)



# (b)

dart <-  runif(1, min= 0, max = 5)



# (c)
A <- matrix(c(3, 4, -1,
               1, 5, 2,
              -2, 3, -2), nrow = 3, ncol = 3, byrow = T)


p_vec <- numeric(length = 3)





for(k in 1:ncol(A))
{
  p_vec[k] <- norm(A[ ,k], type = "2")
  # p_vec[k] = sqrt(sum(A[, k]^2  ))
}

p_vec <- p_vec/sum(p_vec)

sample(1:3, size = 1, prob = p_vec)




# Question 3

num = function(){
  value_till_now = 0
  
  count = 0
  while(value_till_now < 1){
    value_till_now = value_till_now + runif(1)
    count = count +1
  }
  
  
  return(count)
}


values = numeric(length = 1000)
for(i in 1: 1000){
  values[i] = num()
}

mean(values)

# Question 4

candles_recursive = function(n){
  if(n==0){
    return (0);
  }
  

  blown = sample(1:n, 1)
  return(1 + candles(n-blown));
 
}

candles_iterative = function(n){
  remaining = n
  count = 0
  
  while(remaining > 0){
    blown  = sample(1:remaining, 1)
    remaining = remaining - blown
    count = count +1
  }
  
  return(count)
}

values = numeric(length = 1000)

for(i in 1:1000){
  values[i] = candles_iterative(25)
}

mean(values)

for(i in 1:1000){
  values[i] = candles_recursive(25)
}

mean(values)
