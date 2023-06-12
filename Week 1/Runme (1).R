
# Use ctrl + enter to run a line
## Basic arithmatic
1 + 2 
3 + 5 - 2

## assigning value
a <- 4  #use can also use =
b = 5
my.number <- 8
## Two way to declare vector
my.vector <- 5:10
v <- c(907,220,625,502)

my.vector
v
my.number

## operations with objects
a + b
my.number %% 5   #modulo
my.vector %% 5

## Calculating any expression: 
b/a + sqrt(my.number) - sin(b) + log(a)

## Logical checks
a == 10  # is a = 10?
my.number == a
my.vector == my.number

index <- which(my.vector == my.number)
my.vector[index]


# R is great for vectors
sum(my.vector)
mean(my.vector)
median(my.vector)
max(my.vector)
min(my.vector)

## Making functions
greetings <- function(name)
{
  text <- paste("Hello", name)
  return(text)
}

## Calling of Function
greetings("Guys")
greetings("Mr. Bean")

# function for evaluating f(x) = 5x^2 + 3x - 3

fx <- function(x)
{
  val <- 5*x^2 + 3*x - 3
  return(val)
}

fx(4)
fx(10)
fx(4:10)
fx(a)
fx( c(4, 9, 15) )
x <- c(4, 9, 15)
fx(x)


# Loops in R

## For Loop
track <- 0
for(i in 1:length(my.vector))
{
  track <- track + my.vector[i]
}
track


## While Loop

track2 <- 0
n <- length(my.vector)
while(n)
{
  track2 <- track2 + my.vector[n]
  n = n - 1
}
track2

## if-else statement

vec <- 1:100
vec1 <- vector(length = 100) ## Declaring a Vector
for(i in 1:length(vec))
{
  if(vec[i]%%2 == 0)
  {
    vec1[i] = paste(i,"is an even number")
  }
 else
  {
    vec1[i] = paste(i,"is an odd number")
  }
}

vec1


## Declaration of Matrix

M <- matrix(c(1,1,1,2,1,3,1,4),nrow=2,ncol=4)
M
M[1,]
M[,1]

r1 <- c(1, 1, 1, 1)
r2 <- c(1, 2, 3, 4)
M <- rbind(r1, r2)
M

c1 <- c(1, 1)
c2 <- c(1, 2)
c3 <- c(1, 3)
c4 <- c(1, 4)
M <- cbind(c1,c2,c3,c4)
M
M[2,3]

## rep() is a function in R, let us explore its working
?rep
rep(0, 5)
rep(1,10)

## To declare a diagonal matrix
K3 = diag(3)
K4 = diag(4)
K3
K4

# Loading a csv file (csv is much easier than xls)
## Save the "SOC171A Survey (Responses)" file in your system
## set the working directory where you save this file
## use can check your present working directory bt getrwd() function
## to set a working directory we can use setwd("address of the folder")
## if still not able to understand it just google it.

data <- read.csv("SOC171A_Survey_Responses.csv")
head(data)  # to see the top of the data
str(data)   # structure of the data
dim(data)   # dimensions

data[1,1]
data[1, ]
data[3:5, ]
data[c(4, 7, 9), ]

# When in doubt
?head
?dim
?read.csv


