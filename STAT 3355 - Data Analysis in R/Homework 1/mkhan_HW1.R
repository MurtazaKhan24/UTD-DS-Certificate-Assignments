# Problem 1

round(8 + 9 - 7 / 3 ^ 0.3, digits=0)

round(log2(sqrt((15+16)/(14+12))), digits=2)

round(((11 + sin(pi/4)) / (factorial(3) + abs(-10)))^2, digits=0)

round( 6 + 5 - 4 / 3^2 ,digits = 0)

round(exp(sqrt((14 + 13) / (12 + 11))), digits=0)

round(((11 + factorial(12)) / (factorial(13) + 14))^2, digits = 2)

# Problem 2

RF <- c(2.6, 3.05, 3.74, 3.48, 5.49, 4.25, 2.57, 2.18, 3.14, 4.82, 3.28, 3.01)


## Create months vector to add to RF
RF_months <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')

## Mean yearly rainfall
round(mean(RF), digits = 2)

## Month with min rain
RF_months[RF == min(RF)]

## Month with max rain
RF_months[RF == max(RF)]

# Problem 3
H2 <- c(2700, 2600, 3050, 2900, 3000, 2500, 2600, 3000, 2800, 3200, 2800, 3400)

H2_months <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')

## Sum of hummers sold in 2002
sum(H2)

## Month with greatest increase in sales compared to prev. month
H2_months[which(diff(H2, differences = 1) == max(diff(H2, differences = 1))) + 1]

## Month with greatest decrease in sales compared to prev. month
H2_months[which(diff(H2, differences = 1) == min(diff(H2, differences = 1))) + 1]



# Problem 4

## creates new vector with given values
arr <- c(1, -2, 3, -4, 5, 100)

## creates a new vector with prev. vector values each multiplied by -1
product_arr <- arr * -1

## returns new vector values that are positive
product_arr[ product_arr > 0]

## create a sequence from 1-50
seq_to_fifty <- seq(1:50)

## test whether an observation is even
check_if_even <- seq_to_fifty %% 2 ==0

## subset seq_to_fifty by the test above
seq_to_fifty <- seq_to_fifty[check_if_even]
seq_to_fifty

## create a function that calculates the mean of given values
calculate_mean <- function(values) {
  sum(values) / length(values) 
}

# Problem 5
## creates a function to check if values are a perfect square
PrintSquare <- function() {
  ## runs a for loop from 1:1000 to check for perfect square
  for (i in 1:1000) {
    ## check if the square root of current val has no leading decimals
    if(sqrt(i) %% 1 == 0) {
      ## if not, then the value is a perfect square
      print(i)
    }
  }
}

# Problem 6

TwinPrimes <- function(n) {
  ## Create a vector to store initial prime numbers
  primes <- c(2, 3)
  
  ## Loop through remaining numbers(4-n)
  for (i in 4:n) {
    ## Assume i-th number is prime
    is_prime <- TRUE
    
    ## Check if the number is divisible by any other primes found, if it is then end the loop
    for (j in primes) {
      if (i %% j == 0) {
        is_prime <- FALSE
        break
      }
    }
    ## If number is prime, add it to the list of primes
    if (is_prime) {
      primes <- c(primes, i)
    }
  }
  ## Use diff method to determine the amount of primes with a difference of two and store the sum to the function.
  prime_diffs <- diff(primes)
  TwinPrimes <- sum(prime_diffs == 2)
  return(TwinPrimes)
}

TwinPrimes(32)



