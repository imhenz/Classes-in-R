library(R6)

# ==================== Question 1 =============================================

Account <- R6Class(
  "Account",
  public = list(
    balance = 0,
    account_holder = "",
    
    deposit = function(amount) {
      self$balance = self$balance + amount
    },
    
    withdraw = function(amount) {
      self$balance = self$balance - amount
    }
  )
)

# ==================== Question 2 =============================================

CheckAccount <- R6Class(
  "CheckAccount",
  
  inherit = Account,
  
  public = list(
    overdraft_max = -200,
    
    withdraw = function(amount) {
      value = self$balance - amount
      
      if(value < self$overdraft_max) {
        stop("You cannot go above the maximum overdraft")
      } else {
        super$withdraw(amount)
      }
    }
    
  )
)

# ==================== Question 3 =============================================

SafeAccount <- R6Class(
  "SafeAccount",
  
  private = list(
    balance = 0
  ),
  
  public = list(
    account_holder = ""
  ),
  
  active = list(
  
    deposit = function(value) {
      
      # if there's no deposit, print out the balance of the user
      if(missing(value)) {
        return(paste("Current balance:", private$balance))
      }
      
      # The program should not accept a vector of values
      # This is to ensure the acceptance of scalar values only.
      if(length(value) != 1) {
        stop("The amount to deposit should be a single value and not a vector of numbers")
      }
      
      # Amount to be deposited must be a number either numeric or
      # a string that can be converted to a number.
      if(is.character(value)) {
        tryCatch(value <- as.numeric(value), warning = function(w) {})
      }
      if(!is.numeric(value) || is.na(value)){
        warning("The amount to be deposited must be a valid number")
        return()
      }
      
      # We cannot deposit a negative value: 
      # This will be equivalent to withdrawal. So throw an error here.
      if(value <= 0) {
        stop("The amount to be deposited must be greater than 0")
      }
      
      # Increase balance since all constraints have been satisfied
      private$balance <- private$balance + value
    },
    
    withdraw = function(value) {
      
      # Program warns and does nothing if there's no specified amount
      if(missing(value)) {
        warning("You have not stated the amount to be withdrawn")
        return()
      }
      
      # The program should not accept a vector of values
      if(length(value) != 1) {
        stop("Please specify a single value e.g 100 and not a vector of values")
      }
      
      # Amount to be withdrawn must be a number either numeric or
      # a string that can be converted to a number.
      if(is.character(value)) {
        tryCatch(value <- as.numeric(value), warning = function(w) {})
      }
      if(!is.numeric(value) || is.na(value)){
        warning("The amount to be withdrawn must be a valid number")
        return()
      }
      
      # user cannot withdraw a negative value: 
      # This will be equivalent to depositing. So throw error here.
      if(value <= 0) {
        stop("The amount to be withdrawn must be greater than 0")
      }
      
      # The value to be withdrawn must also to be <= balance since we didn't specify
      # an overdraft
      result <- private$balance - value
      
      if(result < 0) {
        stop(paste("You cannot withdraw more than your balance:", private$balance))
      }
      
      private$balance <- result
      
      print(paste("You've successfully withdrew:", value))
      print(paste("Current balance:", private$balance))
    }
  )
)




# FOR TESTING ONLY: DELETE AFTER TESTING

acc <- Account$new()
acc$deposit(100)
acc$withdraw(50)
acc$balance


check_acc <- CheckAccount$new()
check_acc$deposit(100)
check_acc$withdraw(50)
check_acc$withdraw(500)
check_acc$balance


safe_acc <- SafeAccount$new()
safe_acc$deposit = 100
safe_acc$withdraw = 50
safe_acc$deposit
