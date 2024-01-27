
remind_me <- function() {
  message("Don't forget to buy: Milk, Hummus, Bread")
}

cheat <- function(exercise_number) {
 
  solutions <- list(
    `Exercise 1` = "Solution 1",
    `Exercise 2` = "Solution 2",
    `Exercise 3` = "Solution 3"
  )
  
  if (exercise_number %in% names(solutions)) {
    message(paste("For", exercise_number, ":", solutions[[exercise_number]]))
  } else {
    warning("Exercise not found.")
  }
}


#Example:
cheat("Exercise 1")

#output: solution 1

#Link to the functions: 
