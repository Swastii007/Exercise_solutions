
remind_me <- function() {
  message("Don't forget to buy: Milk, Hummus, Bread")
}

cheat <- function(exercise_number) {
 
  solutions <- list(
    `Exercise 3.1.1` = "Solution 1",
    `Exercise 3.1.2` = "Solution 2",
    `Exercise 3.1.3` = "Solution 3"
  )
  
  if (exercise_number %in% names(solutions)) {
    message(paste("For", exercise_number, ":", solutions[[exercise_number]]))
  } else {
    warning("Exercise not found.")
  }
}

#Link to the functions: https://github.com/SwastiSharma/your-repo/remind_me_cheat.R
