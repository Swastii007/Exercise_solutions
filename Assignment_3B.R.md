
#Q3.2R.1-------------------------------------------------------------------------
#remind_me function
remind_me <- function() {
  message("Don't forget to buy: Milk, Hummus, Bread")
}

#cheat_function
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

#output: Exercise not found (because it is case sensitive)

#Link to the functions: https://github.com/Swastii007/Exercise_solutions



#Q3.2R.2-------------------------------------------------------------------------

#make_art function


# Set seed for reproducibility
make_art <- function(seed = NULL) {
  
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
# Generate random art 
  art <- matrix(runif(100), ncol = 10, nrow = 10)
  
# Display the art 
  
  image(art, col = gray.colors(10))
}


#example:
make_art()

#or

make_art(seed = 123)









