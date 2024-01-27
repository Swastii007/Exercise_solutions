
#Swasti Shrama
#Assignment-3

#Q3.1.1---------------------------------------------------------------------------

total_students <- 70
set.seed(123)
#random grades for assignment and exam
assignment_grades <- matrix(runif(total_students * 4, min = 0, max = 30), ncol = 4)
exam_grades <- matrix(runif(total_students, min = 0, max = 80), ncol = 1)
#combination of assignments and exam grades
total_grades <- rowSums(assignment_grades) + exam_grades
#applying half point
total_grades_half_points <- round(total_grades / 2, 1)
#plot
hist(total_grades_half_points, breaks = seq(0, 100, by = 5), col = "skyblue", border = "black",
     main = "Simulated Grades Distribution", xlab = "Total Half-Point Grades",
     ylab = "Number of Students")
grid()

#Q3.1.2----------------------------------------------------------------------------

library(tidyverse)

data_url <- "https://bit.ly/3GLVQ86"
weather_data <- read_csv(data_url) #load data

weather_data$DATE <- as.Date(weather_data$DATE) #convert 'DATE' to date format.

#data filteration for the last 70 years.
last_seventy_years <- filter(weather_data, DATE >= Sys.Date() - 365*70)

#plot
ggplot(last_seventy_years, aes(x = DATE, y = TMIN)) +
  geom_point() +
  labs(title = "Minimum Temperature at Schiphol Airport (Last 70 Years)",
       x = "Time",
       y = "Minimum Temperature (TMIN)") +
  theme_minimal()

#Q3.1.3----------------------------------------------------------------------------

#install and load packages
library(ggplot2)
install.packages("titanic")
library(titanic)

#load dataset
data("titanic_train")

#dataframe
gender_data <- data.frame(
  gender = rep(c("female", "male"), each = 2),
  status = rep(c("alive", "dead"), times = 2),
  count = c(230, 310, 105, 590)
)

#barplot
ggplot(gender_data, aes(x = factor(gender), y = count, fill = factor(status))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Titanic Survival by Gender",
       x = "Gender",
       y = "Count",
       fill = "Survival Status") +
  scale_fill_manual(values = c("alive" = "green", "dead" = "red")) +
  theme_minimal()

#Q3.1.4-----------------------------------------------------------------------------

library(ggplot2)
library(titanic)
data("titanic_train")
gender_data <- data.frame(
  gender = rep(c("female", "male"), each = 2),
  status = rep(c("alive", "dead"), times = 2),
  count = c(230, 310, 105, 590)
)
generate_plot <- function(theme) {
  
  p <- ggplot(gender_data, aes(x = factor(gender), y = count, fill = factor(status))) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Titanic Survival by Gender",
         x = "Gender",
         y = "Count",
         fill = "Survival Status") +
    scale_fill_manual(values = c("alive" = "green", "dead" = "red")) +
    theme_set(theme) +
    theme_minimal()
  print(p)
}

#experimenting with themes

# BEST: theme_gray: provides a clean and neutral background, making the bar plot clear and easy to read. 

generate_plot(theme_gray())


# WORST: theme_void: no axes and labels makes it difficult to read.

generate_plot(theme_void())


#Q3.1.5----------------------------------------------------------------------------

plot(cars$speed, cars$dist)

#adding axis labels

plot(cars$speed, cars$dist, xlab = "Speed", ylab = "Distance")

# changing colour

plot(cars$speed, cars$dist, col = "pink")

#adding title

plot(cars$speed, cars$dist, main = "Speed vs. Distance")

#adding everything at once
plot(cars$speed, cars$dist, xlab = "Speed", ylab = "Distance", main = "Speed vs. Distance", col = "pink")

#Q3.1.6----------------------------------------------------------------------------------------------------
library(ggplot2)
library(dplyr)
data("ChickWeight")

chicks_to_plot <- c(1, 20, 5, 40, 19)
filtered_data <- ChickWeight %>% filter(Chick %in% chicks_to_plot)


filtered_data$Chick <- factor(filtered_data$Chick, levels = chicks_to_plot)

ggplot(filtered_data, aes(x = Chick, y = weight)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", fill = "black") +
  labs(x = "Chick",
       y = "Max_weight") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "lightgray"))

#Q3.1.7----------------------------------------------------------------------------

library(ggplot2)
data("cars")
ggplot(cars, aes(x = speed, y = dist)) +
  geom_point(color = "black", size = 0.5) +
  geom_smooth(method = 'loess', formula = 'y ~ x', color = 'blue', se = TRUE, fill = 'darkgray', size = 0.5) +
  labs(x = "Speed",
       y = "dist") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "lightgray"))


#Q3.1.8-------------------------------------------------------------------------------

# Load libraries
library(ggplot2)
library(dplyr)
library(patchwork)

# Load dataset
data("ChickWeight")

# Original Plot
chicks_to_plot <- c(1, 20, 5, 40, 19)
filtered_data <- ChickWeight %>% filter(Chick %in% chicks_to_plot)


filtered_data$Chick <- factor(filtered_data$Chick, levels = chicks_to_plot)
original_plot<- ggplot(filtered_data, aes(x = Chick, y = weight)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", fill = "black") +
  labs(x = "Chick",
       y = "Max_weight") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "lightgray"))

# Second Plot - Development over time
development_plot <- ChickWeight %>%
  filter(Chick %in% chicks_to_plot) %>%
  mutate(Chick = factor(Chick, levels = chicks_to_plot)) %>%  # Preserve order of levels
  ggplot(aes(x = Time, y = weight, color = as.factor(Chick))) +
  geom_line() +
  labs(x = "Time",
       y = "Weight",
       color = "Chick") +
  theme_minimal()

# Combine both plots using patchwork
combined_plot <- original_plot + development_plot +
  plot_layout(nrow = 1, ncol = 2, widths = c(1, 1.5))

# Display the combined plot
combined_plot

#Q3.1.9-----------------------------------------------------------------------------
install.packages("ggstatsplot")
library(ggstatsplot)
data("ToothGrowth")

ggstatsplot::ggbetweenstats(
  data = ToothGrowth,
  x = "supp",
  y = "len",
  title = "Comparison of Teeth Growth with Different Supplements",
  xlab = "Supplement",
  ylab = "Tooth Length",
  type = "np"
)

#Q3.1.10--------------------------------------------------------------------------

install.packages("plotly")
library(plotly)

data <- read.csv("https://shorturl.at/nouyJ")

plot_ly(data, x = ~ShoulderToWaist, y = ~HeadCircumference, z = ~TotalHeight, type = "scatter3d", mode = "markers",
        marker = list(size = 6, color = "#4682B4", opacity = 0.5)) %>%
  layout(scene = list(aspectmode = "cube"))


#Q3.1.11---------------------------------------------------------------------------

library(ggplot2)
library(plotly)

install.packages("gganimate")
library(gganimate)

install.packages("cranlogs")
library(cranlogs)

data_ggplot2 <- cranlogs::cran_downloads(package = "ggplot2", from = "2014-01-01", to = "2024-01-01")
data_plotly <- cranlogs::cran_downloads(package = "plotly", from = "2014-01-01", to = "2021-01-01")

ggplot2_animation <- ggplot(data_ggplot2, aes(x = date, y = count)) +
  geom_line(color = "red") +
  labs(title = "ggplot2 Package Popularity Over Time",
       x = "Date",
       y = "Package Downloads") +
  transition_reveal(date)

plotly_animation <- ggplot(data_plotly, aes(x = date, y = count)) +
  geom_line(color = "blue") +
  labs(title = "plotly Package Popularity Over Time",
       x = "Date",
       y = "Package Downloads") +
  transition_reveal(date)

combined_animation <- gganimate::gg_animate(
  plot_list = list(
    ggplot2_animation + gganimate::enter_fade(),
    plotly_animation + gganimate::enter_fade()
  ),
  nframes = length(unique(c(data_ggplot2$date, data_plotly$date))),
  interval = 0.1,  # Adjust the interval as needed
  filename = "animated_plot.gif",
  title_frame = TRUE
)


#Q3.1.12--------------------------------------------------------------------------

install.packages("quantmod")
library(quantmod)

stock_symbol <- "AAPL"
getSymbols(stock_symbol, src = "yahoo", from = "2023-01-01", to = "2023-12-31")

chart_Series(Cl(get(stock_symbol)), name = paste(stock_symbol, "Closing Price"))



#Q3.1.13---------------------------------------------------------------------------

library(quantmod)
plotstock <- function(stock_symbol = "AAPL", year = 2023, file_name = "stock_plot.png") {

  start_date <- paste0(year, "-01-01")
  end_date <- paste0(year, "-12-31")
  getSymbols(stock_symbol, src = "yahoo", from = start_date, to = end_date)
  
  # Plot the closing price using chart_Series
  chart_Series(Cl(get(stock_symbol)), name = paste(stock_symbol, "Closing Price"))
}


#Q3.1.14---------------------------------------------------------------------------

# for question 10 I have mentioned comments and x, y, z assignment is clearer now.

# Install and load necessary packages
install.packages("plotly")
library(plotly)

# Read the CSV data from the provided URL
data <- read.csv("https://shorturl.at/nouyJ")

# Create a 3D scatter plot using plot_ly
plot_ly(
  data,
  x = ~ShoulderToWaist,
  y = ~HeadCircumference,
  z = ~TotalHeight,
  type = "scatter3d",
  mode = "markers",
  marker = list(size = 6, color = "#4682B4", opacity = 0.5)
) %>%
  # Set layout options for the 3D scene
  layout(scene = list(aspectmode = "cube"))


# For Q7, the readability is improved by breaking it down and the comments are added.
# broke lines, consistency in indentation, labels are descriptive and capitalization is also
# consistent.

# Load necessary packages
library(ggplot2)

# Load dataset
data("cars")

# Create a ggplot with point and smoothed line
ggplot(cars, aes(x = speed, y = dist)) +
  geom_point(color = "black", size = 0.5) +
  geom_smooth(
    method = 'loess',
    formula = 'y ~ x',
    color = 'blue',
    se = TRUE,
    fill = 'darkgray',
    size = 0.5
  ) +
  labs(x = "Speed", y = "Distance") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "lightgray"))


#Q3.1.15--------------------------------------------------------------------------

#The code compares the lengths of two strings, "dog" and "cat," and returns TRUE 
#if they have the same length, otherwise FALSE.


# Reasons for poor style.

# Inconsistent Assignment: a <- "dog" uses <- for assignment, while b = "cat" uses =.
# Inconsistent naming: Variable names lxx, lyy, and v are not descriptive, making the code less readable.
# Redundant Comparison: The line v <- lxx == lyy is redundant; the logical result of the comparison is already stored in the variable v.
# Unnecessary Function Definition:The function v is unnecessary; the code could achieve the same result without defining a function
# Lack of Comments

#Q3.1.16----------------------------------------------------------------------------

matrix(c(1, 8, 21, 2, 10, 24, 3, 12, 27), nrow = 3, byrow = TRUE)

#Q3.1.17----------------------------------------------------------------------------

#In Mac you can format the code by highlighting it and pressing 
# Command + Option + I

#Q3.1.18--------------------------------------------------------------------------

install.packages("magick")
library(magick)

pic1 <- image_read("/Users/swasti/Desktop/Pic1.png")
pic1_with_text <- image_annotate(pic1, "Our energy before starting programming course", 
                                 location = "center", size = 60, color = "white", font = "Arial-Bold", gravity = "north")

output_path <- "/Users/swasti/Desktop/Pic1_meme.png"
image_write(pic1_with_text, output_path)
browseURL(output_path)
#------------------------------------------------------------------------------------


install.packages("knitr")

library(knitr)

knitr::include_graphics("/Users/swasti/Desktop/Pic1_meme.png")







