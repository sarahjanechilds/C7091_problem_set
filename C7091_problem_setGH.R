## C7091 Professional Skills for Data Science ####
## Miss S-J Childs
## Student ID: 24370500
## Assignment 1: Problem Sets
## Source: https://c7091.github.io/website/problem-sets.html
## 2024-10-23

## CONTENTS ####
## 00 Setup
## 01 Question One: Plotting and subsets with the mpg data
## 02 Question Two: Pseudocode
## 03 Question Three: Understanding functions
## 04 Question Four: Working with matrices
## 05 Question Five: ANOVAs code
## 06 Question Six: F-Statistic
## 07 Question Seven: Write your own questions
## 08 Question Eight: Add nested lists and indexing to your questions

## 00 Setup ####

# Install packages and load libraries
install.packages("emmeans")
install.packages("pwr")
install.packages("tidyverse")
library(emmeans)
library(pwr)
library(tidyverse) 

# Set working directory

getwd() # All good

## 01 Question One ####

## "Make the plot and axis labels match"

data(mpg) # read in

colnames(mpg) # Typos in original code, so double-checking variable names

# Corrected code
plot(mpg$displ, mpg$hwy, col = as.factor(mpg$year), 
     main = "Displacement by Highway Miles per Gallon", # added title
     xlab = "Displacement (L)", 
     ylab = "Highway (MPG)", pch = 19) # added measurement units
legend("topright", legend = unique(mpg$year), 
       col = unique(as.factor(mpg$year)), pch = 19)

## "Use base R to filter the data where cyl equals 8"

# Corrected code
subset(mpg, cyl == 8) 
# 8 is a numeric, not a character. == for equality, = means assignment

## "Extend code to calculate the proportions of manual and automatic shift cars"

# Created a table of transmission types
trans_table <- table(mpg$trans) # shows the frequency of different types

# Calculated the proportions
help("proportions") # entries of x, divided by the appropriate marginal sums

trans_proportions <- proportions(trans_table) # but is this helpful?
View(trans_proportions)

# Visualization of transmission types
barplot(trans_table, main ="Transmission Types in the mpg Dataset", 
        xlab ="Transmission Type",
        ylab ="Frequency", col="blue") # now easier to comprehend proportions



## Question Two ####

## "Write pseudocode steps for calculating the volume of a cylinder."

# Step One: Look up the formula for cylinder volume
# Volume = π * radius^2 * height

# Step Two: State the height and radius for cylinder
height <- 3.2
radius <- 5.5

# Step Three: State value of pi to 3 decimal places
print(pi) # checking
pie <- round(pi, 3)
print(pie) # checking

# Step Four: Use formula to calculate 
cy_volume <- pie * radius^2 * height

# Step Five: Round the volume to 2 decimal points
cy_volume_rounded <- round(cy_volume, 2)

# Step Six: Show the answer
print(cy_volume_rounded)


## Question Three ####

## "What value is required for the 'd' argument in the pwr.t.test() function?"

help("pwr-package")
help("pwr.t.test")

# A statistical method known as power analysis. 
# My understanding of power analysis as having four core elements:
# Statistical power == detecting an effect if there's one
# Significance aka 'alpha' == the level needed (0.05) to reject the null hypothesis
# Sample size == the number of observations
# Effect size == the difference between the mean divided by the combined
# standard deviation, aka 'Cohen's d' and the 'd' argument in pwr.t.test function.
# The 'd' argument is the mean difference / standard deviation difference

# Example code
mean_diff <- mean("hypothetical data B" - "hypothetical data A")
sd_diff <- sd("hypothetical data B" - "hypothetical data A")
d <- mean_diff / sd_diff

## Question Four ####

## "What is the role of the set.seed() function in the question context?"

help("set.seed")
# It's a random number generator and helps with reproducibility.
# The question code chunk has random sampling, but you don't 
# want the results to change each time you run the code, so 
# you set a seed.



## "Why does calculating the mean of the copied original matrix return a 
## numeric value while the mean of the same matrix when missing values are 
## filled from the list returns NA?"

# In R you need to be careful when making copies. Making a copy of the matrix,
# but then altering the original will NOT change the copied version. This is
# because when copying a matrix, R creates a new independent copy of the data.

## "Fix the code such that mean_of_original_copy and mean_matrix are equal"

# Corrected code

set.seed(123)
matrix_with_na <- matrix(sample(c(NA, 1:9), 9, replace = TRUE), nrow = 3)
copy_of_original<-matrix_with_na

matrix_with_na[1,2] <- NA
matrix_with_na[2,3] <- NA
matrix_with_na[3,1] <- NA

print(copy_of_original)
print(matrix_with_na)

list_with_values <- as.list(as.numeric(matrix(1:9, nrow = 3)))
# originally as.character, must be numeric or logical

print(list_with_values)

for (i in 1:3) {
  for (j in 1:3) {
    if (is.na(matrix_with_na[i,j])) {
      matrix_with_na[i,j] <- list_with_values[[(i-1)*3 + j]] 
    }
  }
}
print(matrix_with_na)

# Now calculating the mean of the matrices
mean_matrix <- mean(matrix_with_na)
mean_of_original_copy <- mean(copy_of_original)
print(mean_of_original_copy)
print(mean_matrix)

# Both matrices now match at 4.333333

## Question Five ####

## "Explain why this function is not working and return the working equivalent."

data("iris")

# Original code leads to "error: arguments imply differing number of rows: 0, 3"

names(iris) # Typos in original, checking names

# Corrected code
run_anova_posthoc <- function() {
  
  anova_result <- aov(Sepal.Length ~ Species, data = iris)
  
  posthoc_result <- emmeans(anova_result, pairwise ~ Species)
  
  comparisons <- as.data.frame(summary(posthoc_result$contrast)) # accessibility
  
  print(comparisons)
  
  comparisons_df <- data.frame(Species = comparisons$contrast, # NOT $species
                               estimate = comparisons$estimate,
                               p.value = comparisons$p.value)
  
  return(comparisons_df)
}

run_anova_posthoc() # Now it's working!

## "Interpret what the estimate means and rank the species"

# I interpret the estimate to mean the difference in the Sepal.Length
# means between the species pairs, so if positive that's a higher mean, but 
# the negative indicates lower mean.

emmeans(anova_result, ~ Species) # look at emmean column to rank


## Question Six ####

## "Using the iris dataset conduct an anova of sepal length and report F statistic"

# dataset already loaded

# Performing anova
iris_anova <- aov(Sepal.Length ~ Species, data = iris)

# Summarise the results
anova_summary <- summary(iris_anova)

# See the results
print(anova_summary) # format is 'classic' anova table style

# Let's look at the F statistic
f_statistic <- anova_summary[[1]]["Species", "F value"]

print(f_statistic)

# A longer way to code for anova

n.groups <- ncol(iris) # we want to know how many groups we have
n.per.group <- vector(mode = "integer", length = ncol(iris))














## Question Seven ####

## "Write your own questions"

# Q"3"
# In your own words, what is the 'quiet' argument in the knit() function in
# the {knitr} package? Show the code involved including  any appropriate
# comment code required to answer this question.

library(knitr)
help(knitr)
help(quiet)







