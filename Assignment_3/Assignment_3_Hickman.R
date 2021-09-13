# YOUR REMAINING HOMEWORK ASSIGNMENT (Fill in with code) ####

library(tidyverse) # loads tidyverse
dat <- iris # created and named so that I still have access to unmodified iris 

# 1.  Get a subset of the "iris" data frame where it's just even-numbered rows

seq(2,150,2) # here's the code to get a list of the even numbers between 2 and 150

# ss is the name of the subset; iris denotes the df; seq() gives a selection for row specs; columns are blank. This is now it's own viewable data frame.
ss <- iris[(seq(2,150,2)), ]
print(ss)

# 2.  Create a new object called iris_chr which is a copy of iris, except where every column is a character class

# Being my first attempt, this method seems too simple and non-specific.
iris_chr <- as.character(iris[ , 1:5])
iris_chr <- as.character(iris_chr)
class(iris_chr)

# On second attempt, this set of functions (while incredibly redundant) reassigns character class to previous numeric and factor classes.
# Concatenation labelled "iris_chr" prints as characters and is identified as such by sapply().
iris$Sepal.Length <- as.character(iris$Sepal.Length)
iris$Sepal.Width <- as.character(iris$Sepal.Width)
iris$Petal.Length <- as.character(iris$Petal.Length)
iris$Petal.Width <- as.character(iris$Petal.Width)
iris$Species <- as.character(iris$Species)

iris_chr <- c(iris$Sepal.Length, iris$Sepal.Width, iris$Petal.Length, iris$Petal.Width, iris$Species)

print(iris_chr)
class(iris_chr)
sapply(iris, class)

# 3.  Create a new numeric vector object named "Sepal.Area" which is the product of Sepal.Length and Sepal.Width

# Before completing this step, I had to convert Sepal.Length and Sepal.Width back to numeric class.
iris$Sepal.Length <- as.numeric(iris$Sepal.Length)
iris$Sepal.Width <- as.numeric(iris$Sepal.Width)

# 1D vector representing the product of Sepal.Length and Sepal.Width
Sepal.Area <- as.vector(iris$Sepal.Length * iris$Sepal.Width)
class(Sepal.Area)

# 4.  Add Sepal.Area to the iris data frame as a new column

# cbind() combines specified vectors to a df, with df, followed by vector data object
iris <- cbind(iris, Sepal.Area)
iris

# 5.  Create a new dataframe that is a subset of iris using only rows where Sepal.Area is greater than 20 
# (name it big_area_iris)

# iris1 is the subset of iris containing Sepal.Area. This function specifies only rows over 20. 
big_area_iris <- data.frame(iris[Sepal.Area > 20, ])
dim(big_area_iris)

# 6.  Upload the last numbered section of this R script (with all answers filled in and tasks completed) 
# to canvas
# I should be able to run your R script and get all the right objects generated
