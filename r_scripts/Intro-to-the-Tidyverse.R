#The tidyverse package works ONLY with dataframes
# It contains 9 R packages: readr, tibble, ggplot2, dplyr, tidyr, lubridate, purrr, stringr, and forcats

#In this script, I am going through the datacamp "Introduction to the Tidyverse" course

# First, I am going to lead in the data that we are going to work withL the gapminder dataset which shows facts about countries like GDP and life expecancy over time
library(gapminder)
#We are first going to be working with the dplyr package, so I will load that in as well
library(dplyr)

#first let's display the gapminder dataset in ther terminal
gapminder
#We see that this dataframe is a tibble, and has 1704 rows and 6 columns as indicated by "A tibble: 1,704 × 6"


#The rest of this chapter we will learn about the verbs of dplyr

# Filter verb ----
#The filter verb is used to filter rows based on a condition
#For example, if we want to filter the gapminder dataset to only show rows where the year is 2007 and the country is Australia, we would use the following code:
gapminder %>%
  filter(year == 2007, country == "Australia")

# Arrange verb ----
#The arrange verb sorts a table based on a variable, in ascending or descending order

gapminder %>%
  arrange(gdpPercap)

#The default is sorting by ascending order, but you can specify descending order by using the desc() function
gapminder %>%
  arrange(desc(gdpPercap))

#You can filter and then arrange, to find the highest gdp by year
gapminder %>%
  filter(year == 2007) %>%
  arrange(desc(gdpPercap))


# Mutate verb ----
#The mutate verb can be used to change a variable, like this, where the variable to be changed is on the left of the equal sign and the new value is on the right
gapminder %>%
  mutate(pop = pop/1000000)

#You can also use the mutate verb to add a new variable to a dataframe
#Let's say we want to know the total gdp. To calculate this, we want to multiple the gdp per capita by the population
gapminder %>%
  mutate(gdp = gdpPercap * pop)


#Now let's combine everything.
#Supose we want to find the countries with the highest gdp in 2007
gapminder %>%
  mutate(gdp = gdpPercap * pop) %>%
  filter(year == 2007) %>%
  arrange(desc(gdp))


# Summarize verb ----
#The summarize verb is used to summarize a dataset
#For example, if we want to find the average life expectancy in 2007, we would use the following code:
gapminder %>%
  filter(year == 2007) %>%
  summarize(avgLifeExp = mean(lifeExp))


# Group_by verb ----
#The group_by verb is used to group a dataset by a variable
#For example, if we want to find the average life expectancy by year, we would use the following code:
gapminder %>%
  group_by(year) %>%
  summarize(avgLifeExp = mean(lifeExp))

#You can also group by multiple variables
#For example, if we want to find the average life expectancy by year and continent, we would use the following code:
gapminder %>%
  group_by(year, continent) %>%
  summarize(avgLifeExp = mean(lifeExp))



# Separate verb ----
#The separate verb is used to separate a single variable into multiple variables
#For example, if we want to separate the country variable into two variables, continent and country, we would use the following code:
gapminder %>%
  separate(country, c("continent", "country"), sep = " ")

# the rename verb ----
# the rename () verb changes the names of individual variables using "new_name" = "old_name" syntax
gapminder %>%
  rename(gdp_per_capita = gdpPercap)


# Relocate verb ----
#the relocate verb is used to move a variable to a new position in the dataframe
#For example, if we want to move the year variable to the first position in the dataframe, we would use the following code:
gapminder %>%
  relocate(year, .before = 1)
#You can also use variable names to move things around. for example, you could move the year variable to after the country variable using this code
gapminder %>%
  relocate(year, .after = "country")







#Set Operations in dplyr ----
# Perform set operations using the rows of a data frame.

# intersect(x, y) finds all rows in both x and y.
intersect(x, y)
# union(x, y) finds all rows in either x or y, excluding duplicates.
union(x, y)
# union_all(x, y) finds all rows in either x or y, including duplicates.
union_all(x, y)
# setdiff(x, y) finds all rows in x that aren't in y.
setdiff(x, y) 
# symdiff(x, y) computes the symmetric difference, i.e. all rows in x that aren't in y and all rows in y that aren't in x.
symdiff(x, y)
# setequal(x, y) returns TRUE if x and y contain the same rows (ignoring order).
setequal(x, y)




# Data visualization with ggplot2 ----



# Conditional statements in R ----
# Conditional statements are used to perform different actions based on different conditions.
# The basic structure of an if statement is  if(condition) {expression}
# For example, if we want to print "Hello, World!" if x is greater than 5, we would use the following code:
x <- 10
if(x > 5) {
  print("Hello, World!")
}

# We can also use else statements to perform a different action if the condition is not met.
# The basic structure of an if/else statement is  if(condition) {expression} else {expression}
# For example, if we want to print "Hello, World!" if x is greater than 5 and "Goodbye, World!" if x is less than or equal to 5, we would use the following code:
x <- 3
if(x > 5) {
  print("Hello, World!")
} else {
  print("Goodbye, World!")
}


# We can also use else if statements to check multiple conditions.
# The basic structure of an if/else if/else statement is  if(condition) {expression} else if(condition) {expression} else {expression}
# For example, if we want to print "Hello, World!" if x is greater than 5, "Goodbye, World!" if x is less than or equal to 5 and greater than 0, and "Goodnight, World!"
#if x is less than or equal to 0, we would use the following code:
x <- -1
if(x > 5) {
  print("Hello, World!")
} else if(x > 0) {
  print("Goodbye, World!")
} else {
  print("Goodnight, World!")
}

#Note that the else statement must be on the same line of code as the ending curly bracket for the if statement, or R will not understand that the else statement
#goes with that if statement



#Plotting Data ----

# You can plot 2 variables with the plot() function
# the X axis is the first variable in the plot function, and the Y axis is the second variable in the plot function
plot(gapminder$gdpPercap, gapminder$lifeExp)


# We can creare a regression line eith the lm function
regline <- lm(gapminder$gdpPercap ~ gapminder$lifeExp)
#Now let's add the regression line to the plot
abline(regline,col=2)
#"col=2" here means use the color value 2, which is red, to plot the result.

#This will give us the summary of the regression line, including the coefficients and the R-squared value
summary(regline)

#Display data in a boxplot.
boxplot(gapminder$lifeExp)

#Display data in a histogram
hist(gapminder$lifeExp, col="blue")

#from bioinformatics class
#You can see a bell-shaped distribution for geneA and geneB, but not geneC or geneD. It is important to check 
#the distribution of the data before you apply the t-test on the data. Because the t-test assumes the data are
#normally distributed. There are some statistical methods to rigorously test if the data are normally distributed 
#or not. However, in practice, it is usually OK to just plot the data and check if the data are bell-shaped.




# Pivoting dataframes ----
#Pivoting dataframes is a way to reshape dataframes so that they are easier to work with
#There are two main functions for pivoting dataframes in R: pivot_longer() and pivot_wider()
# pivot_longer() is used to make a dataframe longer and pivot_wider() is used to make a dataframe wider
#For example, if we want to pivot the gapminder dataframe so that the columns are year, country, and gdpPercap, and the rows are the values of
# lifeExp, we would use the following code:
gapminder %>%
  pivot_longer(cols = c(lifeExp, pop, gdpPercap), names_to = "variable", values_to = "value")


#Here is an example of using pivot_longer from my DIYtranscriptomics class, in Step6_module.R from dir2
module.assign.pivot <- pivot_longer(module.assign.df, # dataframe to be pivoted
                                    cols = 1:2307, # column names to be stored as a SINGLE variable
                                    names_to = "geneID", # name of that new variable (column)
                                    values_to = "module") # name of new variable (column) storing all the values (data)


