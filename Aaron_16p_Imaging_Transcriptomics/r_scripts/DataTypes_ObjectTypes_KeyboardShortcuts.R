#DATA types ----
# Numeric: Numbers that have a decimal value or are a fraction in nature have a data type as numeric.
num <- 1.2
print(num)

# Integer: Numbers that do not contain decimal values have a data type as an integer. However, to create an integer data type, you explicitly use as.integer()
#and pass the variable as an argument.
int <- as.integer(2.2)
print(int)

# Character: As the name suggests, it can be a letter or a combination of letters enclosed by quotes is considered as a character data type by R. It can be alphabets or numbers.
char <- "datacamp"
print(char)

# Logical: A variable that can have a value of True and False like a boolean is called a logical variable.
log_true <- TRUE
print(log_true)

# Factor: They are a data type that is used to refer to a qualitative relationship like colors, good & bad, course or movie ratings, etc. They are useful in statistical modeling.
# To achieve this, you will make use of the c() function, which returns a vector (one-dimensional) by combining all the elements.
fac <- factor(c("good", "bad", "ugly","good", "bad", "ugly"))
fac
print(fac)
class(fac)
# The fac factor has three levels as good, bad, and ugly, which can be checked using the keyword levels, and the type of level will be a character.
levels(fac)
nlevels(fac)

#To check the data type of an object
# class(object_name)
variable1 <- "cabbage"
class(variable1)

# To check all the variables or objects that have been defined by you in the working environment
# ls()
ls()



#Types of Objects ----
#Lists ----
#A list can contain elements of various data types and is often known as an ordered collection of values
#It can contain vectors, functions, matrices, and even another list inside it
# Lists in R are one-indexed, which means the index starts with 1
lis1 <- 1:5  # Integer Vector
lis1

lis2 <- factor(1:5)  # Factor Vector
lis2

lis3 <- letters[1:5]  # Character Vector
lis3

combined_list <- list(lis1, lis2, lis3)
combined_list

# Let's access each vector in the list separately. To achieve this, you will use double square brackets since the three vectors are placed on one level inside the list. 
combined_list[[3]]

#Now, let us try to access the fifth element from the third vector, which gives the letter e.
combined_list[[3]][5]

#Finally, let's try to flatten the list. One important thing to remember is that since combined_list is a combo of character and numeric datatype, the
#character data type will get the precedence and the data type of complete list will become a character
flat_list <- unlist(combined_list)
class(flat_list)
flat_list
length(flat_list)

#Vectors ----
#Vectors are an object which is used to store multiple information or values of the SAME data type
#A vector can not have a combination of both integer and character
#A vector can be created with a function c(), which will combine all the elements and return a one-dimensional array.
grades <- c(88,65,90,40,65)
class(grades)
length(grades)
#Now, let's try to access a specific element by its index.
grades[4]
grades[6] #returns NA since there is no sixth element in the vector


#Slicing: Similar to Python, the concept of slicing can be applied in R as well.
# Let's try to access elements from second to fifth using slicing.
grades[2:5]

char_vector <- c("a", "b", "c")
print(char_vector)
class(char_vector)
length(char_vector)

#If we create a vector that has both numeric and character values, the numeric values will get converted to a character data type.
char_num_vec <- c(1,2, "a")
char_num_vec


vec <- c(1:1024)
#try to access the middle and the last element. To do that, you will use the length function.
#The code is accessing the last element of a vector called "vec".
vec[length(vec)]
length(vec)
vec[length(vec)/2]

#To create a vector of odd numbers, you can use the function seq, which takes in three parameters: start, end, and step size.
seq(1,10, by = 2)


# A vector is a one dimensional array that can hold numeric, integer, character, or logical data types.
# There are multiple ways to create a vector:
myvec <- c(1,2,3,4,5,6,7,8)
# The seq function generates sequences of numbers given a starting and stopping point. For example,
# seq(1,8) is to generate a sequence numbers (a vector) from 1 to 8.
myvec <- seq(1,8)
myvec <- 1:8
myvec


#To pull out certain elements from a vector, can use the index. The index starts from 1.
myvec[2]

# If we want more than one elements, we can use a vector of index. For example, if we want the first and third elements
myvec[c(1,3)]

# If we want the first, second and third elements:
myvec[c(1,2,3)]
myvec[1:3]

# We can use conditions to take out elements. For example, we want elements that larger than 5:
myvec[myvec>5]

# "summary()" calculates min, max, mean and quantiles of a numeric vector.
summary(myvec)

mean(newvec2)
median


#Matrix ----
#a matrix is used to store information about the same data type. However, unlike vectors, matrices are capable of holding two-dimensional information inside it.
#The syntax of defining a matrix is:
# matrix(data, nrow, ncol, byrow, dimnames)
M <- matrix(vector, nrow=r, ncol=c, byrow=FALSE, dimnames=list(char_vector_rownames, char_vector_colnames))
#byrow=TRUE signifies that the matrix should be filled by rows. byrow=FALSE indicates that the matrix should be filled by columns (the default).
#dimnames is an optional argument that can be used to provide names to the rows and columns of the matrix.

#Let's quickly define a matrix M of shape $2\times3$.
M = matrix( c('AI','ML','DL','Tensorflow','Pytorch','Keras'), nrow = 2, ncol = 3, byrow = TRUE)
print(M)

#Let's use the slicing concept and fetch elements from a row and column.
M[1:2,1:2] #the first dimension selects both rows while the second dimension will select elements from 1st and 2nd column


# another example of making a matrix
y <- matrix(1:9,ncol=3)

# To get elements from a matrix, use the format [row,column]
#Take out the element in the first row and first column:
y[1,1]
# Take out the elements in the second column:
y[,2]

# to see the dimensions of a matrix
dim(y)

# You can use the attach() function to Attach Set of R Objects to Search Path, to make referring to individual columns easier

?attach

#Dataframes ----
#A data frame is a two-dimensional data structure that can store data of different types. It is similar to a matrix, but it has a header row that can store column names.
#Data frames are a more generalized form of a matrix. It contains data in a tabular fashion. 
#The data in the data frame can be spread across various columns, having different data types. The first column can be a character while the second column can be an integer,
#and the third column can be logical.
#The variables or features are in columnar fashion, also known as a header,
#the observations are in rows with the first element being the name of the row followed by the actual data, also known as data rows

#Dataframes can be created using the data.frame() function.
dataset <- data.frame(
  Person = c("Aditya", "Ayush","Akshay"),
  Age = c(26, 26, 27),
  Weight = c(81,85, 90),
  Height = c(6,5.8,6.2),
  Salary = c(50000, 80000, 100000)
)

dataset
class(dataset)

nrow(dataset) # this will give you the number of rows that are there in the dataset dataframe
ncol(dataset) # this will give you the number of columns that are there in the dataset dataframe

df1 = rbind(dataset, dataset) # a row bind which will append the arguments in row fashion. So it adds the other dataframe to the first, by creating additional rows
df1

df2 = cbind(dataset, dataset) # a column bind which will append the arguments in column fashion.
df2

#Let's look at the head function which is very useful when you have millions of records and you want to look at only the first few rows of your data.
#Similarly, the tail function will output the last few rows of your data.
head(df1,3) # here only three rows will be printed

str(dataset) #this returns the individual class or data type information for each column.

#Now let's look at the summary() function, which comes in handy when you want to understand the statistics of your dataset.
#it divides your data into three quartiles, based on which you can get some intuition about the distribution of your data. It also shows if there are any missing values in your dataset.
summary(dataset)


#Converting between object types ----
#You can convert between object types using as. functions

#Dataframe to matrix
#You can convert a dataframe to a matrix using the as.matrix() function.
#Let's convert the dataset dataframe to a matrix.
dataset_matrix <- as.matrix(dataset)
dataset_matrix
class(dataset_matrix)

#matrix to dataframe
#You can convert a matrix to a dataframe using the as.data.frame() function.
#Let's convert the dataset_matrix matrix to a dataframe.
dataset_df <- as.data.frame(dataset_matrix)
dataset_df
class(dataset_df)

#Selecting elements from a dataframe ----
#You can select elements from a data frame with the help of square brackets [ ].
#By using a comma, you can indicate what to select from the rows and the columns respectively. For example:
#my_df[1,2] selects the value at the first row and second column in my_df.
#my_df[1:3,2:4] selects rows 1, 2, 3 and columns 2, 3, 4 in my_df.
#If you don't want to look up the column name, you can just put in the column title
#planets_df[1:3,"type"]
#You can use the subset function to select rows based on a condition.
#subset(dataframe, subset = some_condition)
#i.e. subset(planets_df, subset = planets_df$diameter<1)

#Sorting elements in a dataframe
#With the order() function, you can sort your data according to a certain variable in the dataset.
#The order function can also be applied to a vector
#Example: order(planets_df$diameter) will sort the planets_df dataframe according to the diameter column.
#This will return a vector of indeces of the dataframe sorted by diameter
#You can use this vector to sort the dataframe
#store the vector as a new object: sorted_index <- order(planets_df$diameter)
#sort the dataframe: planets_df[sorted_index,]



#KEYBOARD SHORTCUTS ----
#Ctrl Enter: Run the current line of code, or a set of selected lines of code (Command Enter on a Mac)
#Ctrl Enter: Run the current line of code, or a set of selected lines of code (Command Enter on a Mac)
#Ctrl Shift Enter: Run the current script (Command Shift Enter on a Mac)
#Ctrl L: Clear the console (Command L on a Mac)
#Ctrl Shift M: Insert a new cell below the current cell (Command Shift M on a Mac)
#Ctrl Shift N: Insert a new cell above the current cell (Command Shift N on a Mac)

#Alt - (minus sign): Shortcut for the arrow assignment operator <- (Option - on a Mac)
#Ctrl Shift C: Comment out a line of code (Command Shift C on a Mac)
#Ctrl Shift M: The pipe operator %>% used in the tidyverse (introduced later)



#Factors
#Factors are used to represent categorical data. They can be ordered or unordered.
#Factors are stored as integers, and have labels associated with these unique integers.
#Factors are useful for statistical analysis and for plotting.
#Factors are created using the factor() function.
#The factor() function takes in a vector as an argument, and returns a factor.
#The levels argument in the factor() function is used to specify the unique values in the factor.
#The levels are ordered alphabetically by default, but you can specify the order using the levels argument.
#The levels of a factor can be accessed using the levels() function.
#The nlevels() function returns the number of levels in a factor.
#The as.factor() function can be used to convert a vector to a factor.
#The droplevels() function can be used to remove unused levels from a factor.
#The fct_reorder() function from the forcats package can be used to reorder the levels of a factor based on the values of another variable.



library(tidyverse)
?facet_wrap
?geom_text

