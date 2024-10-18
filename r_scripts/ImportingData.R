# read a data file in from your computer using the read_delim function from the readr package 
# typically I will just load tidyverse because it contains the readr package, among other things

#Make sure you're in the right directory
#to see what directory you are in, use getwd()
getwd()
#You can also see what working directory you are in by looking at the console, the top line, after it says what version of R you're using

#To change your working directory, use setwd() and put the path in the parenthesis
#You can easily change working directories by going to session -> set working directory -> choose directory
setwd()

#Importing data into R
#Use the read_delim() function from the readr package to read in a data file
#This function reads a delimited file (including CSV (comma separated file) and TSV (tab separated file)) into a tibble
#You can also use read_csv, read_tsv, read_csv2, read_table, read_fwf, read_log, read_fortran
read_delim()
my.dataframe <- read_delim("swine_study.txt")

#As of right now, the data I imported to R is not saved anywhere. It's just an object in the environment. 
#To save it, you can use the write_tsv() function from the readr package
write_tsv()
write_tsv(my.dataframe,"test.txt")

#A tibble is just the tidyverse version of a dataframe

myNumericVector <- c(2,4,8,3,1) #notice I don't use spaces in variable names!
my_character_vector <- c("have", "a", "very", "very", "good", "day!")
#When you have a dataframe, you can access a single column using the dollar sign followed by the column you want to access
#And you can store them in a separate vector or whatever if you want to
my.matrix <- as.matrix(read_delim("matrix.txt")) # notice how we can nest functions
# dataframe
my.dataframe # we already created this object in the section above
# we can access any column of our dataframe using the '$' operator
my.dataframe$SampleID
# list
myList <- c(my.dataframe, myNumericVector, my_character_vector)
# we can evaluate data structures with logical expressions
myLogical <- is.numeric(myNumericVector)
myLogical <- is.numeric(my_character_vector)
# you can always check what type of data an object is either by looking in the Environment browser, or by:
class(myNumericVector)


#Subsetting dataframes  in R
my.dataframe$current_antibiotics == "Yes"
no_antibiotics <- my.dataframe[my.dataframe$current_antibiotics == "Yes", ]






