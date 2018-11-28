#~~~~~~~~~~~~~~~~~~~~~~~~~#
# CREATING SIMPLE OBJECTS #
#~~~~~~~~~~~~~~~~~~~~~~~~~#
# Save a value as an object. Note you may use either "<-" or "=" to assign values to a new object 
myNumber <- 5 
myNumber2 = 7

# See contents of an object by entering its name 
myNumber 
myNumber2
## R is case sensitive!
mynumber
MyNumber

# Use object in further procedures
myNumber - 3

# Existing objects can be updated by assigning it a new value. BE CAREFUL -- this will overwrite the object with the new assigned value!
myNumber
myNumber <- myNumber - 3
myNumber

# Copy objects
myNumber3 <- myNumber
myNumber3

#~~~~~~~~~~~~~~~~~~~~~~~~~~#
# CREATING COMPLEX OBJECTS #
#~~~~~~~~~~~~~~~~~~~~~~~~~~#
### Vectors ###-- a sequence of values assigned to a single object
# c() function -- combine numeric values or existing objects into a vector. Note c() coerces all arguments to the same class (e.g., all numeric, all character, all logical, etc.)
v1 <- c(1,2,3,1,1)
v1
class(v1)

v2 <- c("A","B","C")
v2
class(v2)

v3 <- c(T,F,T,F)
v3
class(v3)

v4 <- c(1, "A", T)
v4
class(v4)

v5 <- c(myNumber, myNumber2, myNumber3)
v5

v6 <- c(myNumber, 3, 4, 5)
v6

# ":" -- create continous sequence of numbers that increase by 1
v7 <- 1:10
v7

# seq() function -- create sequences of numbers that increase by specified amount
v8 <- seq(from = 0, to = 1, by = .1)
v8

# rep() function -- create vector of repeated values or sequence of values
## To repeat same values, use "times" argument
v9 <- rep(1, times = 5)
v9

v10 <- rep(1:3, times = 5)
v10

## To repeat each value of a vector, use "each" argument
v11 <- rep(1:3, each = 5)
v11

## Often useful to nest rep() functions to create particular sequences
v12 <- rep(rep(1:3, each = 3), times = 2)
v12

v13 <- rep(rep(1:3, times = 3), each = 2)
v13


### Matrix ### -- 2-dimensional object with values organized into unnamed rows and columns
# matrix() -- creates a matrix from a given set of values.
## Number of values supplied is enough to fill entire matrix
mat1 <- matrix(data = 1:16, nrow = 4, ncol = 4)
mat1

## If number of values supplied is NOT enough to fill entire matrix, values will be recycled BUT you will receive a warning
matrix(data = 1:10, nrow = 4, ncol = 4)

## If a single value is supplied, same number will be used for all cells
mat2 <- matrix(data = NA, nrow = 4, ncol = 4)
mat2

## By default, R populates values by column; to populate by row, use "byrow = T" argument
matrix(data = 1:16, nrow = 4, ncol = 4, byrow = T)

## Appending a new row or column to matrix
mat3 <- cbind(mat1, 17:20) # appends one new column to end of matrix; use cbind(17:20, mat1) to add column to beginning
mat3

mat4 <- rbind(mat1, rep(100,4)) ## appends one new row to end of matrix; use rbind(rep(100,4), mat1) to add row to beginning
mat4

### Data frames ### -- 2-dimensional object with values organized into named rows and columns
# data.frame() -- creates a data frame from a given set of VECTORS
df1 <- data.frame("A" = 1:5, "B" = 6:10, "C" = 11:15)
df1

# Each column of data in a data frame should have the same length
data.frame("A" = 1:10, "B" = 1:3, "C" = 1:3)
data.frame("A" = 1:10, 
           "B" = c(1:3, rep(NA, times = 7)), 
           "C" = c(1:3, rep(NA, times = 7)))

# Naming data frames
names(df1)
names(df1) <- c("Var1", "Var2", "Var3")
df1

# Turning matrices into data frames
as.data.frame(matrix(data = 1:16, nrow = 4, ncol = 4))


### Arrays ### -- multidimensional objects with values arrayed
# array() -- creates an array with dimensions given by "dim()" argument. Number of values in dim() determines number of dimensions, value determines size of dimension
a1 <- array(data = 1:27, dim = c(3,3,3)) # Creates a 3x3x3 array
a1

a2 <- array(data = 1:24, dim = c(2,3,4)) # Creates a 2x3x4 array
a2


### Lists ### -- objects that can hold any type of object or combination of complex objects
# list() -- creates a list object from a given set data or existing objects. Each object in a list is referred to as a list element. Number of elements is determined by the number of objects supplied to the list
l1 <- list(1, 1:10, mat1, df1) # Creates a list where first element contains the number 1, second element contains the vector 1:10, third element contains the matrix mat1, and fourth elemetn contains the data frame df1
l1

# List elements can be named by supplying names during the list construction
l2 <- list("dat1" = 1, 
           "dat2" = 1:10, 
           "dat3" = mat1, 
           "dat4" = df1)
l2$dat1
l2$dat2

# Lists can be nested (i.e., list of lists)
l3 <- list(l2, l2) # Creates a 2-level list. Top level contains a list with the objects (l2, l2); each l2 contains a list of other objects
l3
## Nested lists can be named
l4 <- list("Person1" = l2,
           "Person2" = l2)
l4$Person1

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# WORKING WITH COMPLEX OBJECTS #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
### Determining size of object ###
# Vectors
length(v1) # return number of values stored in a vector
str(v1) # provides details of structure


# Matrix
length(mat1) # return total number of values in a matrix
dim(mat1) # return number of rows and columns in a matrix
str(mat1) # provides details of structure


# Data frame
length(df1) # return number of unique vectors (i.e., columns) in a data frame
dim(df1) # return number of rows and columns in a matrix
str(df1) # provides details of structure


# Array
length(a1) # return total number of values in an array
dim(a1) # returns size of each array dimension
str(a1) # provides details of structure


# List
length(l1) # return number of elements in a list
str(l1) # provides details of structure



### Selecting/indexing values from a known position ###
# Vectors - use "[x]" where x = position of value to be selected
v1[5] # returns value in the 5th position in vector


# Matrix 
## To select single value, use "[x,y]" where x = row number and y = column number of value to be selected
mat1[2,3] # returns value in row 2, column 3 position of matrix

## To select ALL values of a single row, supply ONLY a row number
mat1[2,] # returns all values in row 2 of matrix

## To select ALL values of a single column, supply ONLY a column number
mat1[,2] # returns all values in column 2 of matrix


# Data frame
## To select single value, use "[x,y]" where x = row number and y = column number of value to be selected
df1[1,2] # returns value in row 1, column 2 position of data frame
## Can also select column using $ and then ask for a value in a specific position of that vector using []
df1$Var2[2] # returns 2nd value in the Var2 column

## To select all values of a single row, supply ONLY a row number
df1[2,] # returns all values in row 2 of data frame

## To select all values of a single column, supply column name using $, a column number, or column name in []
df1$Var2 # returns all values from column named "Var2" in data frame
df1[,"Var2"] # returns all values from column named "Var2" in data frame
df1[,2] # returns all values in column 2 of data frame (Var2 column)


# List
## To select an entire list element, use [x] or [[x]] where x = list element to select. IMPORTANT: using [x] returns element as class = list, using [[x]] returns only the element
l1[3] # returns 3rd element of list
class(l1[3]) # element returned as list

l1[[3]] # returns 3rd element of list
class(l1[[3]]) # element returned as class of stored object

## To select specific value from a list element, combine [[x]] with one of the selection procedures above
l1[[3]][1,] # selects 3rd element of list and returns first row of matrix
l1[3][1,] # NOTE THAT THIS DOESN'T WORK because using [x] to select list element returns element 3 as a list, not a matrix

## If lists are named, can select element directly by name
l2$dat3

## Selection in nested list structures follows same convention as above, but must specify which element from each list level should be selected
l3[[1]] # Selects all elements within element 1 of top-level list
l3[[1]][[3]] # Selects element 3 within element 1 of top-level list
l3[[1]][[3]][1,] # Selects all values from row 1 or element 3 within element 1 of top-level list

## If lists are named, can select elements directly by name
l4$Person1 # Selects all elements of Person 1 list
l4$Person1$dat3 # Selects dat3 from Person 1 list


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# VECTOR & MATRIX OPERATIONS #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
## Operations on vectors and matrices work on EACH value within the object
v1 + 10 # adds 10 to each value in vector
mat1 + 10 # adds 10 to each value in matrix

v1*10 # multiplies each value in vector by 10
mat1*10 # multiplies each value in matrix by 10

v1 + v1 # adds together each element in same position of vectors
mat1 + mat1 # adds together each element in same position of matrices
