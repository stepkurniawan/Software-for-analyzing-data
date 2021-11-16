##General introduction by Jannes Münchow##
##########################################

#Create a vector from 1 to 10
nums <- 1:10
#Add 1 to each element of the vector
nums + 1
#Add 1 and 2 in an alternating manner
nums + c(1, 2)

#Create a matrix with 5 rows, 3 columns and the column names A, B and C
rmat = matrix(rnorm(15), nrow = 5, ncol = 3,
              dimnames=list(NULL, c("A", "B", "C")))  #NULL indicates that we don't want to define rownames
#What are the column names
colnames(rmat)
#not working for just created matrices
rownames(rmat)
#Assign rownames
rownames(rmat) <- 1:5

#Vector from 1 to 10
1:10
#The same can be achieved using the seq-command
seq(1, 10)
#Create a sequence from 1 to 10, by defines the distance between the elements
seq(from = 1, to = 10, by = 2)
#Create a sequence starting from 10 with an increment of 5 and a length of 10 elements
seq(from = 10, by = 5, length = 10)

#Create 3 columns each containing factors
thelevels = data.frame(group = gl(n = 3, k = 10, length = 30, labels = c("mouse", "cat", "dog")),
                       subgroup = gl(n = 5, k = 2, length = 30),
                       obs = gl(2, 1, length = 30))
#Structure of the created dataframe
str(thelevels)

#Create a vector with 10 randomly chosen numbers between 1 and 10
sequence <- sample(1:10)
#Repeat a 10 times
rep("a", 10)

#Create a matrix with 2 columns and 3 rows
matrixR <- matrix(ncol = 2, nrow = 3)
#fill the first column
matrixR[, 1] <- c(1, 2, 3)
#fill the second column
matrixR[, 2] <- c(4, 5, 6)

#Convert the matrix into a dataframe
testR <- as.data.frame(matrixR)
#Rename the columns
colnames(testR) <- c("one", "two")
#Rename the rows
rownames(testR) <- c("line1", "line2", "line3")

#Create a date
as.Date("1915-6-16")
#another format recognized by the as.Date-command
as.Date("1990/02/17")
#how to specify which of the numbers corresponds to month, day and year
as.Date("1/15/2001", format = "%m/%d/%Y")
#another way of creating a date
mydate1 = strptime("4/Mar/2012:12:51:00", format = "%d/%b/%Y:%H:%M:%S")
?strptime

#Create 2 dates
mydate1 = strptime("5/Mar/2012:09:05:00", format = "%d/%b/%Y:%H:%M:%S")
mydate2 = strptime("8/Mar/2012:16:55:00", format = "%d/%b/%Y:%H:%M:%S")
#Calculate the time difference in hours
difftime(mydate2, mydate1, units= "hours")

#Create a vector
data = c(1,2,2,3,1,2,3,3,1,2,3,3,1)
#Convert the vector into a factor
fdata = factor(data)

#Renaming factors
rdata = factor(data, labels=c("I","II","III"))
rdata

#Create a vector with names of months
mons = c("March", "April", "January", "November", "January", "September", "October", "September", "November", "August", "January",
         "November", "November", "February", "May", "August", "July", "December", "August", "August", "September", "November",
         "February", "April")
         
#Convert the vector into a factor
mons <- factor(mons)

#Use the table function to obtain how often each months was counted
table(mons)

#skip slide 210

#Levels should be ordered properly
mons = factor(mons, levels = c("January", "February", "March", "April", "May", "June",
              "July", "August", "September", "October", "November", "December"))
table(mons)

#Create a vector
fert = c(10, 20, 20, 50, 10, 20, 10, 50, 20)
#Convert into a factor with ordered levels
fert = factor(fert, levels = c(10,20,50), ordered = T)

#Letters in R
#Create letters using sampling with replacement
lets <- sample(letters, size = 100, replace = T)
#Convert the characters into a factor
lets <- factor(lets)
#show us a table of the first five elements
table(lets[1:5])

#Using some (logical) operators
nums = c(12, 9, 8, 14, 7, 16, 3, 2, 9)
#all numbers greater than 10
nums > 10
#all numbers greater 10 and smaller than 16
nums > 10 & nums < 16

#At which position can we find the numbers > 10 in the vector
which(nums > 10)
#ok, and which number do they take
nums[which(nums > 10)]
#Replace all numbers > 10 by 0
nums[nums > 10] <- 0

#x = matrix(1:12, nrow = 4, ncol = 3)
#x[, 1]

#Table example Crawley, page 187
#red blood cells were counted on 10 000 slides
cells <- rnbinom(10000, size=0.63, prob=0.63/1.83)
#We want to count how many times we got no red blood cells on the slide, and how often we got 1, 2, 3, ... cells.
table(cells)
#Here we know that the first 5000 samples came from male patients and the second 5000 from females
gender <- rep(c("male", "female"), c(5000, 5000))
#To tabulate the counts separately for the two sexes
table(cells, gender)

#working with characters
state.name

#number of characters of each state
nchar(state.name)

x = 7; y = 10
#print a text in the R console (often needed while programming your own functions)
cat("x should be greater than y, but x =",x," and y =",y,"\n")
#cut a string
substr(state.name, start = 2, stop = 6)

sentence = "R is a free software environment for statistical computing"
#Split a sentence into its parts
parts = strsplit(sentence, split = " ")

#pets = c("dog", "cat", "duck", "chicken", "duck", "cat", "dog")
#tt = table(pets)

#Dataframe iris
iris
#how is it structured
str(iris)
#What are its colomn names
names(iris)
#How do the first six lines look like?
head(iris)

#mean Petal width
mean(iris$Petal.Width)
#tabulate the petal widths for each species separately
table(iris$Petal.Width, iris$Species)
#Calculate the mean of the columns 1 to 4
apply(iris[, c(1:4)], 2, mean)

require(stats); require(graphics)
x <- list(a = 1:10, beta = exp(-3:3), logic = c(TRUE,FALSE,FALSE,TRUE))
#calculate the mean of each list member
lapply(x, mean)

#Calculate the mean sepal length (= iris[, "Sepal.Length"]) for each species
iris[, c("Sepal.Length", "Species")]
tapply(iris[, "Sepal.Length"], iris$Species, FUN = mean)
