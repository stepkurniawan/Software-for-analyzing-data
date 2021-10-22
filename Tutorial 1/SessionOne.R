library(tidyverse) #to import the downloaded package
library(jsonlite)

root_dir <- getwd()
setwd("S:/OneDrive - Leuphana Universität/Semester 1/Software for analyzing data/Tutorial 1/Data")

data <- paste(root_dir,"/Data/Titanic.csv", sep = "")  #concacenate as string



# reading csv
ds <- read.csv("Titanic.csv")
ds #print this
summary(ds) # summary of ds

tibble::view(ds) # view as a table

head(ds) 
tail(ds)

str(ds) # structure of a table

dim (ds) #  get how many rows, column -> as a vector
nrow(ds) # number of rows
ncol(ds) # number of columns

colnames(ds) # [1] "survived"  "fare"      "residence" "class"     "age"
rownames(ds)

ds$fare # access column called "fare" in dataset "ds" , prints out the rows

head(levels(as.factor(ds$age))) # age are not always integer
#"0.1667" "0.3333" "0.4167" "0.6667" "0.75"   "0.8333"

levels(as.factor(ds$age))


mean(ds$survived)
sd(ds$age, na.rm == TRUE)

str(ds)

# rm (ls())
ello <- list()
ello[1] <- 1
ello[2] <- 2

vec <- c("one", "two", "three")
vec2 = c("one","two","threhaha")

x <- c(10.4, 5.6, 3.1, 6.4, 21.7)
1/x

y <- x[!is.na(x)]


# levels = like distinct , to see what are the different data
levels(as.factor(ds$age)) 

#residence
str(ds$residence)
levels(as.factor(ds$residence)) #[1] "0" "1" "2" -> actually a categorical data

new_ds = ds
new_ds$residence <- as.raw(new_ds$residence)
new_ds$residence
str(new_ds$residence)
levels(as.factor(ds$residence))

#summary
summary(new_ds)
