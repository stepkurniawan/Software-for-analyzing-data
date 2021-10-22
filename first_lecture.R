plot(iris)



for (i in 1:10){
    print("this is awesome")
}

print("ab")

getwd() #get working directory
setwd()

ds <- read.csv(Titanic.csv) #comma separated
ds <- read.csv(Titanic.csv, sep = ".") #things separated
ds <- read.csv2(Titanic.csv) #semicolon separated3
