#SessionTwo
library(tidyverse) #to import the downloaded package

#-------------------------------------------------------
#SELECT 
#--------------------------------------------------------

select(mtcars, mpg, cyl)
select(mtcars, mpg:hp) #select range of column
select(mtcars, -cyl, -hp) # everything but not cyl and hp

select(mtcars, starts_with("d"))

select (mtcars, contains("d")) # the data contains something

select (mtcars, mpg) # return a dataframe/ tibble, even for a single column
class(select (mtcars, mpg)) 

pull(mtcars, mpg) #get data from the column mpg as a vector 


#-------------------------------------------------------
#FILTER 
#--------------------------------------------------------
filter(mtcars, mpg > 20, gear!=4) # filter rows AND
filter(mtcars, disp < 200 | wt>5 ) #filter rows using OR
filter (mtcars, hp > mean(hp)) # filter on the fly 

# logical expression
# %% modulus

#-------------------------------------------------------
#DATA MASKING 
#--------------------------------------------------------

mtcars[mtcars&mpg >20] # normal base R
mtcars %>% filter (mpg > 20) #tidyverse know the column name

slice(mtcars, 1:5)#slice the ROW -> get only a specific row
slice_min(mtcars, mpg)# like filter
slice_max(mtcars, hp) 

#-------------------------------------------------------
#MUTATE CREATING NEW COLUMN 
#--------------------------------------------------------
mutate(mtcars, mpl = mpg /3.785) # mutate: create new column mpl from mpg
mutate(mtcars, mtdiff = round(wt - mean(wt),1 )) # on the fly result
mutate(mtcars, am = NULL) # delete column am

transmute (mtcars, hppcyl = round(hp/cyl, 2)) # like mutate, but only new col

#-------------------------------------------------------
#SUMMARY 
#--------------------------------------------------------

summarize(mtcars, meanmpg = mean(mpg))#not a scalar, but a dataframe
summarise(mtcars, minhp = min(hp), maxhp = max(hp))
summarise(mtcars, checkNA=any(is.na(mpg))) # check NA in a column
summarize(mtcars, n()) #row counts

#-------------------------------------------------------
#ARRANGE 
#--------------------------------------------------------
arrange(mtcars, mpg)# sort tibble based on a column
arrange(mtcars, desc(mpg))
arrange(mtcars, gear, disp)# arrange mtcats with (1) gear

#-------------------------------------------------------
#EXPORT 
#--------------------------------------------------------
root_dir <- getwd()
root_dir
setwd("S:/Users/steph/OneDrive - Leuphana Universität/Semester 1/Software for analyzing data/Tutorial 2/")

write.csv(mtcars, file = "mtcars.csv")
