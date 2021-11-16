# Execise putting all together session two
library(tidyverse) #to import the downloaded package
data("mtcars")

ds <- mtcars
ds
dsHpPerGear <- mutate(ds, hpPerGear = hp/gear)
ds_filtered <- filter(dsHpPerGear, carb != 4, hpPerGear>50)

arrange(ds_filtered, desc(carb), mpg) #arrange based on high carb and low miles/gallon

mean(ds_filtered$disp)
# summarise everything
summarize(ds_filtered, displacement_mean = mean(disp), quarter_mile_time_mean = mean(qsec))

