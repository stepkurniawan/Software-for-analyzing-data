#check the dataset
?Seatbelts

Seatbelts
?str
str(Seatbelts) # Time-Series data
seat_df <- as.data.frame(Seatbelts)
str(seat_df)

hist(Seatbelts)
hist(seat_df$rear, breaks = 5) # not so normally distributed
hist(seat_df$front, breaks= 10) #normally distributed

summary(Seatbelts)
summary(seat_df)

#simple tests
boxplot(seat_df$front, seat_df$rear)
shapiro.test(seat_df$front) # p-value = 0.6549
shapiro.test(seat_df$rear) # p-value = 0.05158
var.test(seat_df$front, seat_df$rear) #p-value < 2.2e-16
t.test(seat_df$front, seat_df$rear) #p-value < 2.2e-16

