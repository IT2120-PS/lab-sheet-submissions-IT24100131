# Set the working directory
setwd("C:\\Users\\it24100131\\Desktop\\it24100131")  # Update with your actual path

# Import the dataset into a data frame called "Delivery_Times"
Delivery_Times <- read.table("Exercise - Lab 05.txt", header=TRUE, sep=",")
head(Delivery_Times)  # Check first few rows to verify successful import



# Draw the histogram with 9 classes, using right-open intervals
hist(Delivery_Times$Delivery_Time, 
     main="Histogram of Delivery Times", 
     xlab="Delivery Time (minutes)", 
     ylab="Frequency", 
     breaks=seq(20, 70, by=5),  # 9 intervals between 20 and 70
     right=FALSE)  # Right-open intervals



# Comment on the shape of the histogram
# The histogram shows a right-skewed distribution with the majority of delivery times
# concentrated between 20 and 40 minutes. There are fewer occurrences of longer delivery
# times, and the distribution tails off as delivery time increases.





# Calculate the cumulative frequency
cum_freq <- cumsum(table(cut(Delivery_Times$Delivery_Time, breaks=seq(20, 70, by=5), right=FALSE)))

# Draw the cumulative frequency polygon (ogive)
plot(seq(20, 65, by=5), cum_freq, type='o', 
     main="Cumulative Frequency Polygon (Ogive) for Delivery Times", 
     xlab="Delivery Time (minutes)", 
     ylab="Cumulative Frequency", 
     ylim=c(0, max(cum_freq)), 
     pch=16)

