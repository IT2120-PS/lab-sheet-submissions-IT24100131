# Set working directory - change to your folder path
setwd("C:\\Users\\it24100131\\Desktop\\it24100131")

# Import the dataset 'DATA 4.txt' (replace with your actual filename)
# Assuming the dataset is space separated
data <- read.table("DATA 4.txt", header=TRUE, sep = ",")

# View the data (optional)
fix(data)  # Opens spreadsheet style viewer in RStudio

# Attach data to call variables directly
attach(data)
   


# Boxplots
boxplot(X1, main="Box plot for Team Attendance", outline=TRUE, outpch=8, horizontal=TRUE)
boxplot(X2, main="Box plot for Team Salary", outline=TRUE, outpch=8, horizontal=TRUE)
boxplot(X3, main="Box plot for Years", outline=TRUE, outpch=8, horizontal=TRUE)

# Histograms
hist(X1, ylab="Frequency", xlab="Team Attendance", main="Histogram for Team Attendance")
hist(X2, ylab="Frequency", xlab="Team Salary", main="Histogram for Team Salary")
hist(X3, ylab="Frequency", xlab="Years", main="Histogram for Years")

# Stem and Leaf Plots
stem(X1)
stem(X2)
stem(X3)



# Mean
mean(X1)
mean(X2)
mean(X3)

# Median
median(X1)
median(X2)
median(X3)

# Standard Deviation
sd(X1)
sd(X2)
sd(X3)




# Summary (Min, Q1, Median, Mean, Q3, Max)
summary(X1)
summary(X2)
summary(X3)

# Or get quartiles directly
quantile(X1)
quantile(X2)
quantile(X3)

# Extract Q1 and Q3 for X1
q1_x1 <- quantile(X1)[2]
q3_x1 <- quantile(X1)[4]
print(paste("Q1 for X1:", q1_x1))
print(paste("Q3 for X1:", q3_x1))


IQR(X1)
IQR(X2)
IQR(X3)

  




get.mode <- function(v){
  counts <- table(v)
  modes <- names(counts)[counts == max(counts)]
  return(modes)
}

# Test mode function on Years
mode_years <- get.mode(X3)
print(paste("Mode(s) of Years:", paste(mode_years, collapse=", ")))



get.outliers <- function(z){
  q1 <- quantile(z, 0.25)
  q3 <- quantile(z, 0.75)
  iqr <- q3 - q1
  
  ub <- q3 + 1.5 * iqr
  lb <- q1 - 1.5 * iqr
  
  outliers <- z[z < lb | z > ub]
  
  print(paste("Upper Bound =", ub))
  print(paste("Lower Bound =", lb))
  
  if(length(outliers) == 0){
    print("No outliers found.")
  } else {
    print(paste("Outliers:", paste(sort(outliers), collapse=", ")))
  }
  
  return(outliers)
}

# Check for outliers
outliers_X1 <- get.outliers(X1)
outliers_X2 <- get.outliers(X2)
outliers_X3 <- get.outliers(X3)








