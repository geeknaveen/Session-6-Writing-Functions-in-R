# Session 6: Writing Functions in R ::::::::::

# 1. Write a R program using control operators to test whether following values are prime 
# numbers or not by providing a PRIME or NOT PRIME message as output : 
# A. 103 
# B. 82 
# C. 179 

# Creating function with num variable and calling it by is.prime
is.prime <- function(num) {
  if (num == 2) 
    {
    print("PRIME")
  } else if (any(num %% 2:(num-1) == 0)) 
    {
    print("NOT PRIME")
  } else 
    { 
    print("PRIME")
  }
}

# Testing the number's for prime or not
is.prime(103) 
is.prime(82)
is.prime(179)

# 2. Write a R program using control operators to identify letter u and a both occur in the 
# following words: 
# 1. above 
# 2. unit 
# 3. Under 

# Creating a vector v
v <- c("above","unit","Under")
# If a & u are case insensitive:;
grepl('a',v,ignore.case = TRUE) | grepl('u',v,ignore.case = TRUE)

# If A & U are case-sensitive:::::
grepl('a',v,ignore.case = FALSE) | grepl('u',v,ignore.case = FALSE)


# 3. Write a function that to calculate BMI (Body Mass Index):  
#    BMI for a person is defined as their body mass divided by the square of their height 

# The weight is in kilograms and the height in meters::::
  BMI <- function(height,weight) {
       # return(weight/(height*height)) 
  BMI <- weight/(height*height);
    if (BMI < 15) BMIResult <- "Very severely underweight";
    if (BMI >= 15 && BMI < 16) BMIResult <- "Severely underweight";
    if (BMI >= 16 && BMI < 18.5) BMIResult <- "UNDERWEIGHT";
    if (BMI >= 18.5 && BMI < 25) BMIResult <- "Normal (healthy weight)";
    if (BMI >= 25 && BMI < 30) BMIResult <- "Overweight";
    if (BMI >= 30 && BMI < 35) BMIResult <- "Obese Class I (Moderately obese)";
    if (BMI >= 35 && BMI < 40) BMIResult <- "Obese Class II (Severely obese)"; 
    if (BMI >= 40) BMIResult <- "Obese Class III (Very severely obese)";
    BMIResult     
}

# Passing Height(in m) & weight(in Kgs) to BMI function
BMI(1.90,95)

# 4. Write a function called  sum_of_cubes, that calculates the sum of cubes of the first n 
# natural numbers :  
# if  we have two numbers : 1, 2 then sum of squares is 9 ( 1^3  +  2^3) 
# if  we have three numbers : 1, 2, 3 then sum of squares is 36 ( 1^3  +  2^3 + 3^3) 

# In other words, the sum of the first n cubes is the square of the sum of the first n natural numbers.

sum_of_cubes <- function(num) {
if(num < 0) {
  print("Enter a positive number")
} else {
  cube = (num * (num + 1)/2)^2;
}
  print(paste("The sum of cubes is ", cube, sep = ": "))
}

sum_of_cubes(4)
# sapply(1:3,sum_of_cubes)

# 5. Write a function to calculate the mode (highest frequency) of the following vector: 
#     x = c(2,3,3,4,4,5,6,7,9,10) 

# Solution - 1:
Mode = function(x){ 
  ta = table(x)
  tam = max(ta)
  if (all(ta == tam))
    mod = NA
  else
    if(is.numeric(x))
      mod = as.numeric(names(ta)[ta == tam])
  else
    mod = names(ta)[ta == tam]
  return(mod)
}

# Calling the mode function (highest frequency)
Mode(c(2,3,3,4,4,5,6,7,9,10))

# Solution - 2:
x = c(2,3,3,4,4,5,6,7,9,10) 
temp <- table(as.vector(x))
names(temp)[temp == max(temp)]

# 6. Write a function to calculate the no. of prime numbers  of the following vector : 
#    x = c(2,2,3,3,4,5,7,11,15,19,24,29)

# Creating a vector x
x <- c(2,2,3,3,4,5,7,11,15,19,24,29)
x
# Initializing count as 0
count <- 0
for (val in x) {
   if (val == 2)  
        count = count+1
   {
     if (any(val %% 2:(val-1) == 0))  
       count = count+1;
   }
} 
print(paste("Total no: of prime numbers is", sep = " ", count))
    
# 7. Create a R package for calculating the count of prime numbers, name it as 
# "CountPrime" 




# 8.  Perform below operations using Data.frame and Data.table 
# a.  Load 2 files (.csv) and show it on screen ( F1 - empno, deptid,mgr_id , F2 - empno, 
#                                                 sal, DOJ) 
# b.  Perform equi join 
# c.  Perform left outer join 
# d.  Perform right outer join 
# e.  Perform full outer join 
# f.  Perform filter operation -Eg find all the rows for which col1 is null 
# g.  Perform group by , sum, average operation 
# h.  Perform (A "-" B) operation 
# i.  Create a derived column (empname in F2) - do some data transformation on that 
# j.  Create a "working" test that example Business Analytics with R 
# Write output for each in separate files (eg- equijoin.csv ,leftoutjoin.csv) 

# ********** Portion not covered **************

# 9.  Create R functions for the following operations 
# a.  Find out unique combinations of data based on a particular column or group of columns. 
# i.  Example: 
#   - Select count(distinct stdid) from student group by classid 
# - Select count(distinct stdid) from student group by classid, sectionid 
# .  Write a function to find out duplicate entries  based on a key or group of keys 

# ********** Portion not covered **************

# 10. Create R functions for the following operations 
# a.  Find out if there are any nulls in a dataset or in some specific number of columns 
# b.  Write a function to read data from hdfs and dump it back to hdfs 
#
# ********** Not attended Bigdata Hadoop classes *******

# 11. Create R functions for the following operations 
# a.  Remove duplicates from a given vector and return it back. 
# b.  Compute count of distinct  
# c.  Concatenate two strings. 
# d.  Perform Column-wise/Row-wise sum using apply function. 
# e.  Get list of files in an hdfs path. 
# f.  Delete a file from hdfs if it exists. 

#********** Not attended Bigdata Hadoop classes *******

# 12. Create R functions for the following operations 
# a.  Load some csv data from hdfs. 
# b.  Get latest hive partition given a base hdfs path (<basePath>/date=20150201). 
# c.  Rename column names in a dataframe 
# d.  Drop given column from a data frame 
# e.  Illustrate the difference between NA, NULL, NaN. 
# f.  Return true if and only if all rows of given vector satisfy a certain condition. 
# g.  Compute number of unique combinations in a data frame grouped by certain columns.

#********** Not attended Bigdata Hadoop classes *******
