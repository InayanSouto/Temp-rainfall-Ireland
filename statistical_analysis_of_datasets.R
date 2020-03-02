library(ggpubr)
library(pwr)
library(dplyr)
library(tibble)
#install.packages("finalfit")
library(finalfit)
#install.packages("naniar")
library(naniar)

#importing data
rainfall <- read.csv('rainfall.csv', 
                     header = TRUE, 
                     sep = ",",
                     strip.white = TRUE,
                     stringsAsFactors = FALSE)
temp <- read.csv('temp.csv',
                 header = TRUE, 
                 sep = ",",
                 strip.white = TRUE,
                 stringsAsFactors = FALSE)
str(rainfall)



# removing unwanted rows; any row containging
# a certain string is removed
# only want the mean (given in dataset)
temp <- temp[grepl("Mean", temp$X.1),]

rainfall <- rainfall[grepl("Total", rainfall$X.1),]

#merging two columns and removing old column
temp$X.1 <- paste(temp$X, temp$X.1)
temp <- temp[,-1]

rainfall$X.1 <- paste(rainfall$X, rainfall$X.1)
rainfall <- rainfall[,-1]

# dealing with non-interger values
temp[temp == ".."] <- NA

rainfall[rainfall== ".."] <- NA



# making rainfall and temp the variables in each dataframe
# transposing the column and rows

temp <-temp %>%
  t() %>%
  as.data.frame(stringsAsFactors = F) %>%
  rownames_to_column("value") %>%
  `colnames<-`(.[1,]) %>%
  .[-1,] %>%
  `rownames<-`(NULL)


rainfall <-rainfall %>%
  t() %>%
  as.data.frame(stringsAsFactors = F) %>%
  rownames_to_column("value") %>%
  `colnames<-`(.[1,]) %>%
  .[-1,] %>%
  `rownames<-`(NULL)


#for visualising the missing data and giving percentage that is missing
# 7 of of the 15 weather stations have a substantial number of missing values
# it is not known what caused these NA values
# but they will be ignored in the analysis and will be dropped
# imputation would be the preferred choice over dropping the variables
#  but the variables that remain should give a decent view of the data even
# with the missing data being dropped
# this will of course effect the accuracy of the study

par(mfrow=c(2,4))
gg_miss_var(rainfall)
gg_miss_var(temp)
pct_miss(rainfall)
pct_miss(temp)
  

#do not want to lose any data realting to the years,
# so will swap the row and columns again in order
# to drop the weather stations that have NA rather than
# dopping the years as this would greatly effect
# the granularity of the data 

temp <-temp %>%
  t() %>%
  as.data.frame(stringsAsFactors = F) %>%
  rownames_to_column("value") %>%
  `colnames<-`(.[1,]) %>%
  .[-1,] %>%
  `rownames<-`(NULL)


rainfall <-rainfall %>%
  t() %>%
  as.data.frame(stringsAsFactors = F) %>%
  rownames_to_column("value") %>%
  `colnames<-`(.[1,]) %>%
  .[-1,] %>%
  `rownames<-`(NULL)

#removing na
temp <- na.omit(temp) 
rainfall <- na.omit((rainfall))

#transposing again

temp <-temp %>%
  t() %>%
  as.data.frame(stringsAsFactors = F) %>%
  rownames_to_column("value") %>%
  `colnames<-`(.[1,]) %>%
  .[-1,] %>%
  `rownames<-`(NULL)


rainfall <-rainfall %>%
  t() %>%
  as.data.frame(stringsAsFactors = F) %>%
  rownames_to_column("value") %>%
  `colnames<-`(.[1,]) %>%
  .[-1,] %>%
  `rownames<-`(NULL)


# renaming variables
colnames(temp) <- c("Month & Year", "Belmullet Temp","Valentia Temp", "Casement Temp", "Cork Temp",
                           "Dublin Temp","Malin Head Temp", "Mullingar Temp", "Shannon Temp")

colnames(rainfall) <-c("Month & Year","Belmullet Rain","Valentia Rain", "Casement Rain", "Cork Rain",
                          "Dublin Rain","Malin Head Rain", "Mullingar Rain", "Shannon Rain")



# merging the two datasets by the X.1 colum (yearly and monthly data)
total <- merge(temp, rainfall, by = "Month & Year")
total_data <-  merge(temp, rainfall, by = "Month & Year")

total <-total_data[,-1]
rownames(total) <- total_data[,1]

total[, 2:ncol(total)] <- sapply(total[, 2:ncol(total)],
                                 as.numeric)

#####ignore##### this is for yearly data, replacing each variable with the mean for that year######
#sorting out the years
#only want to know averagae yearly values  for temp and rain
# so edit each value in Month & Yeart varible to only contain the year
# find.list <- list("X", "M01", "M02", "M03",
#               "M04", "M05", "M06", "M07", 
#                    "M08", "M09", "M10", "M11", "M12")
# find.string <- paste(unlist(find.list), collapse = "|")
# # # 
# total_data$`Month & Year` <- gsub(find.string, replacement = "", x = total_data$`Month & Year`)
# total_data$`Month & Year` <- as.numeric(total_data$`Month & Year`)
#levels(total_data$`Month & Year`)
# rm(Ulster)
# 
# #making the first colum the index (years and months)
# #  <-total_data[,-1]
# # rownames(total) <- total_data[,1]
# # 
# # 
# # #new dataframe containg the average temp for each station 
# # # convert all columns excpet first to numeric
# # # calculate the mean values for each factor variable
# # # making the first colum  the index (years)
# total[, 1:ncol(total)] <- sapply(total[, 1:ncol(total)],
#                                   as.numeric)
# # 
# yearly1 <- aggregate(total[,2:ncol(total)], 
#            by = list(total_data$`Month & Year`), 
#            FUN = mean)
# summary(total_data)[["1st Qu."]]
# outvals <- boxplot(Munster, plot = FALSE)$out
# 
# outvals
# summary(total)

# splitting into two groups; before the millenium and after

# #1990s
# nineties_avg <- yearly1 %>% 
#   filter(Group.1 < 2000)
# 
# as.factor(nineties_avg$Group.1)
# boxplot(`Malin Head Rain`~ Group.1, data = nineties_avg)
# plot(nineties_avg$Group.1, nineties_avg$`Malin Head Rain`)
# 
# # 2000's
# millenia_avg <-yearly1 %>%
#   filter(Group.1 > 1999)
# 
# 
# 
# yearly <- yearly1[,-1]
# rownames(yearly) <- yearly1[,1]
# 
# 

#rainfall is the dependant variable and temperature is the independant
# want to see if rainfall is affected by temp change
#make dataframe numeric

total_data[, 1:ncol(total)] <- sapply(total_data[, 1:ncol(total_data)],
                                 as.numeric)
str(total_data)

# before Splitting dataset into groups by province,
# checking each weather station data in Connaucht (only one with stations >2)
#  they are positively skewed. from the histograms and density graph
par(mfrow = c(2,2))
hist(total$`Belmullet Rain`, main = "Belmullet rain hist.")
hist(total$`Mullingar Rain`, main = "Mullingar rain hist.")
hist(total$`Shannon Rain`, main = "Shannon rain hist.")

#median is a better approximation and so will be used to
# represent the province as a whole
plot(density(total$`Belmullet Rain`),main = "Example of Connaucht skewness")
abline(v =c(median(total$`Belmullet Rain`), mean(total$`Belmullet Rain`)),
        col = c('red','blue'), lty = 2)


# splitting dataset into data pertaining to each province
Ulster <- total %>% select(`Malin Head Temp`,`Malin Head Rain`)
Munster <- total %>% select(`Valentia Temp`,`Valentia Rain`)
Leinster <- total %>% select(`Casement Temp`, `Casement Rain`,
                             `Dublin Temp`, `Dublin Rain`)
Connaucht <- total %>% select(`Belmullet Temp`, `Belmullet Rain`,
                              `Mullingar Temp`, `Mullingar Rain`,
                              `Shannon Temp`, `Shannon Rain`)

#mean rain and temp for connaucht and leinster for each year
# as they contain > 1 weather station
# this will unfortunaetly effect granularity 
Connaucht$Mean_Temp <- apply(Connaucht[,c("Belmullet Temp","Mullingar Temp",
                                          "Shannon Temp")], 1, median)
Connaucht$Mean_Rain <- apply(Connaucht[,c("Belmullet Rain", "Mullingar Rain",
                                          "Shannon Rain")], 1, median)
Leinster$Mean_Temp <- apply(Leinster[,c("Casement Temp", "Dublin Temp")], 1, mean)
Leinster$Mean_Rain <- apply(Leinster[,c("Casement Rain", "Dublin Rain")], 1, mean)

# mean, stdev skew and kurtosis for each province
my_stats <- function(x, na.omit = FALSE){
  if(na.omit)
    x <- x[!is.na(x)] #omits missing values
  med <- median(x)
  m <- mean(x)
  n <- length(x)
  s <- sd(x)
  skew <- sum((x-m) ^ 3 / s ^ 3)/n
  kurt <- sum((x-m) ^ 4 / s ^ 4)/n - 3
  
  return(c(median= med,n = n, mean = m, stdev = s, skew = skew,
           kurtosis = kurt))
}
sapply(Connaucht[,7:ncol(Connaucht)], my_stats)
sapply(Munster, my_stats)
sapply(Ulster, my_stats)
sapply(Leinster[,5:ncol(Leinster)], my_stats)
#each have kurtosis < 3 indiciating Platykurtic distribution so fewer extreme 
# extreme values at the tails than that of normal distribution
# further edging us toward non-parametric test

#function telling how manyh outliers are present in each group
# there are very few outliers per group comapared to the amount of data

outvals <- function(x){
    outliers <-boxplot(x, plot = FALSE)$out
  

  
   return(sum(outliers > 0))

  }

sapply(Ulster, outvals)
sapply(Connaucht, outvals)
sapply(Munster, outvals)
sapply(Leinster, outvals)


#density plots and histograms for each province
# shows releveant skewness of the dependat variable
d_u <- density(Ulster$`Malin Head Rain`) 
d_c <- density(Connaucht$Mean_Rain)
d_m <- density(Munster$`Valentia Rain`)
d_l <- density(Leinster$Mean_Rain)



par(mfrow=c(2,2))
#densit plots; using median as straight line throught the median
# through the median because of the positive skewness

plot(d_u, main = "Ulster Rain dist.")
abline(v = 87.8,#median
       lty = 2, col = "blue")
plot(d_c, main = "Connaucht Rain dist.")
abline(v =79.5 ,#median
        col ="blue", lty = 2)
plot(d_m, main = "Munster Rain dist.")
abline(v = 122.6,#median
       lty = 2, col = "blue")
plot(d_l, main = "Leinster Rain dist.")
abline(v = 60.3,#median
       lty = 2, col = "blue")

hist(Ulster$`Malin Head Rain`, main = "Ulster Rain hist.")
hist(Connaucht$Mean_Rain, main = "Connaucht Rain hist.")
hist(Munster$`Valentia Rain`, main = "Munster Rain hist.")
hist(Leinster$Mean_Rain, main = "Leinster Rain hist.")
# postiviely skewed; best to use median. 

#effect size for the analysis is 0.3
#  correlation is the analysis type
cohen.ES(test = "r", size = "medium")


# type 2 error tolerance of 0.1 so Power = 0.90
# type 1 error tolerance is usually 0.05. anything
# greater than this and H0 must be accepted or risk
# makign type 1 error
# two sided test (default); using two tail test because
# regardless of the direction of the relationship, I am
# testing for the possibility of a relationship in both directions
r_test <- pwr.r.test(r = 0.3, sig.level = 0.05, power = 0.9, alternative = "two.sided")
plot(r_test)

#taking the recommended sample

set.seed(1234)
sample <- dplyr::sample_n(Ulster, 150)
sample_c <- dplyr::sample_n(Connaucht, 150)
sample_m <- dplyr::sample_n(Munster, 150)
sample_l <- dplyr::sample_n(Leinster, 150)
#normality plot
# to see if the data follows a normal distribution.
# central limit theorem states that a large enough sample size
# sample > 30 will lead to a normal dist.



ggqqplot(sample$`Malin Head Rain`, main ="Ulster Normality")

ggqqplot(sample_c$Mean_Rain, main = "Connaucht Normality")

ggqqplot(sample_m$`Valentia Rain`, main = "Munster Normality")

ggqqplot(sample_l$Mean_Rain, main = "Leinster Normality")

#shapiro to show normality as visually determening normality is unreliable with 
# large dataframes
# p value is << 0.05 meaning a non-parametric test must be used
# also the plots shows the data does not follow a normal distribution - high kurtosis

shapiro.test(sample$`Malin Head Rain`)
shapiro.test(sample_c$Mean_Rain)
shapiro.test(sample_m$`Valentia Rain`)
shapiro.test(sample_l$Mean_Rain)


# correlation test: spearman test

# ulster
cor.test(sample$`Malin Head Temp`, sample$`Malin Head Rain`,
         method = "spearman", exact = FALSE)

# munster
cor.test(sample_m$`Valentia Temp`, sample_m$`Valentia Rain`,
         method = "spearman", exact = FALSE)

#connacht 
cor.test(sample_c$Mean_Temp, sample_c$Mean_Rain,
         method = "spearman", exact =FALSE)
# 
# #cor.test(sample_c$`Belmullet Temp`, sample_c$`Belmullet Rain`,
#          method = "spearman", exact =FALSE)
# 
# cor.test(sample_c$`Mullingar Temp`, sample_c$`Mullingar Rain`,
#          method = "spearman", exact =FALSE)
# cor.test(sample_c$`Shannon Temp`, sample_c$`Shannon Rain`,
#          method = "spearman", exact =FALSE)

# Leinster
cor.test(sample_l$Mean_Temp, sample_l$Mean_Rain,
         method = "spearman", exact = FALSE)

#Leinster accepts the null hypothesis as p-value > 0.05; avoiding a type 1 error
# each of the other provinces have p valu << 0.05 and so accept the alternate hypothesis; 
# avoiding a type 2 error (accepting H0)



library(forecast)

write.csv(Ulster, file = "ulster.csv")
y_range <- c(min(sample_m$`Valentia Rain`), max(sample_m$`Valentia Rain`))

plot(ma(sample_m$`Valentia Rain`, 15), ylim = y_range)
Acf(Ulster)

library(tseries)
adf.test(Ulster)

Pacf(Ulster)

ndiffs(h)


