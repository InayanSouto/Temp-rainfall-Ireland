library(ggpubr)
library(pwr)
library(dplyr)
library(tibble)
#install.packages("finalfit")
library(finalfit)
#install.packages("naniar")
library(naniar)
#install.packages("e1071")
library(e1071)
# default graph parameter setings
resetPar <- function() {
  dev.new()
  op <- par(no.readonly = TRUE)
  dev.off()
  op
}
## linear regression
par(mfrow = c(1,3))
scatter.smooth(Ulster$`Malin Head Temp`, Ulster$`Malin Head Rain`, main =  "Ulster scatter")
scatter.smooth(Connaucht$Mean_Temp, Connaucht$Mean_Rain, main = "Connacht scatter")
scatter.smooth(Munster$`Valentia Temp`, Munster$`Valentia Rain`, main = "Munster scatter")

#boxplots to spot any otuliers. outliers in teh predictor can effect
# the predictions made by the model
par(mfrow = c(1,3))
boxplot(Ulster$`Malin Head Temp`, main = "Ulster boxplot")
boxplot(Connaucht$Mean_Temp, main = "Connacht boxplot")
boxplot(Munster$`Valentia Temp`, main = "Munster boxplot")
par(resetPar())
#no outliers in any of the predictors

# density plots for temp (predictor) of each province
# would rather have the distribution be normal and not skewed
par(mfrow = c(3,1))
plot(density(Ulster$`Malin Head Temp`), main = "Ulster temp density plot", ylab =
       "Frequency", sub = paste("Skewness:",
                                round(e1071::skewness(Ulster$`Malin Head Temp`), 2)))
plot(density(Connaucht$Mean_Temp), main = "Connacht temp density plot", ylab =
       "Frequency", sub = paste("Skewness:",
                                round(e1071::skewness(Connaucht$Mean_Temp), 2)))
plot(density(Munster$`Valentia Temp`), main = "Munster temp density plot", ylab =
       "Frequency", sub = paste("Skewness:",
                                round(e1071::skewness(Munster$`Valentia Temp`), 2)))
par(resetPar())

# each follow a similar distribution; with a  dip in frequency around the mean.
# bimodal. they are not normally distributed

hist(Ulster$`Malin Head Temp`, main = "Ulster temp histrogram")
hist(Connaucht$Mean_Temp, main = "Connacht temp histrogram")
hist(Munster$`Valentia Temp`, main = "Munster temp histrogram")

cor(Ulster$`Malin Head Temp`, Ulster$`Malin Head Rain`)
cor(Connaucht$Mean_Temp, Connaucht$Mean_Rain)
cor(Munster$`Valentia Temp`, Munster$`Valentia Rain`)

# correlation between the two variables is low ~ -0.2 (negative correation as expected)
# each weather station in Connacht has checked individually for correaltion between
# temp and rainfall and was found to be roughly the same as the mean of each
# the correlation values coincide with information from the scatter plots i.e. low correlation


#linear models for each of the provinces, as well as leinster which was ruled out
# added to see if any trend in the other provinces is obvious to the eye
# these show the statistical signifigance of the models

conn_lm <- lm(Connaucht$Mean_Rain ~ Connaucht$Mean_Temp)
mnstr_lm <- lm(Munster$`Valentia Rain` ~ Munster$`Valentia Temp`)
lnstr <- lm(Leinster$Mean_Rain ~ Leinster$Mean_Temp)

par(mfrow = c(2,2))
plot(Ulster$`Malin Head Temp`,
     Ulster$`Malin Head Rain`,
     xlab = "Temp(degress celsius",
     ylab = "Rainfall(mm)",
     main = "Ulster plot showing regression line")
abline(ulster_lm)
plot(Connaucht$Mean_Temp,
     Connaucht$Mean_Rain,
     xlab = "Temp(degress celsius",
     ylab = "Rainfall(mm)",
     main = "Connacht plot showing regression line")
abline(conn_lm)
plot(Munster$`Valentia Temp`,
     Munster$`Valentia Rain`,
     xlab = "Temp(degress celsius",
     ylab = "Rainfall(mm)",
     main = "Munster plot showing regression line")
abline(mnstr_lm)
plot(Leinster$Mean_Temp,
     Leinster$Mean_Rain,
     xlab = "Temp(degress celsius",
     ylab = "Rainfall(mm)",
     main = "Leinster plot showing regression line")
abline(lnstr)

# there is a slight trend in the data of Uslter, Connacht and Munster
# as temperature increases the rainfall level decreases. 

ulster_lm
conn_lm
mnstr_lm
#regression information of each of the models
summary(ulster_lm)
summary(conn_lm)
summary(mnstr_lm)


# each of these models show a low p value so the null hypothesis is safely rejected
# and the predictor does have an effect on the dependant variable i.e. temperature effects
# the rainfall (alternate hypothesis).
# r quared value is also very low. This indicates the model is not great at making predictions
# due to a lot of unexplained variance in the data. Each model is statistically significant
#  but they are a poor fit to the data. This isn't an ideal scenario but it is not the worst.
# Predictions made using these models will not be fully accurate; the r squred value shows
# each model explains < 7% of the variance.
# Models dont seem to be a good fit to the data but are statistically significant nonetheless.
# might have to alter research question and  investigate the change in rainfall and temp over time
# 


# checking the resiudal normality. 
par(mcfro())
residual_normalitiy <- function(x,y,z){
  par(mfrow = c (3,1))
  hist(x$residuals, main = "Ulster histogram of residuals")
  hist(y$residuals, main = "Connacht histogram of residuals")
  hist(z$residuals, main = "Munster histogram of residuals")
  #ggqqplot(x$residuals, main = "QQ plot of residuals")

  
}

residual_normalitiy(x = ulster_lm, y = conn_lm, z = mnstr_lm)

shapiro.test(ulster_lm$residuals)
shapiro.test(conn_lm$residuals)
shapiro.test(mnstr_lm$residuals)


#the residuals are not normally distributed for each of the models
# and further indicate that the models are a poor fit.
# dont explain the behaviour of the data very well





## trying polynomial regression
# similar to linaer regeression

ulster_pr <- lm(Ulster$`Malin Head Rain` ~ Ulster$`Malin Head Temp` 
                + I(Ulster$`Malin Head Temp` ^ 2))
conn_pr <- lm(Connaucht$Mean_Rain ~ Connaucht$Mean_Temp
                + I(Connaucht$Mean_Temp ^ 2))
                
mnstr_pr <- lm(Munster$`Valentia Rain` ~ Munster$`Valentia Temp` 
                + I(Munster$`Valentia Temp` ^ 2))
ulster_pr

summary(ulster_pr)
summary(conn_pr)                 
summary(mnstr_pr)

# again, like linear models, these are not a good fit.
# p values for each model is below the cut off (0.05) and thus H0
# is rejected. This means there is a relationship between the predictor and the 
# dependant variable. The r squared  value is close to zero.
#This means that a lot of the variability has not been explained by the model
# even tho the model is significant, its nto a good fit to the data


#AIC and BIC to determine which model would fit bests; poly or linear
AIC(ulster_lm)
BIC(ulster_lm)
AIC(ulster_pr)
BIC(ulster_pr)
# LINEAR FOR ULSTER 
AIC(conn_lm)
BIC(conn_lm)
AIC(conn_pr)
BIC(conn_pr)
# LINEAR FOR CONNACHT
AIC(mnstr_lm)
BIC(mnstr_lm)
AIC(mnstr_pr)
BIC(mnstr_pr)
# LINEAR FOR MUNSTER

#LINEAR MODEL- ULSTER
# choosing 80% of the rows
records <- sample(1:nrow(Ulster), 0.8 * nrow(Ulster))
# 80/20 split for trainging and testing 
training_data <- Ulster[records,]
testing_data <- Ulster[-records,]

linear_model <- lm(`Malin Head Rain`~`Malin Head Temp`, data = training_data)

summary(linear_model)


# p values are below the cut off so the models are still
# statistically significant. R squared values are comparative to original
# model; low. This means not practically significant 

rain_predict <- predict(linear_model, testing_data)


actual_preds <- data.frame(cbind(actuals = testing_data$`Malin Head Rain`,
                                 predicted = rain_predict))
head(actual_preds)

correlation_accuracy <- cor(actual_preds)
correlation_accuracy


min_max_accuracy <- mean(apply(actual_preds, 1, min) / apply(actual_preds, 1, max))
min_max_accuracy

# MAPE 
mape <- mean(abs((actual_preds$predicted - actual_preds$actuals)) / actual_preds$actuals)
mape

## lINEAR REGRESSION CONNACHT 

# choosing 80% of the rows
records_C <- sample(1:nrow(Connaucht), 0.8 * nrow(Connaucht))
# 80/20 split for trainging and testing 
training_data_C <- Connaucht[records,]
testing_data_C <- Connaucht[-records,]

linear_model_C <- lm(`Mean_Rain`~`Mean_Temp`, data = training_data_C)

summary(linear_model_C)


# p values are below the cut off so the models are still
# statistically significant. R squared values are comparative to original
# model; low. This means not practically significant 

rain_predict_C <- predict(linear_model_C, testing_data_C)

actual_preds_C <- data.frame(cbind(actuals = testing_data_C$Mean_Rain,
                                   predicted = rain_predict_C))
head(actual_preds_C)

cor(actual_preds_C)


mean(apply(actual_preds_C, 1, min) / apply(actual_preds_C, 1, max))
# MAPE 
mean(abs((actual_preds_C$predicted - actual_preds_C$actuals)) / actual_preds_C$actuals)

## Linear Regression Munster

# choosing 80% of the rows
records_M<- sample(1:nrow(Munster), 0.8 * nrow(Munster))
# 80/20 split for trainging and testing 
training_data_M <- Munster[records,]
testing_data_M <- Munster[-records,]

linear_model_M <- lm(`Valentia Rain`~`Valentia Temp`, data = training_data_M)

summary(linear_model_M)


# p values are below the cut off so the models are still
# statistically significant. R squared values are comparative to original
# model; low. This means not practically significant 

rain_predict_M <- predict(linear_model_M, testing_data_M)

actual_preds_M <- data.frame(cbind(actuals = testing_data_M$`Valentia Rain`,
                                   predicted = rain_predict_M))
head(actual_preds_M)

cor(actual_preds_M)


mean(apply(actual_preds_M, 1, min) / apply(actual_preds_M, 1, max))
# MAPE 
mean(abs((actual_preds_M$predicted - actual_preds_M$actuals)) / actual_preds_M$actuals)

# Conclusion
#low p value in the lineare models show there is enough evidence to reject H0 and that
# there is a statistical significant relationship between rainfall and temperature.
# However, the low correlation and lack of explanation for the variance in the data given by the model
# tell us the relationship is  not practically significant 
# the predictive models were 20% accurate which is low. Given that the model diagnostics were indicitive
# of a poor fit, this comes as no surprise.There are most definitely other variables unaccounted for that would
# have affected both the rain and temp data. The fact that the values in eachset were average values means
# the granularity was already effected but these were the only relevant datasets found in relation to this topic.
# A lot of data had to be removed from the dataset as result of missing values, had these values been usable then
# the models would have been more robust in their prediction. The missing values accounted for almost half the weather
# station data in Ireland, this was frustrating as it hindered the initial analysis and the subsequent predictive analysis.
# Heatwaves, unusually heavy downpours or storms would have effected the data and were not taken into account when
# carrying out analysis. These extra variables should be included along with any others that might directly effect rainfall and 
# temperature data if the study was to be carried out again. The missing data might have included information that could have accounted
# for the presence of an unseen variable but unfortunately this is just an assumption. 
# alter research questiona and carry out time series analysis.