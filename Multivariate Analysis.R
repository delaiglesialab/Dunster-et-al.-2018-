## Loading packages for later use
library(corrplot)

## Removal of columns that I do not care about for this analysis. I am interested in the
## differences in sleep patterns between years, thus I have removed subject numbers, excess
## grades, etc. Remove the columns as desired

## Taking a quick look at the data, minus my variable of interest (Year) in order to see
## how well my individual explanatory variables are correlated. The more correlated, the 
## greater the need to adjust which variables I include so I avoid multicollinearity
corr.dat <- cor(na.omit(Dat[,-c(1)]))
corrplot(corr.dat,method="color")

## An individual way to compare two variables at a time for how well they are correlated
cor.test(Dat$Year, Dat$School..R1..F2.)
cor.test(Dat$Average.Weekday.Duration, Dat$Average.Weekday.Onset)

## Normalizing or scaling the data so that things like mood_score which has values up to
## 60 do not have a disproportionate effect on my model, since things like duration only
## have values up to 12 +/-

## First I need to separate the variables I want to scale from the variables I do not
Temp.Dat <- Dat[,-c(1:2)]
Dat.Year.School <- Dat[, -c(3:21)]

## Before this, Year values were either 1 or 2. Below, the glm will require that they be
## between 0 and 1, therefore I am simply changing all the 1's to 0's and 2's to 1's. The
## information will be the same, but the glm will be able to use it in the function
Dat.Year.School$Year[Dat.Year.School$Year == 1] <- 0
Dat.Year.School$Year[Dat.Year.School$Year == 2] <- 1


## The scale function can only operate on numeric variables, therefore I need to change
## several columns from 'integers' to 'numeric' before scaling the data
Temp.Dat[16:19] <- lapply(Temp.Dat[16:19], as.numeric)

## Then I scale only the data I have selected in Dat1
scale.dat <- scale(Temp.Dat)

## Finally, I want to put the variables back together into one matrix which I can do with
## cbind since the two individual matrices have the same number of rows
Dat2 <- cbind(Dat.Year.School, scale.dat)

## If I want to see what variables have the most missing values, I can use the missmap 
## function to plot a graph to visually inspect what variables are missing data
library(Amelia)
missmap(Dat2, main = "Missing values vs Observed")


## Running hypothesis-driven models to assess differences between years when considering
## more than explanatory variable at a time. Multiple models were needed to control for 
## multiple collinearity, thus Onset, Offset, and Duration are given deparate models AND
## Onset/Offset are NOT considered with Mid-Sleep on Work Days (MS.Work) as midsleep is 
## directly calculated from onset/offset

## Model with the lowest AIC value + only 1 sleep variable
model4 <- glm(Year ~ School..R1..F2.+Average.Weekday.Offset+
              horne_type+mood_score+sleepiness_score+S2.Grade,family = binomial, data = Dat2)
summary(model4)
exp(coefficients(model4))