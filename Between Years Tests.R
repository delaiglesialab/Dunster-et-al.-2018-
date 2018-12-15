## This script walks through the analysis of School day sleep parameters using t-tests for parametric data
## and the Wilcox test for non-parametric data. Data are sourced from "Sleep and Survey Summary.csv"

## Testing normality in the variables via QQ Plots
qqnorm(Dat$Average.Weekday.Onset, main = "Weekday Onset")
qqline(Dat$Average.Weekday.Onset)
qqnorm(Dat$Average.Weekday.Offset, main = "Weekday Offset")
qqline(Dat$Average.Weekday.Offset)
qqnorm(Dat$Average.Weekday.Duration, main = "Weekday Duration")
qqline(Dat$Average.Weekday.Duration)
qqnorm(Dat$sleepiness_score, main = "Sleepiness")
qqline(Dat$sleepiness_score)
qqnorm(Dat$mood_score, main = "Mood")
qqline(Dat$mood_score)
qqnorm(Dat$Social.Jetlag, main = "Social Jetlag")
qqline(Dat$Social.Jetlag)
qqnorm(Dat$Social.Jetlag..Fixed., main = "Social Jetlag Fixed")
qqline(Dat$Social.Jetlag..Fixed.)
qqnorm(Dat$MS.Work, main = "Mid Sleep Work")
qqline(Dat$MS.Work)
qqnorm(Dat$S2.Grade)
qqline(Dat$S2.Grade)

## Testing normality via the shapiro test. p > 0.05 means normal
WDOnset <- shapiro.test(Dat$Average.Weekday.Onset)
# p-value = 0.0005428
WDOffset <- shapiro.test(Dat$Average.Weekday.Offset)
# p-value = 1.5e-06
WDDuration <- shapiro.test(Dat$Average.Weekday.Duration)
# p-value = 1.881e-05
SleepinessNorm <- shapiro.test(Dat$sleepiness_score)
# p-value = 0.01279
MoodNorm <- shapiro.test(Dat$mood_score)
# p-value = 1.69e-09
SJLNorm <- shapiro.test(Dat$Social.Jetlag)
# p-value = 0.00659
SJLFNorm <- shapiro.test(Dat$Social.Jetlag..Fixed.)
# p-value = 3.349e-07
MidsleepNorm <- shapiro.test(Dat$MS.Work)
# p-value = 0.1244
GradesNorm <- shapiro.test(Dat$S2.Grade)
# p-value = 3.791e-10

## Viewing a histogram of the data to assess outliers
hist(Dat$Average.Weekday.Onset, main = "Weekday Onset")
hist(Dat$Average.Weekday.Offset, main = "Weekday Offset")
hist(Dat$Average.Weekday.Duration, main = "Weekday Duration")
hist(Dat$sleepiness_score, main = "Sleepiness Scores")
hist(Dat$mood_score, main = "Depression Scores")
hist(Dat$Social.Jetlag, main = "Social Jetlag")
hist(Dat$MS.Work, main = "Mid Sleep Work")

## Testing for homogeneity of variances by Year
WDOnsetHV <- bartlett.test(Dat$Average.Weekday.Onset, Dat$Year)
# p-value = 0.1591
WDOffsetHV <- bartlett.test(Dat$Average.Weekday.Offset, Dat$Year)
# p-value = 0.2055
WDDurationHV <- bartlett.test(Dat$Average.Weekday.Duration, Dat$Year)
# p-value = 0.6804
SleepinessHV <- bartlett.test(Dat$sleepiness_score, Dat$Year)
# p-value = 0.7562
MoodHV <- bartlett.test(Dat$mood_score, Dat$Year)
# p-value = 0.04256
SJLHV <- bartlett.test(Dat$Social.Jetlag, Dat$Year)
# p-value = 0.358
SJLFHV <- bartlett.test(Dat$Social.Jetlag..Fixed., Dat$Year)
# p-value = 0.8969
MSWorkHV <- bartlett.test(Dat$MS.Work, Dat$Year)
# p-value = 0.5299
GradesHV <- bartlett.test(Dat$S2.Grade, Dat$Year)
# p-value = 0.4878


Years <- c("2016", "2017")

## Testing differences in duration between years
Duration <- wilcox.test(Dat$Average.Weekday.Duration ~ Dat$Year, paired = FALSE)
DurationU <- Duration$statistic
Duration
# p-value = 0.0007325575

# According to Grissom and Kim (2012) the effect size for a wolcox test can be calculated by
# dividing the U statistic by the product of the two N's. Or in this case, the product of the
# number of subjects in the 2016 and 2017 datasets (84*94). According to the online source at
# https://stats.stackexchange.com/questions/79843/is-the-w-statistic-output-by-wilcox-test-in-r-the-same-as-the-u-statistic/79853
# When paired = FALSE in the wilcox test, like in our case, then the W statistic that R generates
# is the same as the U statistic. Thus, to get the effect size you divide the W provided by R
# by 7896 (84*94 for the weekday data)
DurationES <- DurationU/(84*94)

# creating a boxplot of the Duration data
boxplot(Average.Weekday.Duration ~ Year, data = Dat, main = "Weekday Duration", xlab = "Year", 
        ylab = "Duration", names = Years, col = c("magenta", "orange"))


## Testing differences in onset between years
Onset <- wilcox.test(Dat$Average.Weekday.Onset ~ Dat$Year)
OnsetU <- Onset$statistic
Onset
# p-value = 0.04594
boxplot(Average.Weekday.Onset ~ Year, data = Dat, main = "Weekday Onset", xlab = "Year", 
        ylab = "Onset", names = Years, col = c("magenta", "orange"))


## Testing differences in offset between years
Offset <-  wilcox.test(Dat$Average.Weekday.Offset ~ Dat$Year)
OffsetU <- Offset$statistic
Offset
# p-value = 3.598e-10

OffsetES <- OffsetU/(84*94)

boxplot(Average.Weekday.Offset ~ Year, data = Dat, main = "Weekday Offset", xlab = "Year", 
        ylab = "Offset", names = Years, col = c("magenta", "orange"))


## Testing differences in mid-sleep on work days between years (t-test because data are 
## normally distributed)
Midsleep <- t.test(Dat$MS.Work ~ Dat$Year)
Midsleep
# p-value = 6.137e-05
boxplot(MS.Work ~ Year, data = Dat, main = "Mid Sleep Work Days", xlab = "Year", 
        ylab = "Mid Sleep", names = Years, col = c("magenta", "orange"))


## Testing differences in sleepiness between years
Sleepiness <-  wilcox.test(Dat$sleepiness_score ~ Dat$Year)
Sleepiness
# p-value = 0.03687
boxplot(sleepiness_score ~ Year, data = Dat, main = "Sleepiness", xlab = "Year", 
        ylab = "Sleepiness", names = Years, col = c("magenta", "orange"))

## Testing differences in SJL between years SLEEP CORRECTED
SJL.Old <-  wilcox.test(Dat$Social.Jetlag ~ Dat$Year)
SJL.OldU <- SJL.Old$statistic
SJL.Old
# p-value = 0.01185

# In 2016 there were 13 NA's in the SJL column out of 94 total observations, thus the N for 2016
# is 94 - 13 = 81. In 2017 there were 8, thus the N for 2017 is 84 - 8 = 76
SLJ.OldES <- SJL.OldU/(81*76)

boxplot(Social.Jetlag ~ Year, data = Dat, main = "Social Jetlag", xlab = "Year", 
        ylab = "SJL", names = Years, col = c("magenta", "orange"))

## Finding the medians for the two years requires that I break the data up by year
Dat16 <- Dat[Dat$Year == 1,]
Dat17 <- Dat[Dat$Year == 2,]

## To calculate the median, I need to manually choose which variable from which year and 
## use the 'median' function

Onset16 <- median(Dat16$Average.Weekday.Onset)
# 11.45240741 or 11:27pm
Onset17 <- median(Dat17$Average.Weekday.Onset)
# 11.641136365 or 11:38pm

Offset16 <- median(Dat16$Average.Weekday.Offset)
# 18.4 or 6:24am
Offset17 <- median(Dat17$Average.Weekday.Offset)
# 19.14 or 7:08am

Duration16 <- median(Dat16$Average.Weekday.Duration)
# 6.834259259 or 6:50 hours
# mean = 6:52
Duration17 <- median(Dat17$Average.Weekday.Duration)
# 7.4016666665 or 7:24 hours
# mean = 7:16

MSWork16 <- median(Dat16$MS.Work, na.rm = T)
# 15.00606061 or 3:00am
MSWork17 <- median(Dat17$MS.Work, na.rm = T)
# 15.4825 or 3:29am

SJL16 <- median(Dat16$Social.Jetlag, na.rm = T)
# 1.6
SJL17 <- median(Dat17$Social.Jetlag, na.rm = T)
# 1.25

S2Grades16 <- median(Dat16$S2.Grade, na.rm = T)
# 77.5
S2Grades17 <- median(Dat17$S2.Grade, na.rm = T)
# 82
S2Grades16 <- mean(Dat16$S2.Grade, na.rm = T)
# 74.6
S2Grades17 <- mean(Dat17$S2.Grade, na.rm = T)
# 76.6


Sleepiness16 <- median(Dat16$sleepiness_score, na.rm = T)
# 7
Sleepiness17 <- median(Dat17$sleepiness_score, na.rm = T)
# 6



## Calculating ages for 2016 and 2017 for each school
# Formatting the birthdates to a correct format
Dat16$Birthday <- as.Date(Dat16$Birthday, format("%m/%d/%y"))
Dat17$Birthday <- as.Date(Dat17$Birthday, format("%m/%d/%y"))

# First day of the study for each year
Start2016 <- as.Date("4/18/2016", format("%m/%d/%Y"))
Start2017 <- as.Date("4/17/2017", format("%m/%d/%Y"))

# Creating data frames for each shool/year
Dat16Roo <- subset(Dat16, Dat16$School..R1..F2. == 1)
Dat16Fra <- subset(Dat16, Dat16$School..R1..F2. == 2)
Dat17Roo <- subset(Dat17, Dat17$School..R1..F2. == 1)
Dat17Fra <- subset(Dat17, Dat17$School..R1..F2. == 2)

# Creating vectors with just the birthday data without missing values
BD2016Roo <- c(Dat16Roo$Birthday)
BD2016Roo <- na.omit(BD2016Roo)

BD2016Fra <- c(Dat16Fra$Birthday)
BD2016Fra <- na.omit(BD2016Fra)

BD2017Roo <- c(Dat17Roo$Birthday)
BD2017Roo <- na.omit(BD2017Roo)

BD2017Fra <- c(Dat17Fra$Birthday)
BD2017Fra <- na.omit(BD2017Fra)

# Calculating a vector of ages by subtracting the birth days from study start dates
Age2016Roo <- round((Start2016-BD2016Roo)/365,2)
Age2016Fra <- round((Start2016-BD2016Fra)/365,2)
Age2017Roo <- round((Start2017-BD2017Roo)/365,2)
Age2017Fra <- round((Start2017-BD2017Fra)/365,2)

# Calculating the average ages
Roo2016 <- mean(Age2016Roo)
Fra2016 <- mean(Age2016Fra)
Roo2017 <- mean(Age2017Roo)
Fra2017 <- mean(Age2017Fra)

# Calculating the SE of the means
SERoo2016 <- sd(Age2016Roo)/sqrt(51)
SEFra2016 <- sd(Age2016Fra)/sqrt(43)
SERoo2017 <- sd(Age2017Roo)/sqrt(41)
SEFra2017 <- sd(Age2017Fra)/sqrt(43)