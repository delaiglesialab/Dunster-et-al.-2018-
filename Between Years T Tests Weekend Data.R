## This script walks through the analysis of Free Day sleep parameters using t-tests for parametric data
## and the Wilcox test for non-parametric data. Data are sourced from "Sleep and Survey Summary.csv"

## Testing normality in the variables via QQ Plots
qqnorm(Dat$Average.Weekend.Duration, main = "Weekend Duration")
qqline(Dat$Average.Weekend.Duration)

qqnorm(Dat$Average.Weekend.Onset, main = "Weekend Onset")
qqline(Dat$Average.Weekend.Onset)

qqnorm(Dat$Average.Weekend.Offset, main = "Weekend Offset")
qqline(Dat$Average.Weekend.Offset)


## Testing normality via the shapiro test. p > 0.05 means normal
WEOnset <- shapiro.test(Dat$Average.Weekend.Onset)
WEOnset
# p-value = 0.8644

WEOffset <- shapiro.test(Dat$Average.Weekend.Offset)
WEOffset
# p-value = 0.6335

WEDuration <- shapiro.test(Dat$Average.Weekend.Duration)
WEDuration
# p-value = 3.938e-05


## Testing for homogeneity of variances by Year
WEOnsetHV <- bartlett.test(Dat$Average.Weekend.Onset, Dat$Year)
WEOnsetHV
# p-value = 0.3276

WEOffsetHV <- bartlett.test(Dat$Average.Weekend.Offset, Dat$Year)
WEOffsetHV
# p-value = 0.5538

WEDurationHV <- bartlett.test(Dat$Average.Weekend.Duration, Dat$Year)
WEDurationHV
# p-value = 0.05146


Years <- c("2016", "2017")

## Due to the multiple comparisons, the threshold for significant will be lower than
## p = 0.05. According to Bonferroni, significance will be where p < 0.017

## Testing differences in duration between years
Duration <- wilcox.test(Dat$Average.Weekend.Duration ~ Dat$Year)
Duration
# p-value = 0.1211
boxplot(Average.Weekend.Duration ~ Year, data = Dat, main = "Weekend Duration", xlab = "Year", 
        ylab = "Duration", names = Years, col = c("red", "blue"))


## Testing differences in onset between years
Onset <- t.test(Dat$Average.Weekend.Onset ~ Dat$Year)
Onset
# p-value = 0.0391
boxplot(Average.Weekend.Onset ~ Year, data = Dat, main = "Weekend Onset", xlab = "Year", 
        ylab = "Onset", names = Years, col = c("red", "blue"))

## Testing differences in offset between years
Offset <-  t.test(Dat$Average.Weekend.Offset ~ Dat$Year)
Offset
# p-value = = 0.4009
boxplot(Average.Weekend.Offset ~ Year, data = Dat, main = "Weekend Offset", xlab = "Year", 
        ylab = "Offset", names = Years, col = c("red", "blue"))


## Finding the medians for the two years requires that I break the data up by year
Dat16 <- Dat[Dat$Year == 1,]
Dat17 <- Dat[Dat$Year == 2,]

Onset16 <- median(Dat16$Average.Weekend.Onset)
# 12.304166665 or 12:18am
Onset17 <- median(Dat17$Average.Weekend.Onset)
# 12.72875 or 12:43
# Difference of 25 minutes

Offset16 <- median(Dat16$Average.Weekend.Offset)
# 20.5725 or 8:34am
Offset17 <- median(Dat17$Average.Weekend.Offset)
# 20.76041667 or 8:45am
# Difference of 11 minutes

Duration16 <- median(Dat16$Average.Weekend.Duration)
# 8.35625 or 8:21 hours
Duration17 <- median(Dat17$Average.Weekend.Duration)
# 8.0966666665 or 8:05 hours
# Difference of -16 minutes