# This R code calculates the Chi Squared values for absentee data for the first periods
# of Roosevelt and Franklin schools. In order to run a Chi Square test, you must first
# create a table with expected and observed values. In this case, we are testing to see
# if the tardy data changed between school years (before/after the new start times). We 
# received the data in the form of total tardy counts per year for each school (FT/RT)
# and the tardys as an average per student (FT__S/RT__S).

# The null hypothesis is that the number of tardies did not change between years. The
# alternative hypothesis is that the number of tardies did change between years. Before
# we could compare the observed to the expected, we need to decide what our expected number
# of tardies is. Because of the intervention (SST), we would expect the number of tardies in
# 2016 to be the same as 2017, as PROPORTIONAL to the number of students. This is different
# than the average across students. Thus we must first back-calculate the number of studnets
# that were in each school for both years, then calculate the expected value for 2017

# Naming convention for the variables
# FT__/RT__ <- The raw number of tarides reported for Franklin and Roosevelt, respectively
# FT__S/RT__S <- The number of tardies/student for Franklin and Roosevelt, respectively.
# FS__T/RS__T <- The back calculated number of students in each school for each year. This
#              is calculated by dividing the number of tardies by the number of tardies per
#              student.
# F/R TExpected <- The expected total number of tardies for 2017 if they stay proportional
#                 to the number of tardies in 2016 for each school, respectively. This is 
#                 calculated by cross multiplying and dividing. FS16    FS17
#                                                               ----  = ----
#                                                               FT16     X 
#                 where 'X' is the number of tardies expected

FT16 <- 8591
FT16S <- 6.2
FS16T <- round(FT16/FT16S)

FT17 <- 5992
FT17S <- 4.3
FS17T <- round(FT17/FT17S)

FTExpected <- round((FS17T*FT16)/FS16T)


RT16 <- 6199
RT16S <- 3.5
RS16T <- round(RT16/RT16S)

RT17 <- 6088
RT17S <- 3.4
RS17T <- round(RT17/RT17S)

RTExpected <- round((RS17T*RT16)/RS16T)


## Franklin Tardy
DatFraT <- matrix(c(FTExpected, FT17), nrow = 1, ncol = 2 )
Columns <- c("Expected", "Actual")
colnames(DatFraT) <- Columns
DatFraT <- as.table(DatFraT)

FraTardy <- chisq.test(DatFraT)
FraTardy

## Roosevelt Tardy
DatRooT <- matrix(c(RTExpected, RT17), nrow = 1, ncol = 2 )
Columns <- c("Expected", "Actual")
colnames(DatRooT) <- Columns
DatRooT <- as.table(DatRooT)

RooTardy <- chisq.test(DatRooT)
RooTardy



# Now I will repeat the same set of experiments using absent data instead of tardy data. The
# theory, process, and naming conventions will stay the same. However, instead of 'T' in the
# variable names I will use 'A' for 'Absent'

FA16 <- 21619
FA16S <- 15.5
FS16A <- round(FA16/FA16S)

FA17 <- 18933
FA17S <- 13.6
FS17A <- round(FA17/FA17S)

FAExpected <- round((FS17A*FA16)/FS16A)


RA16 <- 23264
RA16S <- 13
RS16A <- round(RA16/RA16S)

RA17 <- 23089
RA17S <- 12.8
RS17A <- round(RA17/RA17S)

RAExpected <- round((RS17A*RA16)/RS16A)


## Franklin Data Alone
DatFraA <- matrix(c(FAExpected, FA17), nrow = 1, ncol = 2 )
Columns <- c("Expected", "Actual")
colnames(DatFraA) <- Columns
DatFraA <- as.table(DatFraA)

FraAbsent <- chisq.test(DatFraA)
FraAbsent

## Roosevelt Data Alone
DatRooA <- matrix(c(RAExpected, RA17), nrow = 1, ncol = 2 )
Columns <- c("Expected", "Actual")
colnames(DatRooA) <- Columns
DatRooA <- as.table(DatRooA)

RooAbsent <- chisq.test(DatRooA)
RooAbsent