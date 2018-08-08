library("PKPDmisc")
library("dplyr")
library("tidyverse")
library("xlsx")
library("ggplot2")

setwd("/Users/praneeth/Documents/umb/vancoR")
vancdata <- read.csv("Vancomycin_NONMEM_TY.csv")

data1 <-read.csv("Pediatric Vancomycin_deid.csv")
data2 <-read.csv("Vancomycin_Levels.csv")
data3 <-read.csv("cohort_demo.csv")

str(data1)
#making cohort demo more concise
head(data3)
names(data3)
cohort_demo <- select(data3, X:X.29, Delivery.Encounter.Data:X.113)
head(cohort_demo)
#fixing column names
class(cohort_demo[1,3])
colnames(cohort_demo) <- as.character(unlist(cohort_demo[1,]))
cohort_demo <- cohort_demo[-1,]


cohort_demo$PT_ID <- as.numeric(as.character(cohort_demo$PT_ID)) #numeric PT_ID
cohort_demo$PATIENT_ID <- as.numeric(as.character(cohort_demo$PATIENT_ID)) #numeric PATIENT_ID
cohort_demo$PATIENT_ENCNTR <- as.numeric(as.character(cohort_demo$PATIENT_ENCNTR)) #numeric PATIENT_ENCNTR 
#looking at cohort demo
str(cohort_demo$SEX_CD)
summary(cohort_demo$SEX_CD)
any(is.na(cohort_demo))

which(cohort_demo$SEX_CD == "")
levels (cohort_demo[6260,])
cohort_demo1 <- cohort_demo[-c(6260,6261,6262), ]

#AGE
cohort_demo1$ADMIT_AGE_DYS <- (as.numeric(as.character(cohort_demo1$ADMIT_AGE_DYS))) / 365
ggplot(cohort_demo1, aes(ADMIT_AGE_DYS)) + geom_histogram()

#Ethnicity
levels(cohort_demo1$ETHNICITY_CD)
missing_ethnicity <- which(cohort_demo1$ETHNICITY_CD == "")
ID_missing_ethnicity <- cohort_demo1$PT_ID[missing_ethnicity]

#Race
levels(cohort_demo1$RACE_DSC)
summary(cohort_demo1$RACE_DSC)

#narrow down cohort_demo even more 
names(cohort_demo1)
cohort_demo2 <- cohort_demo1[,-c(4:6,8,15:33)]

names(cohort_demo2)[3] <- "ENCT"
names(cohort_demo2)[4] <- "AGE"

#Data2
str(data2)
levels(data2$RESULT_TXT)
str(data2$RESULT_TXT)
misslvl1 <- which(data2$RESULT_TXT == "")
containlvl1 <- which(data2$RESULT_TXT != "")

misslvl_ID <- data2$PATIENT_ID[misslvl1]
containlvl_ID <- data2$PATIENT_ID[containlvl1]

nolevels_pts <- setdiff(misslvl_ID, containlvl_ID) #patients with no vanco levels

#finding which rows have those patient IDs
new_vec <- c(0,0)
x <- 1
while(x <= length(nolevels_pts)) {
  new_vec[x] <-which(data2$PATIENT_ID == nolevels_pts[x])
  x = x + 1
}
length(new_vec)
length(nolevels_pts)

peak_tro_data <- data2[-c(new_vec),] #dataset with patients who have no levels removed



#Data1
names(data1)
summary(data1$HEIGHT_AMT)
which(is.na(data1$HEIGHT_AMT))

#identifying rows with patients with missing levels
new_vec2 <- which(data1$PATIENT_ID == nolevels_pts[1])
x <- 2
while(x <= length(nolevels_pts)) {
  new_vec2 <- append(new_vec2, which(data1$PATIENT_ID == nolevels_pts[x]), after = length(new_vec2))
  x = x + 1
}
length(new_vec2)

dosing_data <- data1[-c(new_vec2), ] #dataset with patients who have no levels removed


#combining it all
names(dosing_data)
dosing_data <- dosing_data[,-c(4:5,7:11,14:18)]
names(peak_tro_data)
peak_tro_data <- peak_tro_data[,-c(5:6,8)]
names(cohort_demo2)
cohort_demo2 <- cohort_demo2[,-c(5,9:10)]
names(cohort_demo2)[3] <- "PATIENT_ENCNTR"
mergedata <- merge(dosing_data, peak_tro_data, by = c("PT_ID", "PATIENT_ID", "PATIENT_ENCNTR"))
mergedata <- arrange(mergedata, PT_ID, PATIENT_ID, PATIENT_ENCNTR)
mergedata <- merge(mergedata, cohort_demo2, by = c("PT_ID", "PATIENT_ID", "PATIENT_ENCNTR"))
mergedata <- arrange(mergedata, PT_ID, PATIENT_ID, PATIENT_ENCNTR)
names(mergedata)
mergedata <- mergedata[, c(1,2,3,4,9,5,6,10,12,13,14,11,7,8)]


#Drug Name
summary(dosing_data$DRUG_NM)
summary(dosing_data$GIVEN_STATUS_CD)
summary(dosing_data$COUNT1_AMT)

#Vancdata
str(vancdata$AMT)
levels(vancdata$AMT)
which(vancdata$AMT == ".")

v_containlvl1 <- which(vancdata$DV != ".")

v_containlvl_ID <- vancdata$PTNT[v_containlvl1]


#Merging data from the 1 & 2 
mergedata <- merge(data1, data2, by = c("PT_ID", "PATIENT_ID","PATIENT_ENCNTR"))
mergedata <- arrange(mergedata, PT_ID, PATIENT_ID, PATIENT_ENCNTR)

#Column names
columnvanc <- names(vancdata)

#looking at mergedata
head(mergedata)
summary(mergedata)

