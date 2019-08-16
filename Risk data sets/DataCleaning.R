#Final datasets produced for model
#TBBcount - count of takeback boxes: 
str(TBBcount) #Merge on FIPScode, take in 2,3
#EDODfinal - emergency department overdoses - any OD and Opioid OD, 2017
str(EDODfinal) #take in 2-6, merge on County.Code.Number
#SnAfinal - Seizure and arrest records - amount seized (fent, no fent, overall), n arrests, n incidents
str(SnAfinal) #merge on FIPScode, take in 2-7 
#DrugDispFinal - 2017 total annual rate of opioid dispensation per 1000 excluding bupenorphrine
str(DrugDispFinal) #keep 2-3, merge on County.Code.Text
#MAwOUD - 2017 N MA members w OUD dx
str(MAwOUD) #keep 3, 4, merge on County.Code.Number
#MAwMAT - 2016 N MA members on MAT
str(MAwMAT) #keep 3, 4 merge on County.Code.Number
#MAwNarcan - 2016 N MA members on Narcan
str(MAwNarcan) #Keep Count, Count.Description, merge on County.Code
#MAenrollFinal - includes avg monthly enrollment for kids and adults in 2016 and 2017
str(MAenrollFinal) #2:6, merge on County.Code
#PopDemos2016 - census pop of each county for CY2016, including age, gender, race breakdowns
str(PopDemos2016) #keep all, mereg on county.code - 2016 ACS demos
#CountyCrashData - 2017, impaired deaths
str(CountyCrashData)
#Dropout - school year 2016-17 drop out rates
str(Dropout) #keep 2,3,4 merge on county.code
#HomelessPIT - 2017 Homeless PIT data
str(HomelessPIT) # 2, 3 merge on County.code
#BupDocsFinal - N Bup docs by county, current
str(BupDocsFinal) #2,3 merge on countycode
#ChildAlleg - rate of children with valid allegations per 1000
str(ChildAllegations) #keep 2,3 merge on county.code
#PoliceNalox - naloxone reversals by municipal police depts; reporting is voluntary, not everyone has a municipal police dept (N=4 counties)
str(PoliceNalox)#keep 2-4, merge on County.code
#HIV - prevalence of HIV and HIV due to IDU, 2016 (i also made some features for ratio of the 2)
str(HIV) #, merge on County.Code.Number, keep 2:6
#HEPc - newly dx hepc in 15 to 34 2016
str(HEPc) #2-3, merge on County.Code
#MedInc - median income in 2016
str(MedInc) # 2-4, merge on countyID
#ODdeath_DEA - OD death data by county in PA (3 counties Missing), 2016
str(ODdeath_DEA) # keep 2-5, merge on county.code
#CountySA - SA facilities listed in Samhsa, counted by county. include some special features, current
str(CountySA) #keep 2-6, merge on countycode
#Warmline1 - total intake calls in 2017
str(WarmLine1) #keep 2 and 3, merge on countycode
#MHtx - mh facilities listed on samhsa, current
str(MHtx) #keep 2, 3 merge on countycode

####master list of county codes, w upper and lower
MasterCC <- read.csv("MasterCountyList.csv")


##############################
#Cleaning Take Back Boxes    # 
##############################

#load raw takeback data, pivot to unique county
TakeBackBoxesraw<-read.csv("PIVOT FOR COUNTS BY COUNTY Prescription_Drug_Take-Back_Box_Locations_County_Drug_and_Alcohol_Programs.csv")
str(TakeBackBoxesraw)
summary(TakeBackBoxesraw)
table(TakeBackBoxesraw$County, TakeBackBoxesraw$FIPS.County.Code)

#trimming white space on county, as lancaster is duplicated
TakeBackBoxesraw$County <- trimws(TakeBackBoxesraw$County, "right")

#basic table for county and sum of boxes

TBBcount<-as.data.frame(table(TakeBackBoxesraw$County))
colnames(TBBcount)<-c("county","NTakeBackBoxes")
TBBcount

#adding in unique FIPS codes
FIPScode<-TakeBackBoxesraw$FIPS.County.Code[!duplicated(TakeBackBoxesraw$FIPS.County.Code)] #creates unique set

TBBcount<-cbind(TBBcount, FIPScode)


##############################
#Cleaning ED visits for OD   # 
##############################

EDOD<-read.csv("QTRLY RATES Emergency_Department__ED__Visits_for_Overdose_Identified_Through_Syndromic_Surveillance_2016_Q3_-_Current_County_Health.csv")
EDOD$Year<-as.factor(EDOD$Year)
str(EDOD)

#limit to 2017
EDOD<-EDOD[EDOD$Year == 2017,]
dim(EDOD)
#remove lines with -2 and -3
EDOD$Quarterly.Rate[EDOD$Quarterly.Rate < 0]
EDOD <- EDOD[EDOD$Quarterly.Rate >= 0,]
dim(EDOD)
 
summary(EDOD$Quarterly.Rate)

#limiting data to rate info only
EDOD <- EDOD[EDOD$Type.of.Rate == "Rate of Emergency Department Visits Related to Overdose per 1,000 Population", ]

#Overall OD rate
EDODfinal <- EDOD[EDOD$Overdose.Type == "Any Drug Overdose",]
EDODfinal <- aggregate(EDODfinal$Quarterly.Rate, list(EDODfinal$County.Name), sum)
colnames(EDODfinal) <- c("county", "AnyDrugODrate1000")
head (EDODfinal)
dim(EDODfinal)

#Opioid OD rate
EDODood <- EDOD[EDOD$Overdose.Type == "Any Opioid Overdose",]
EDODood <- na.exclude(EDODood)
EDODood <- aggregate(EDODood$Quarterly.Rate, list(EDODood$County.Name), sum)
colnames(EDODood) <- c("county", "AnyOpioidODrate1000")
head (EDODood)
dim(EDODood)

###count of time periods
#count for any OD
EDOD1 <- EDOD[EDOD$Overdose.Type == "Any Drug Overdose",]
countAODqs <- as.data.frame(table(EDOD1$County.Name))
colnames(countAODqs) <- c("county", "AnyOD_NQsWData")
head(countAODqs)

EDOD2 <- EDOD[EDOD$Overdose.Type == "Any Opioid Overdose",]
EDOD2<- na.exclude(EDOD2)

countOODqs<-as.data.frame(table(EDOD2$County.Name))
colnames(countOODqs) <- c("county", "OpOD_NQsWData")
head(countOODqs)

#merging count columns back into EDODfinal
EDODfinal <- merge(EDODfinal, countAODqs, by.x = "county", by.y = "county")
EDODfinal <- merge(EDODfinal, countOODqs, by.x = "county", by.y = "county")
EDODfinal <- merge(EDODfinal, EDODood, by.x = "county", by.y = "county", all.x = TRUE)

#new col names to clearly identify as ED data
colnames(EDODfinal) <- c("county", "ED_AnyDrugODrate1000", "ED_AnyOD_NQswData", "ED_OpOD_NQswData", "ED_AnyOpioidODrate")

#adding FIPS
EDODfinal <- merge(EDODfinal, EDOD[c(1,10 )], by.x = "county", by.y = "County.Name", all.x = TRUE)
EDODfinal<- EDODfinal[!duplicated(EDODfinal),]
EDODfinal <- EDODfinal[EDODfinal$county != "PENNSYLVANIA",]
head(EDODfinal)

#########################################
#Cleaning Opioid Seizures and arrests   # 
#########################################

SnA<-read.csv("QTRYL Opioid_Seizures_and_Arrests_Year_2013_-_June_2018_County_State_Police.csv")
SnA<-SnA[SnA$Year == 2017,]
str(SnA)
summary(SnA$Qtr.Start.Date)

#creating an incident DF
SnA1<-aggregate(SnA$Incident.Count, list(SnA$County.Name), sum) #using this instead of tapply, tapply was giving me NAs and returning it in a weird format that didn't have 2 columns 
colnames(SnA1)<-c("county","NSeizurenArrestIncidents")
sum(SnA1$NSeizurenArrestIncidents)#check against excel pivot

#creating an amount DF
SnA2<-aggregate(SnA$Drug.Quantity, list(SnA$County.Name), sum) #using this instead of tapply, tapply was giving me NAs and returning it in a weird format that didn't have 2 columns 
colnames(SnA2) <- c("county","KGseizedAllOpioid")
sum(SnA2$KGseizedAllOpioid)#check against excel pivot

#creating an amount DF, fentanyl only
SnAfent<-SnA[SnA$Drug == "Fentanyl",]
summary(SnAfent)
SnA4<-aggregate(SnAfent$Drug.Quantity, list(SnAfent$County.Name), sum) #using this instead of tapply, tapply was giving me NAs and returning it in a weird format that didn't have 2 columns 
colnames(SnA4) <- c("county","KGseizedFent")
sum(SnA4$KGseizedFent)#check against excel pivot



#Creating an amount DF, non-fentanly only
SnAnotfent<-SnA[SnA$Drug != "Fentanyl",]
summary(SnAnotfent)
SnA5<-aggregate(SnAnotfent$Drug.Quantity, list(SnAnotfent$County.Name), sum) #using this instead of tapply, tapply was giving me NAs and returning it in a weird format that didn't have 2 columns 
colnames(SnA5) <- c("county","KGseizedNotFent")
sum(SnA5$KGseizedNotFent)#check against excel pivot

#Creating an arrest DF
SnA6<-aggregate(SnA$Arrests, list(SnA$County.Name), sum) #using this instead of tapply, tapply was giving me NAs and returning it in a weird format that didn't have 2 columns 
colnames(SnA6) <- c("county","NArrests")
sum(SnA6$NArrests)#check against excel pivot


####aggregating the final dataset

SnAfinal<-merge(SnA1, SnA2, by.x = "county", by.y = "county")
SnAfinal<-merge(SnAfinal, SnA4, by.x = "county", by.y = "county", all.x = TRUE)
SnAfinal<-merge(SnAfinal, SnA5, by.x = "county", by.y = "county")
SnAfinal<-merge(SnAfinal, SnA6, by.x = "county", by.y = "county")

#bringing in FIPS codes
SnAfinal<-merge(SnAfinal, TBBcount[c(1,3)], by.x = "county", by.y = "county", all.x = TRUE)
head(SnAfinal)

#replacing NA's with 0 for fentanyl
SnAfinal$KGseizedFent[is.na(SnAfinal$KGseizedFent)]<-0

#############################
# Opioid dispensation data  #
#############################

DrugDisp<-read.csv("Quarterly - Opioid_Dispensation_Data_County_Quarter_3_2016_-_Current_Health.csv")
str(DrugDisp)
#filtering down the data
DrugDisp <- DrugDisp[DrugDisp$Year == 2017,]
DrugDisp <- DrugDisp[DrugDisp$Type.of.Drug.Class == "Opioids (All Schedules) - excluding Buprenorphine",]
DrugDisp <- DrugDisp[DrugDisp$Type.of.Rate.or.Count.Measure =="Dispensations per 1,000 Population",]

#building final dataset, getting annual rate
DrugDispFinal <- aggregate(DrugDisp$Rate.or.Count, list(DrugDisp$County.Name), sum)
colnames(DrugDispFinal) <- c("county", "AnnualDispPer1000")
head(DrugDispFinal,50) 
DrugDispFinal <- merge(DrugDispFinal, DrugDisp[c(1,10)], by.x = "county", by.y = "County.Name")
#unduplicate from merge above
DrugDispFinal <- DrugDispFinal[!duplicated(DrugDispFinal),]
#remove Pennsylvania counts
DrugDispFinal <- DrugDispFinal[!DrugDispFinal$county == "PENNSYLVANIA",]

DrugDispFinal

########################################## 
# MA Enrollment  #
##########################################

MAenroll<-read.csv("Medical_Assistance_Enrollment_July_2003_-_Current_Human_Services.csv")

#trimming down to 2017 only, and excluding overall state numbers
MAenroll <- MAenroll[MAenroll$Calendar.Year == 2017,]
MAenroll <- MAenroll[MAenroll$County.Code > 0,]
str(MAenroll)
summary(MAenroll$Calendar.Year)
summary(MAenroll$County.Code)

MAenrollAll <- aggregate(MAenroll$MA.Individuals, list(MAenroll$County.Name), mean)
colnames(MAenrollAll) <- c("county", "AvgMonthlyEnroll2017")
MAenrollAll

MAenrollKids <- aggregate(MAenroll$MA.Children, list(MAenroll$County.Name), mean)
colnames(MAenrollKids) <- c("county", "AvgKidsMonthlyEnroll2017")
MAenrollKids

MAenrollFinal <- merge (MAenrollAll, MAenrollKids, by.x = "county", by.y = "county")
MAenrollFinal <- merge (MAenrollFinal, MAenroll[c("County.Name","County.Code")], by.x = "county", by.y = "County.Name")
MAenrollFinal <- MAenrollFinal[!duplicated(MAenrollFinal),]
MAenrollFinal

###creating 2016 dataset
MAenroll<-read.csv("Medical_Assistance_Enrollment_July_2003_-_Current_Human_Services.csv")
MAenroll <- MAenroll[MAenroll$Calendar.Year == 2016,]
MAenroll <- MAenroll[MAenroll$County.Code > 0,]
str(MAenroll)
summary(MAenroll$Calendar.Year)
summary(MAenroll$County.Code)

MAenrollAll16 <- aggregate(MAenroll$MA.Individuals, list(MAenroll$County.Name), mean)
colnames(MAenrollAll16) <- c("county", "AvgMonthlyEnroll2016")
MAenrollAll16

MAenrollKids16 <- aggregate(MAenroll$MA.Children, list(MAenroll$County.Name), mean)
colnames(MAenrollKids16) <- c("county", "AvgKidsMonthlyEnroll2016")
MAenrollKids16

###adding 2016 back into master dataset
MAenrollFinal <- merge(MAenrollFinal, MAenrollAll16, by.x = "county", by.y = "county")
MAenrollFinal <- merge(MAenrollFinal, MAenrollKids16, by.x = "county", by.y = "county")
MAenrollFinal


#################################
# BupDocs in 2018 by county     #
#################################
BupDocs <- read.csv("SAMHSA Bupenorphrine physicians_2018_10_05_124406.csv")
head(BupDocs)
BupDocs$county <- toupper(BupDocs$county) #address counties that are lowercase so i can merge in county code
BupDocsFinal <- as.data.frame(table(BupDocs$county))
BupDocsFinal <- BupDocsFinal[BupDocsFinal$Var1 != "" & BupDocsFinal$Var1 != "-",]
colnames(BupDocsFinal) <- c("county", "NBupDocs") 
head(BupDocsFinal)

BupDocsFinal <- merge(BupDocsFinal, MasterCC[c(2,3)], by.x = "county", by.y = "countyUpper", all.y = TRUE  )
BupDocsFinal$NBupDocs[is.na(BupDocsFinal$NBupDocs)] <- 0 #replace NA with 0s for counties that had no record of a bup doc

BupDocsFinal


#######################
# Warm line calls 2017#
#######################
WarmLine <- read.csv("WEEKLY Get_Help_Now_Intake_Hotline_County_Drug_and_Alcohol_Programs.csv")
head(WarmLine)
str(WarmLine)
?as.Date
WarmLine$Week.Begin.Date <- as.Date(WarmLine$Week.Begin.Date, "%m/%d/%Y")

WarmLine$Year <- format(as.Date(WarmLine$Week.Begin.Date , format="%d/%m/%Y"),"%Y")  #extracting date

WarmLine1 <- WarmLine[c("County.Name", "County.Code", "Year", "Total.Intakes")]
WarmLine1 <- WarmLine1[WarmLine1$Year == 2017,]
WarmLine1$County.Name <- trimws(WarmLine1$County.Name, "right")
WarmLine1 <- WarmLine1[WarmLine1$County.Name != "Unknown" & WarmLine1$County.Name != "Commonwealth",]
head(WarmLine1)

WarmLine1 <- aggregate(WarmLine1$Total.Intakes, list(WarmLine1$County.Name), sum)
head(WarmLine1)
colnames(WarmLine1) <- c("county", "Nintakecalls2017")
WarmLine1 <- merge(WarmLine1, MasterCC[c(1,3)], by.x = "county", by.y = "county")
##############################################
# law enforcement naloxone reversals in 2017 #
##############################################

PoliceNalox <- read.csv("Successful_Naloxone_Reversals_by_Law_Enforcement_Years_2014_-_June_2018_County_Drug_and_Alcohol_Program.csv")
PoliceNalox <- PoliceNalox[PoliceNalox$Year == 2017,]
head(PoliceNalox)
PoliceNalox <- PoliceNalox[c("County.Name", "County.code", "Police.Coverage", "Number.of.successful.reversals")]
str(PoliceNalox)
summary(PoliceNalox$Police.Coverage)
summary(PoliceNalox$County.code)
PoliceNalox <- PoliceNalox[PoliceNalox$County.code>=1 & !is.na(PoliceNalox$County.code) ,]

#checking out NA's in reversals
PoliceNalox[is.na(PoliceNalox$Number.of.successful.reversals),] #NAs are all cases w no municipal police, change to 0
PoliceNalox$Number.of.successful.reversals[is.na(PoliceNalox$Number.of.successful.reversals)] <- 0
#making sure NA's have been replaced correctly
PoliceNalox[PoliceNalox$County.Name == "Forest" | PoliceNalox$County.Name == "Fulton" | PoliceNalox$County.Name == "Juniata" | PoliceNalox$County.Name == "Sullivan",]
summary(PoliceNalox)

###############################################################
# SA facilities, excluding NOOP = 1 (doesn't offer opioid tx) #
###############################################################

SATx <- read.csv("SAMHSA SA provider list Behavioral_Health_Treament_Facility_listing_2018_09_19_122512.csv")
str(SATx) 
SATx[is.na(SATx)] <- 0
SATx$AnyMAT <- ifelse(SATx$mu == 1| SATx$bu == 1 | SATx$nu == 1 | SATx$bum == 1 |
                        SATx$bmw == 1 | SATx$otp == 1 | SATx$mm == 1 | SATx$mmw == 1 |
                        SATx$ub == 1 | SATx$un == 1 |  SATx$rpn == 1 | SATx$pain == 1 | 
                        SATx$meth == 1 | SATx$bsdm == 1 | SATx$bwn == 1 | 
                        SATx$bwon == 1 | SATx$beri == 1 | SATx$nxn == 1 | SATx$vtrl == 1 |
                        SATx$bmo == 1 | SATx$mo == 1, 1, 0)

table(SATx$AnyMAT) #provides OUD MAT only
SATx$AcceptsorprovidesMAT <- ifelse(SATx$moa == 1 | SATx$AnyMAT == 1, 1, 0)
table(SATx$AcceptsorprovidesMAT)
SATx <- SATx[SATx$noop != 1,] #removing providers listed as not treating OUD; cases that have mat but dont tx OUD use naltrexone (also for alcohol)
SATx$Accredited <- ifelse (SATx$carf == 1 | SATx$coa == 1 | SATx$hfap == 1 | SATx$jc == 1 | SATx$ncqa == 1, 1, 0) #includes commission on accred of rehab fac, council on accreditation, healthcare facilities accred program, JC, NCQA
table(SATx$Accredited)
head(SATx)

SATxshort <- subset(SATx, select = c("county", "Accredited", "AnyMAT", "AcceptsorprovidesMAT" ))
head(SATxshort)

#creating unique counts by county
CountySA <- as.data.frame(table(SATx$county))
colnames(CountySA) <- c("county", "N.SA.Facilities")
#adding in county code
CountySA <- merge(CountySA, MasterCC[c(1,3)], by.x = "county", by.y = "county", all.y = TRUE )
#assigning a 0 to counties w no providers
CountySA$N.SA.Facilities[is.na(CountySA$N.SA.Facilities)] <- 0
CountySA

###creating unique counts for other variables
str(SATxshort)

countyAnyMAT <- aggregate(SATxshort$AcceptsorprovidesMAT, list(SATx$county), sum)
colnames(countyAnyMAT) <- c("county", "NAlloworOfferMAT")
head(countyAnyMAT)
CountySA <- merge(CountySA, countyAnyMAT, by.x = "county", by.y = "county", all.x = TRUE)

countyinhouseMAT <- aggregate(SATxshort$AnyMAT, list(SATx$county), sum)
colnames(countyinhouseMAT) <- c("county", "NOfferMAT")
head(countyinhouseMAT)
CountySA <- merge(CountySA, countyinhouseMAT, by.x = "county", by.y = "county", all.x = TRUE)

countyAccred <-aggregate(SATxshort$Accredited, list(SATx$county), sum)
colnames(countyAccred) <- c("county", "NAccred")
head(countyAccred)
CountySA <- merge(CountySA, countyAccred, by.x = "county", by.y = "county", all.x = TRUE)

head(CountySA)
CountySA$NAlloworOfferMAT[is.na(CountySA$NAlloworOfferMAT)] <- 0
CountySA$NOfferMAT[is.na(CountySA$NOfferMAT)] <- 0
CountySA$NAccred[is.na(CountySA$NAccred)] <- 0

summary(CountySA)
###############################################################

########################################## 
# Datasets that just need some subsetting#
##########################################

#######MA individuals w OUD dx, 2017
MAwOUD <- read.csv("Individuals_Under_Medical_Assistance_Diagnosed_with_Opioid_Use_Disorder_CY_2015-2017_County_Human_Services.csv")
head(MAwOUD)
MAwOUD <- MAwOUD[MAwOUD$Year == 2017,c("County.Name","Year","Count","County.Code.Number")]
MAwOUD <- MAwOUD[MAwOUD$County.Code.Number > 0,]
colnames(MAwOUD)[3] <- "N_MAw_OUD"
MAwOUD

#######MA individuals on MAT, 2016
MAwMAT <- read.csv("MA individuals using MAT count 2015 2016_County_Human_Services.csv")
head(MAwMAT)

MAwMAT <- MAwMAT[MAwMAT$Year == 2016,c("County.Name","Year","Count.of.Individuals","County.Code.Number")]
MAwMAT <- MAwMAT[MAwMAT$County.Code.Number > 0,]
summary(MAwMAT)
colnames(MAwMAT)[3] <- "N_MA_MAT"

#looking at neg coded values
table(MAwMAT$N_MA_MAT[MAwMAT$N_MA_MAT <= 0]) # n cases where N <= 0
#replace with NA
MAwMAT$N_MA_MAT[MAwMAT$N_MA_MAT < 0] <- NA
summary(MAwMAT$N_MA_MAT)

######Members on MA w narcan script filled, 2016
MAwNarcan <- read.csv("Number_Of_Individuals_Under_Medicaid_Filling_Naloxone_Prescription_CY_2015-2016_County_Human_Services.csv")
str(MAwNarcan)
MAwNarcan <- MAwNarcan[MAwNarcan$Year == 2016, c("County.Name", "Count", "Count.Description", "Number.of.Records", "County.Code")]
summary(MAwNarcan)
MAwNarcan <- MAwNarcan[MAwNarcan$County.Code > 0,]
#NAs on count are suppressed data, 0s are reported as 0s

#####SY 2016-2017 dropout volume and rates (denom based on snapshot of school enrollment)
Dropout <- read.csv("SY2016 Dropout Rates.csv")
colnames(Dropout) <- c("county", "NdropoutsSY16", "DropoutRatesSY16", "county.code")
head(Dropout)

#####PAcountypopulation in 2018
CountyPop <- read.csv("PAcountypopulation2018.csv")
head(CountyPop)
str(CountyPop)
colnames(CountyPop) <- c("county","Npop2018", "growthsince2010", "county.code")

#####CountyCrash Data
CountyCrashData <- read.csv("CountyCrashData.csv")
head(CountyCrashData)

#####PIT homeless count, 2017
HomelessPIT <- read.csv("PIThomeless.csv")
head(HomelessPIT)

#####Valid child protective services allegations per 1000, 2017
ChildAllegations <- read.csv("ValidChildAllegations.csv")
head(ChildAllegations)
colnames(ChildAllegations) <- c("county", "RateChildAlleg", "county.code")

#####HIV prevalence
HIV <- read.csv("Estimated_Prevalence_and_New_Diagnoses_of_HIV_and_HIV_among_Injection_Drug_Users_by_County__2012-2016__Health.csv")
str(HIV)
HIV <- HIV[HIV$Year == 2016 , c("County.Name", "County.Code.Number", "Prevalence.HIV.Disease.Count", "Prevalence.HIV.Disease.Among.IDU.Count")]
summary(HIV)
##NA cases for Prev and IDU prev are suppressed data; couldnt find documention on threshold for surpression

#adding ratio for IDU to overall
HIV$RatioIDUtoAll <- HIV$Prevalence.HIV.Disease.Among.IDU.Count/HIV$Prevalence.HIV.Disease.Count
hist(HIV$RatioIDUtoAll) #checked out a more detailed figure in excel, huntingdon has the max
summary(HIV$RatioIDUtoAll)
sd(HIV$RatioIDUtoAll, na.rm = TRUE)
minSDHIV <- mean(HIV$RatioIDUtoAll, na.rm = TRUE) - sd(HIV$RatioIDUtoAll, na.rm = TRUE)
minSDHIV
maxSDHIV <- mean(HIV$RatioIDUtoAll, na.rm = TRUE) + sd(HIV$RatioIDUtoAll, na.rm = TRUE)
maxSDHIV
HIV$RatioIDUtoAll_Cat <- ifelse (HIV$RatioIDUtoAll >= minSDHIV & HIV$RatioIDUtoAll <= maxSDHIV, "within", 
        ifelse(HIV$RatioIDUtoAll < minSDHIV, "below",
              ifelse(HIV$RatioIDUtoAll > maxSDHIV, "above", NA)))
HIV$RatioIDUtoAll_Cat <- as.factor(HIV$RatioIDUtoAll_Cat)
head(HIV)
summary(HIV$RatioIDUtoAll_Cat)

######HepC
HEPc <- read.csv("Newly_Identified_Confirmed_Chronic_Hepatitis_C_Age_15-34_Year_2007-2016_Health.csv")
head(HEPc)
HEPc <- HEPc[HEPc$Year == 2016, c("County.Name", "County.Code", "Hepatitis.C.counts")] #all NAs are suppressed data
summary(HEPc)
#can try to look at distribution after I do rate for HepC


#####Median Income 2016
MedInc <- read.csv("ACS_16_5yr_medianincome.csv")
head(MedInc)
summary(MedInc)

#####OD death data from PA DEA PDF
ODdeath_DEA <- read.csv("DEA_ODreport_ODrates.csv")
str(ODdeath_DEA)
colnames(ODdeath_DEA) <- c("county", "county.code", "ODrate2016", "percentchangeindrugdeaths1516", "UrbanRuralDEA")


#####ACS Population demographics based on 2016 population estimates
PopDemos2016 <- read.csv("ACS_2016_PopDemographics.csv")
head(PopDemos2016)
str(PopDemos2016)

#####SAmhsa MH tx facilities
MHtx <- read.csv("Behavioral_Health_Treament_Facility_listing_2018_10_16_083515.csv")
MHtx <- MHtx[MHtx$state == "PA",]
MHtx$county <- as.character(MHtx$county) 
MHtx <- as.data.frame(table(MHtx$county))
summary(MHtx)
colnames(MHtx) <- c("county", "NMHfacs")
MHtx$county <- trimws(MHtx$county, "right")
MHtx <- merge(MHtx, MasterCC[c(1,3)], by.x = "county" , by.y = "county", all.y = TRUE)
MHtx$NMHfacs[is.na(MHtx$NMHfacs)] <- 0
str(MHtx)
summary(MHtx)
