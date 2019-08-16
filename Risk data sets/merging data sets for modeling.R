##### datasets created by DataCleaning
#starting with CountyPop as base table
CountyData <- merge(PopDemos2016, TBBcount[c(2,3)], by.x = "county.code", by.y = "FIPScode", all.x = TRUE)
str(CountyData)
  CountyData$PropMale <- CountyData$Nmale / CountyData$TotalPop2016
  CountyData$PropFemale <- (CountyData$TotalPop2016 - CountyData$Nmale) / CountyData$TotalPop2016
  CountyData$Prop14under <- (CountyData$TotalPop2016 - (CountyData$N15to34 + CountyData$N35to59 + CountyData$N60plus))/CountyData$TotalPop2016
  CountyData$Prop15to34 <- CountyData$N15to34 / CountyData$TotalPop2016
  CountyData$Prop35to59 <- CountyData$N35to59 / CountyData$TotalPop2016
  CountyData$Prop60plus <- CountyData$N60plus / CountyData$TotalPop2016
  CountyData$PropWhite <- CountyData$White / CountyData$TotalPop2016
  CountyData$PropBlack <- CountyData$Black / CountyData$TotalPop2016
  CountyData$PropOther <- (CountyData$TotalPop2016 - (CountyData$White + CountyData$Black)) / CountyData$TotalPop2016
  CountyData$RateTBB1K <- CountyData$NTakeBackBoxes / CountyData$TotalPop2016 * 1000
head(CountyData)

CountyData <- merge(CountyData, EDODfinal[c(2:6)], by.x = "county.code", by.y = "County.Code.Number", all.x = TRUE)

CountyData <- merge(CountyData, SnAfinal[c(2:7)], by.x = "county.code", by.y = "FIPScode" , all.x = TRUE)
  CountyData$RateSeizureArrests1000 <- CountyData$NSeizurenArrestIncidents / CountyData$TotalPop2016 * 1000
  CountyData$RateSnAArrests1000 <- CountyData$NArrests / CountyData$TotalPop2016 * 1000

CountyData <- merge(CountyData, DrugDispFinal[c(2,3)], by.x = "county.code", by.y = "County.Code.Text" , all.x = TRUE)

CountyData <- merge(CountyData, MAwOUD[c(3,4)], by.x = "county.code", by.y = "County.Code.Number" , all.x = TRUE)
  CountyData$RateMAOUD1000 <- CountyData$N_MAw_OUD / CountyData$TotalPop2016 * 1000


CountyData <- merge(CountyData, MAwMAT[c(3,4)], by.x = "county.code", by.y = "County.Code.Number" , all.x = TRUE)
  CountyData$RateMAMAT1000 <- CountyData$N_MA_MAT / CountyData$TotalPop2016 * 1000

CountyData <- merge(CountyData, MAwNarcan[c("Count", "Count.Description", "County.Code")], by.x = "county.code", by.y = "County.Code" , all.x = TRUE)    
  CountyData$RateMANarcan1000 <- CountyData$Count / CountyData$TotalPop2016 * 1000

CountyData <- merge(CountyData, MAenrollFinal[c(2:6)], by.x = "county.code", by.y = "County.Code" , all.x = TRUE)    
  CountyData$AvgMonthlyEnrollRate17 <- CountyData$AvgMonthlyEnroll2017 / CountyData$TotalPop2016 * 1000
  CountyData$AvgMonthlyEnrollRate17_Kids <- CountyData$AvgKidsMonthlyEnroll2017 / CountyData$TotalPop2016 * 1000  

CountyData <- merge(CountyData, Dropout[c(2:4)], by.x = "county.code", by.y = "county.code" , all.x = TRUE)    

CountyData <- merge(CountyData, WarmLine1[c(2,3)], by.x = "county.code", by.y = "countycode" , all.x = TRUE)    
  CountyData$RateIntakeCalls1000 <- CountyData$Nintakecalls2017 / CountyData$TotalPop2016 * 1000  

CountyData <- merge(CountyData, HomelessPIT[c(2,3)], by.x = "county.code", by.y = "County.code" , all.x = TRUE)    
  CountyData$RateHomelessPIT <- CountyData$Total.N.Persons.PIT.count / CountyData$TotalPop2016 * 1000
 
CountyData <-  merge(CountyData, BupDocsFinal[c(2,3)], by.x = "county.code", by.y = "countycode" , all.x = TRUE)    
  CountyData$RateBupDocs <- CountyData$NBupDocs / CountyData$TotalPop2016 * 1000

CountyData <-  merge(CountyData, ChildAllegations[c(2,3)], by.x = "county.code", by.y = "county.code" , all.x = TRUE)    
  
CountyData <-  merge(CountyData, PoliceNalox[c(2,3,4)], by.x = "county.code", by.y = "County.code" , all.x = TRUE)    
  CountyData$RatePolSuccReversals <- CountyData$Number.of.successful.reversals / CountyData$TotalPop2016 * 1000
  ##update police coverage variable, can prob collapse
  
CountyData <-  merge(CountyData, HIV[c(2:6)], by.x = "county.code", by.y = "County.Code.Number" , all.x = TRUE)    

  
CountyData <-  merge(CountyData, HEPc[c(2,3)], by.x = "county.code", by.y = "County.Code" , all.x = TRUE)    
  CountyData$RateHepC <- CountyData$Hepatitis.C.counts / CountyData$TotalPop2016 * 1000

CountyData <-  merge(CountyData, MedInc[c(2,3,4)], by.x = "county.code", by.y = "countyID" , all.x = TRUE)    

CountyData <-  merge(CountyData, ODdeath_DEA[c(2:5)], by.x = "county.code", by.y = "county.code" , all.x = TRUE)    

CountyData <-  merge(CountyData, CountySA[c(2:6)], by.x = "county.code", by.y = "countycode" , all.x = TRUE)    
  CountyData$RateSAFacs <- CountyData$N.SA.Facilities / CountyData$TotalPop2016 * 1000
  CountyData$RateAllowOfferMAT <- CountyData$NAlloworOfferMAT / CountyData$TotalPop2016 * 1000
  CountyData$RateOfferMAT <- CountyData$NOfferMAT / CountyData$TotalPop2016 * 1000
  CountyData$RateAccred <- CountyData$NAccred / CountyData$TotalPop2016 * 1000

CountyData <- merge(CountyData, MHtx[c(2,3)], by.x = "county.code", by.y = "countycode", all.x = TRUE)
  CountyData$RateMHFacs <- CountyData$NMHfacs / CountyData$TotalPop2016 * 1000      

CountyData <- merge(CountyData, CountyCrashData[c(2,3)], by.x = "county.code", by.y = "code", all.x = TRUE)
  CountyData$RateImpairedDr <- CountyData$NImpairedDriver2017 / CountyData$TotalPop2016 * 1000      

str(CountyData)
summary(CountyData)

summary(CountyData)

write.csv(CountyData, "CountyData1.csv") #print out to keep and share w team
CountyData <- read.csv("CountyData1.csv") #read back in to quickly fix factors that had levels w 0 values
str(CountyData$Police.Coverage)


