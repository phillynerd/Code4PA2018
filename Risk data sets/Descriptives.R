library(ggplot2)
str(CountyData)
summary(CountyData$TotalPop2016)
example(boxplot)
boxplot(CountyData$TotalPop2016)

boxplot(CountyData$MedianAge)
  summary(CountyData$MedianAge)
  summary(PopDemos2016$MedianAge)
  #error in data, max value has misplaced decimal. should be 39.1989
  CountyData$MedianAge[CountyData$MedianAge == max(CountyData$MedianAge)] <- 39.1989

boxplot(CountyData$PropMale)
boxplot (CountyData$PropFemale)

boxplot(CountyData$Prop14under)
boxplot(CountyData$Prop15to34)
boxplot(CountyData$Prop35to59)
boxplot(CountyData$Prop60plus)

boxplot(CountyData$PropWhite)
boxplot(CountyData$PropBlack)
boxplot(CountyData$PropOther)

boxplot(CountyData$RateTBB1K)
  summary(CountyData$RateTBB1K)

boxplot(CountyData$ED_AnyDrugODrate1000)
  summary(CountyData$ED_AnyDrugODrate1000) #1 counties with NA, note that counties with rates and suppressed info, i kept the rates. So some with rates of 0 really have rates slightly higher than 0
boxplot(CountyData$ED_AnyOpioidODrate)
  summary(CountyData$ED_AnyOpioidODrate) #7 counties with NA

####Sullivan is only county with no hx of reporting
hist(CountyData$KGseizedAllOpioid)
  summary(CountyData$KGseizedAllOpioid) #7 counties NA, data imputed to change to 1
hist(CountyData$KGseizedFent)
  summary(CountyData$KGseizedFent) #7 counties NA, data imputed to change to 1
hist(CountyData$KGseizedNotFent) 
  summary(CountyData$KGseizedNotFent) #7 counties NA, data imputed to change to 1
boxplot(CountyData$RateSeizureArrests1000)
  summary(CountyData$RateSeizureArrests1000) #7 counties NA, data imputed to change to 1
boxplot(CountyData$RateSnAArrests1000)
  summary(CountyData$RateSnAArrests1000) #7 counties NA, data imputed to change to 1

  #For counties that are NA - looks like those counties report in other years, and often report very tiny amounts. 
  CountyData$county.code[is.na(CountyData$KGseizedAllOpioid)]
  #Examining the data, looks like sullivan (57) is the only county that never reports
  #all others report at some point - change non-reports to 0 for all other counties
  CountyData$KGseizedAllOpioid[is.na(CountyData$KGseizedAllOpioid) & CountyData$county.code != 57] <- 0
  CountyData$KGseizedFent[is.na(CountyData$KGseizedFent) & CountyData$county.code != 57] <- 0
  CountyData$KGseizedNotFent[is.na(CountyData$KGseizedNotFent) & CountyData$county.code != 57] <- 0
  CountyData$RateSeizureArrests1000[is.na(CountyData$RateSeizureArrests1000) & CountyData$county.code != 57] <- 0
  CountyData$RateSnAArrests1000[is.na(CountyData$RateSnAArrests1000) & CountyData$county.code != 57] <- 0
  
boxplot(CountyData$AnnualDispPer1000)
  summary(CountyData$AnnualDispPer1000)

boxplot(CountyData$RateMAOUD1000)
  summary(CountyData$RateMAOUD1000)
boxplot(CountyData$RateMAMAT1000)
  summary(CountyData$RateMAMAT1000)  #two NA, check to see if I can impute 0's - NA's are suppressed <lt 10>
boxplot(CountyData$RateMANarcan1000)
  summary(CountyData$RateMANarcan1000) #15 NA, check to see if i can impute 0's - NA's are suppresed <lt 10>
  ##figuring out what to do with NA's
  CountyData$county.code[is.na(CountyData$RateMANarcan1000)] #12 16 27 29 31 44 47 50 52 53 55 57 58 60 62
  summary(CountyData$N_MA_MAT)
  summary(CountyData$N_MAw_OUD)
  summary(CountyData$Count)
  
  testcd <- CountyData[is.na(CountyData$RateMANarcan1000) | is.na (CountyData$RateMAMAT1000),]
  testcd$Count[is.na(testcd$Count)] <- 9

  testcd$RateMANarcan1000 <- testcd$Count / testcd$TotalPop2016 * 1000
  summary(testcd$RateMANarcan1000)
    testcd$RateMANarcan1000
    ##when imputing 1, median is .02485, min .01779, max .20803 mean = .05899
    ##when imputing 9, median is .2236, min .1601 , max 1.8723 , mean = .5309
    ##decision - i'd leave blank for modeling, but to assess risk i'd impute avg of medians into rate
      (.02485 + .2236)/2 #.124225
  
  
  
boxplot(CountyData$AvgMonthlyEnrollRate17)
  summary(CountyData$AvgMonthlyEnrollRate17)  
boxplot(CountyData$AvgMonthlyEnrollRate17_Kids)
  summary(CountyData$AvgMonthlyEnrollRate17_Kids) 

boxplot(CountyData$DropoutRatesSY16)
  summary(CountyData$DropoutRatesSY16) 

boxplot(CountyData$RateIntakeCalls1000)
  summary(CountyData$RateIntakeCalls1000) #2 counties NA 

boxplot(CountyData$RateHomelessPIT)
  summary(CountyData$RateHomelessPIT) 

boxplot(CountyData$RateBupDocs)
  summary(CountyData$RateBupDocs) 

boxplot(CountyData$RateChildAlleg)
  summary(CountyData$RateChildAlleg) 

summary(CountyData$Police.Coverage)
str(CountyData$Police.Coverage)
boxplot(CountyData$RatePolSuccReversals)
  summary(CountyData$RatePolSuccReversals) 

boxplot(CountyData$RatioIDUtoAll) 
  summary(CountyData$RatioIDUtoAll) #15 counties with NA, down to 2 after imputation (methodology below)
 ##Figuring out what to do with NAs
  summary(CountyData$Prevalence.HIV.Disease.Count) #2 NA
  summary(CountyData$Prevalence.HIV.Disease.Among.IDU.Count) #15 NA
  CountyData$county[is.na(CountyData$Prevalence.HIV.Disease.Count)]
  CountyData$county.code[is.na(CountyData$Prevalence.HIV.Disease.Count)]  #12,53
  CountyData$county[is.na(CountyData$Prevalence.HIV.Disease.Among.IDU.Count)]
  CountyData$county.code[is.na(CountyData$Prevalence.HIV.Disease.Among.IDU.Count)] #5, 8, 12, 18, 24, 27, 29, 32, 34, 37, 47, 53, 57, 59, 61
  summary(CountyData$Prevalence.HIV.Disease.Count)
  summary(CountyData$Prevalence.HIV.Disease.Among.IDU.Count) #looks like suppression occurs in counts of 1-5
  
  ##figuring out best imputation for these cases
  testcd <- CountyData[is.na(CountyData$RatioIDUtoAll),]
  testcd$Prevalence.HIV.Disease.Among.IDU.Count[is.na(testcd$Prevalence.HIV.Disease.Among.IDU.Count)] <- 4
  
  testcd$RatioIDUtoAll <- testcd$Prevalence.HIV.Disease.Among.IDU.Count / testcd$Prevalence.HIV.Disease.Count
  summary(testcd$RatioIDUtoAll)
  testcd$RatioIDUtoAll
  #imputing 1 - min 0.01818 med 0.05556 mean 0.06433 max 0.14286, 2 NAs
  #imputing 4 - min .07273 med .22222 mean .25733 max 0.57143, 2 NAs
  ###### decision - I'd impute the median pre-modeling since it's such a tight range for suppression (1-4).  Average median
  (.05556+.2222)/2 #.13888
############### imputing average median to 13/15 counties - not calculating on counties with less than 5 overall HIV cases 
  CountyData$RatioIDUtoAll[is.na(CountyData$RatioIDUtoAll) & !is.na(CountyData$Prevalence.HIV.Disease.Count)] <- .13888

  
boxplot(CountyData$RateHepC)
  summary(CountyData$RateHepC) 
boxplot(CountyData$median.income.estimate)
  summary(CountyData$median.income.estimate)
  ####throwing this into categories to avoid normalization of all variables
  median(CountyData$median.income.estimate)
  sd(CountyData$median.income.estimate)
  CountyData$MedianIncomeCat <- ifelse (CountyData$median.income.estimate > (mean(CountyData$median.income.estimate) + sd(CountyData$median.income.estimate)),"Higher",
                                           ifelse(CountyData$median.income.estimate < (mean(CountyData$median.income.estimate) - sd(CountyData$median.income.estimate)), "Lower",
                                                  ifelse(CountyData$median.income.estimate >= (mean(CountyData$median.income.estimate) - sd(CountyData$median.income.estimate)) & 
                                                           CountyData$median.income.estimate <= (mean(CountyData$median.income.estimate) + sd(CountyData$median.income.estimate)), "1SD","error")))
  CountyData$MedianIncomeCat <- as.factor(CountyData$MedianIncomeCat)
  boxplot(CountyData$median.income.estimate ~ CountyData$MedianIncomeCat)
  boxplot.stats(CountyData$median.income.estimate[CountyData$MedianIncomeCat=="1SD"]) #39770 44926 47023 51349 58585
  boxplot.stats(CountyData$median.income.estimate[CountyData$MedianIncomeCat=="Higher"]) #58980.0 60526.0 62696.5 73067.5 88995.0
  boxplot.stats(CountyData$median.income.estimate[CountyData$MedianIncomeCat=="Lower"]) #forest county is the poorest county in state
  
  
boxplot(CountyData$ODrate2016)
  summary(CountyData$ODrate2016) #3 NAs - these counties just didn't report ODs it looks like
boxplot(CountyData$percentchangeindrugdeaths1516)
  summary(CountyData$percentchangeindrugdeaths1516)#Percent Change stats: min -0.3800  med 0.3700  mean 0.5739  max 3.0000 
  ##checking how many are negative
  CountyData$county[CountyData$percentchangeindrugdeaths1516 < 0 & !is.na(CountyData$percentchangeindrugdeaths1516)] #10 counties under 0
  summary(CountyData$percentchangeindrugdeaths1516[CountyData$percentchangeindrugdeaths1516 < 0 & !is.na(CountyData$percentchangeindrugdeaths1516)])
  CountyData$county[CountyData$percentchangeindrugdeaths1516 >= 0 & !is.na(CountyData$percentchangeindrugdeaths1516)] #54 counties (43 between 0 and <1)
  summary(CountyData$percentchangeindrugdeaths1516[CountyData$percentchangeindrugdeaths1516 >= 0 & !is.na(CountyData$percentchangeindrugdeaths1516)])
  boxplot(CountyData$percentchangeindrugdeaths1516[CountyData$percentchangeindrugdeaths1516 >= 0 & !is.na(CountyData$percentchangeindrugdeaths1516)])
  CountyData$county[CountyData$percentchangeindrugdeaths1516 >= 1 & !is.na(CountyData$percentchangeindrugdeaths1516)] #11 counties over 1
  ###creating cat for change
  CountyData$ChangeinODdeath_CAT <- ifelse(CountyData$percentchangeindrugdeaths1516 < 0 & !is.na(CountyData$percentchangeindrugdeaths1516), "fewer ODs",
    ifelse(CountyData$percentchangeindrugdeaths1516 >= 1 & !is.na(CountyData$percentchangeindrugdeaths1516), "ODs doubled or more",
      ifelse(CountyData$percentchangeindrugdeaths1516 < 1 & CountyData$percentchangeindrugdeaths1516 >= 0 & !is.na(CountyData$percentchangeindrugdeaths1516),"More ODs", NA )))
  CountyData$ChangeinODdeath_CAT <- as.factor(CountyData$ChangeinODdeath_CAT)
  summary(CountyData$ChangeinODdeath_CAT)
  
boxplot(CountyData$RateSAFacs)
  summary(CountyData$RateSAFacs)
boxplot(CountyData$RateOfferMAT)
  summary(CountyData$RateOfferMAT)
boxplot(CountyData$RateAllowOfferMAT)
  summary(CountyData$RateAllowOfferMAT)
#creating a variable to look at those that don't allow MAT
  CountyData$CountSANoMAT <- CountyData$N.SA.Facilities - CountyData$NAlloworOfferMAT
  summary(CountyData$CountSANoMAT)
  CountyData$RateSANoMAT <- CountyData$CountSANoMAT / CountyData$TotalPop2016 * 1000
  
boxplot(CountyData$RateAccred)
  summary(CountyData$RateAccred)

boxplot(CountyData$RateMHFacs)
  summary(CountyData$RateMHFacs)
  
boxplot(CountyData$RateImpairedDr)
  summary(CountyData$RateImpairedDr)
  
################################
#  boxplots by urban/rural     #
################################
  
  
boxplot(CountyData$MedianAge ~ CountyData$UrbanRuralDEA)

  
  boxplot(CountyData$PropMale ~ CountyData$UrbanRuralDEA)
  boxplot (CountyData$PropFemale ~ CountyData$UrbanRuralDEA)
  
  boxplot(CountyData$Prop14under ~ CountyData$UrbanRuralDEA)
  boxplot(CountyData$Prop15to34 ~ CountyData$UrbanRuralDEA)
  boxplot(CountyData$Prop35to59 ~ CountyData$UrbanRuralDEA)
  boxplot(CountyData$Prop60plus ~ CountyData$UrbanRuralDEA)
  
  boxplot(CountyData$PropWhite ~ CountyData$UrbanRuralDEA)
  boxplot(CountyData$PropBlack ~ CountyData$UrbanRuralDEA)
  boxplot(CountyData$PropOther ~ CountyData$UrbanRuralDEA)
  
  boxplot(CountyData$RateTBB1K ~ CountyData$UrbanRuralDEA)
  
  boxplot(CountyData$ED_AnyDrugODrate1000 ~ CountyData$UrbanRuralDEA)
  boxplot(CountyData$ED_AnyOpioidODrate ~ CountyData$UrbanRuralDEA)
  
  boxplot(CountyData$AnnualDispPer1000 ~ CountyData$UrbanRuralDEA)
  ###creating a cat to avoid normalization
  CountyData$AnnualDispCat <- ifelse(CountyData$AnnualDispPer1000 > (mean(CountyData$AnnualDispPer1000) + sd(CountyData$AnnualDispPer1000)),"Higher",
                                     ifelse(CountyData$AnnualDispPer1000 < (mean(CountyData$AnnualDispPer1000) - sd(CountyData$AnnualDispPer1000)), "Lower",
                                            ifelse(CountyData$AnnualDispPer1000 >= (mean(CountyData$AnnualDispPer1000) - sd(CountyData$AnnualDispPer1000)) & CountyData$AnnualDispPer1000 <=(mean(CountyData$AnnualDispPer1000) + sd(CountyData$AnnualDispPer1000)), "1SD", "error")))
  CountyData$AnnualDispCat <- as.factor(CountyData$AnnualDispCat)
  summary(CountyData$AnnualDispCat)
  
  boxplot(CountyData$RateMAOUD1000 ~ CountyData$UrbanRuralDEA)
  boxplot(CountyData$RateMAMAT1000 ~ CountyData$UrbanRuralDEA)
  boxplot(CountyData$RateMANarcan1000 ~ CountyData$UrbanRuralDEA)
  
  
  
  
  boxplot(CountyData$AvgMonthlyEnrollRate17 ~ CountyData$UrbanRuralDEA)
  boxplot(CountyData$AvgMonthlyEnrollRate17_Kids ~ CountyData$UrbanRuralDEA)
  
  boxplot(CountyData$DropoutRatesSY16 ~ CountyData$UrbanRuralDEA)
  
  boxplot(CountyData$RateIntakeCalls1000 ~ CountyData$UrbanRuralDEA)
  
  boxplot(CountyData$RateHomelessPIT ~ CountyData$UrbanRuralDEA)
  
  boxplot(CountyData$RateBupDocs ~ CountyData$UrbanRuralDEA)
  
  boxplot(CountyData$RateChildAlleg ~ CountyData$UrbanRuralDEA)
  
  
  boxplot(CountyData$RatePolSuccReversals ~ CountyData$UrbanRuralDEA)
  boxplot.stats(CountyData$RatePolSuccReversals[CountyData$UrbanRuralDEA == "rural"],)
  boxplot.stats(CountyData$RatePolSuccReversals[CountyData$UrbanRuralDEA == "urban"],)
  
  
  boxplot(CountyData$RatioIDUtoAll ~ CountyData$UrbanRuralDEA) 
  
  
  boxplot(CountyData$RateHepC ~ CountyData$UrbanRuralDEA)
  boxplot.stats(CountyData$RateHepC[CountyData$UrbanRuralDEA == "rural"],)
  boxplot.stats(CountyData$RateHepC[CountyData$UrbanRuralDEA == "urban"],)
  
  boxplot(CountyData$median.income.estimate ~ CountyData$UrbanRuralDEA)
  boxplot.stats(CountyData$median.income.estimate[CountyData$UrbanRuralDEA == "rural"],)
  boxplot.stats(CountyData$median.income.estimate[CountyData$UrbanRuralDEA == "urban"],)
  
  boxplot(CountyData$ODrate2016 ~ CountyData$UrbanRuralDEA)
  boxplot(CountyData$percentchangeindrugdeaths1516 ~ CountyData$UrbanRuralDEA)
  
  
  boxplot(CountyData$RateSAFacs ~ CountyData$UrbanRuralDEA)
  boxplot(CountyData$RateOfferMAT ~ CountyData$UrbanRuralDEA)
  boxplot(CountyData$RateAllowOfferMAT ~ CountyData$UrbanRuralDEA)
  boxplot(CountyData$RateAccred ~ CountyData$UrbanRuralDEA)
  boxplot(CountyData$RateSANoMAT ~ CountyData$UrbanRuralDEA)
  
  
boxplot(CountyData$RateMHFacs ~ CountyData$UrbanRuralDEA)
  
boxplot(CountyData$RateImpairedDr ~ CountyData$UrbanRuralDEA)

##condensing policecoverage variable
summary(CountyData$Police.Coverage)
CountyData$PolCovShort <- ifelse(CountyData$Police.Coverage == "Full coverage","Full coverage",
                                 ifelse(CountyData$Police.Coverage == "Partial coverage", "Partial coverage", "No Coverage/No Munic")) 
#################################
#Fixing HIV cat since imputation#
#################################

minSDHIV <- mean(CountyData$RatioIDUtoAll, na.rm = TRUE) - sd(CountyData$RatioIDUtoAll, na.rm = TRUE)
minSDHIV
maxSDHIV <- mean(CountyData$RatioIDUtoAll, na.rm = TRUE) + sd(CountyData$RatioIDUtoAll, na.rm = TRUE)
maxSDHIV
CountyData$RatioIDUtoAll_Cat <- ifelse (CountyData$RatioIDUtoAll >= minSDHIV & CountyData$RatioIDUtoAll <= maxSDHIV, "1SD", 
                                 ifelse(CountyData$RatioIDUtoAll < minSDHIV, "sig lower",
                                        ifelse(CountyData$RatioIDUtoAll > maxSDHIV, "sig higher", NA)))
CountyData$RatioIDUtoAll_Cat <- as.factor(CountyData$RatioIDUtoAll_Cat)
summary(CountyData$RatioIDUtoAll_Cat)

########################################
#picking final variables for modeling  #
########################################
summary(CountyData)
CountyData$PropMale
CountyData$Prop15to34
CountyData$Prop35to59
CountyData$Prop60plus
CountyData$PropBlack
CountyData$PropOther
CountyData$RateTBB1K
CountyData$ED_AnyDrugODrate1000
CountyData$ED_AnyOpioidODrate #7 NAs
CountyData$RateSeizureArrests1000 #1 NA
  ###or
  CountyData$RateSnAArrests1000 #1 NA
CountyData$KGseizedAllOpioid #1 NA
  ###or
  CountyData$KGseizedFent #1 NA
  CountyData$KGseizedNotFent #1 NA
CountyData$AnnualDispPer1000
  ###or
  CountyData$AnnualDispCat
CountyData$RateMAMAT1000 #2 NA
CountyData$RateMAOUD1000 
CountyData$RateMANarcan1000 #15 NA
CountyData$AvgMonthlyEnrollRate17
CountyData$AvgMonthlyEnrollRate17_Kids
CountyData$DropoutRatesSY16
CountyData$RateIntakeCalls1000 #2 NA
CountyData$RateHomelessPIT
CountyData$RateBupDocs
CountyData$RateChildAlleg
CountyData$Police.Coverage 
  ####maybe as an or? or maybe include both? unsure
  CountyData$RatePolSuccReversals
CountyData$RatioIDUtoAll
  ####or
  CountyData$RatioIDUtoAll_Cat
CountyData$RateHepC
CountyData$MedianIncomeCat
CountyData$ODrate2016 #this is my outcome variable, na = 3
CountyData$UrbanRuralDEA
CountyData$RateSAFacs
  ###OR
  CountyData$RateAllowOfferMAT
  CountyData$RateSANoMAT
CountyData$RateAccred
CountyData$RateMHFacs
CountyData$RateImpairedDr
CountyData$ChangeinODdeath_CAT








  

  