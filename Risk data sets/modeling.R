
RegDemos <- lm(ODrate2016 ~ 
                   PropMale # (higher prop male, lower odds of OD)
                 #+ RateChildAllegCat 
                 + Prop15to34 # (352)
                 + Prop35to59 # (619)
                 + Prop60plus # (254, almos sig)
                  + PropWhite 
                  + median.income.estimate #(higher income, lower OD)
                 # + MedianIncomeCat #(Higher comp to 1sd; lower -11)
                + UrbanRuralDEA
               , data = CountyData)
#r2 - .02023, adjusted .1026


summary(RegDemos)
###############
RegSDOH <- lm(ODrate2016 ~
              DropoutRatesSY16
              + RateChildAlleg #higher rates of child all, lower risk of OD
              + RateHomelessPIT #higher homeless, higher OD
              + RatioIDUtoAll_Cat #Higher proportion of HIV compared to SD, lower risk of OD
              + RateHepC #higher rate of Hep C, higher risk of OD
                , data = CountyData)
summary(RegSDOH)
cor(CountyData$RateChildAlleg, CountyData$RateHomelessPIT, use = "pairwise.complete.obs")
#r2 - .3256, adjusted .2507

#########################
RegMA <- lm(ODrate2016 ~
              RateMAMAT1000
           # RateMAOUD1000
            + RateMANarcan1000 #increase this, increase OD 38.865
            #+AvgMonthlyEnrollRate17 
            +AvgMonthlyEnrollRate17_Kids 
              , data = CountyData)

summary(RegMA)
cor(CountyData$RateMAMAT1000, CountyData$AvgMonthlyEnrollRate17_Kids, use = "pairwise.complete.obs")
#r2 - .4887, adjusted .4567
########################
RegFirstResponders <- lm(ODrate2016 ~
                        ED_AnyDrugODrate1000
                        #+ KGseizedFent
                        #+ KGseizedNotFent
                        + RateIntakeCalls1000 #increase calls increase OD
                        + RatePolSuccReversals
                        + Police.Coverage #No municipal increased risk
                        + RateSeizureArrests1000
                        + KGseizedAllOpioid 
                        + RateImpairedDr
                           , data = CountyData)

summary(RegFirstResponders)
cor(CountyData$RateSeizureArrests1000, CountyData$KGseizedAllOpioid, use = "pairwise.complete.obs")
#r2 - .3333, adjusted .2156
#####################
RegInOut <- lm(ODrate2016~
                 AnnualDispCat #(higher - increase; lower, decrease)
               + RateTBB1K  #(more TBB, lower risk)
               , data = CountyData)
summary(RegInOut)
#r2 - .1798, adjusted .1388
#########################
RegTx <- lm(ODrate2016 ~
              RateBupDocs #increase OD
           # + RateSAFacs #OR
            + RateAllowOfferMAT + RateSANoMAT
            + RateAccred #increase OD
            + RateMHFacs
              , data = CountyData)
?rank

CountyData$ODrateRank <- rank(-CountyData$ODrate2016, ties.method = "first")

CountyData$ODrateRank[CountyData$county == "Philadelphia"]
CountyData$county[CountyData$ODrateRank == 65]
summary(RegTx)


#r2 - .2259, adjusted .1592
#####################################################################
#additional variables created for dashboard based on modeling above #
#####################################################################

#agecats
mean15to34 <-mean(CountyData$Prop15to34)
sd15to34 <- sd(CountyData$Prop15to34)
CountyData$Prop15to34CAT <- ifelse(CountyData$Prop15to34 < mean15to34 - sd15to34, "below",
                                  ifelse(CountyData$Prop15to34 > mean15to34 + sd15to34, "above",
                                         ifelse(CountyData$Prop15to34 <= mean15to34 + sd15to34 & CountyData$Prop15to34 >= mean15to34 - sd15to34, "within", "Other")))

CountyData$Prop15to34CAT <- as.factor(CountyData$Prop15to34CAT)
summary(CountyData$Prop15to34CAT)


mean35to59 <-mean(CountyData$Prop35to59)
sd35to59 <- sd(CountyData$Prop35to59)
CountyData$Prop35to59CAT <- ifelse(CountyData$Prop35to59 < mean35to59 - sd35to59, "below",
                                   ifelse(CountyData$Prop35to59 > mean35to59 + sd35to59, "above",
                                          ifelse(CountyData$Prop35to59 <= mean35to59 + sd35to59 & CountyData$Prop35to59 >= mean35to59 - sd35to59, "within", "Other")))


CountyData$Prop35to59CAT <- as.factor(CountyData$Prop35to59CAT)
summary(CountyData$Prop35to59CAT)

mean60plus <-mean(CountyData$Prop60plus)
sd60plus <- sd(CountyData$Prop60plus)
CountyData$Prop60plusCAT <- ifelse(CountyData$Prop60plus < mean60plus - sd60plus, "below",
                                   ifelse(CountyData$Prop60plus > mean60plus + sd60plus, "above",
                                          ifelse(CountyData$Prop60plus <= mean60plus + sd60plus & CountyData$Prop60plus >= mean60plus - sd60plus, "within", "Other")))


CountyData$Prop60plusCAT <- as.factor(CountyData$Prop60plusCAT)
summary(CountyData$Prop60plusCAT)

meanMedA <-mean(CountyData$MedianAge)
sdMedA <- sd(CountyData$MedianAge)
CountyData$MedianAgeCat <- ifelse(CountyData$MedianAge < meanMedA - sdMedA, "below",
                                   ifelse(CountyData$MedianAge > meanMedA + sdMedA, "above",
                                          ifelse(CountyData$MedianAge <= meanMedA + sdMedA & CountyData$MedianAge >= meanMedA - sdMedA, "within", "Other")))


CountyData$MedianAgeCat <- as.factor(CountyData$MedianAgeCat)
summary(CountyData$MedianAgeCat)

#childalleg
meanCA <-mean(CountyData$RateChildAlleg)
sdCA <- sd(CountyData$RateChildAlleg)
CountyData$RateChildAllegCat <- ifelse(CountyData$RateChildAlleg < meanCA - sdCA, "below",
                                  ifelse(CountyData$RateChildAlleg > meanCA + sdCA, "above",
                                         ifelse(CountyData$RateChildAlleg <= meanCA + sdCA & CountyData$RateChildAlleg >= meanCA - sdCA, "within", "Other")))


CountyData$RateChildAllegCat <- as.factor(CountyData$RateChildAllegCat)
summary(CountyData$RateChildAllegCat)




#homeless
meanHomeless <-mean(CountyData$RateHomelessPIT)
sdHomeless <- sd(CountyData$RateHomelessPIT)
CountyData$RateHomelessPITCat <- ifelse(CountyData$RateHomelessPIT < meanHomeless - sdHomeless, "below",
                                       ifelse(CountyData$RateHomelessPIT > meanHomeless + sdHomeless, "above",
                                              ifelse(CountyData$RateHomelessPIT <= meanHomeless + sdHomeless & CountyData$RateHomelessPIT >= meanHomeless - sdHomeless, "within", "Other")))


CountyData$RateHomelessPITCat <- as.factor(CountyData$RateHomelessPITCat)
summary(CountyData$RateHomelessPITCat)

#HepC
meanHepC <- mean(CountyData$RateHepC, na.rm = TRUE)
sdHepC <- sd(CountyData$RateHepC, na.rm = TRUE)
CountyData$RateHepCCat <- ifelse(CountyData$RateHepC < meanHepC - sdHepC, "below",
                                        ifelse(CountyData$RateHepC > meanHepC + sdHepC, "above",
                                               ifelse(CountyData$RateHepC <= meanHepC + sdHepC & CountyData$RateHepC >= meanHepC - sdHepC, "within", "Other")))


CountyData$RateHepCCat <- as.factor(CountyData$RateHepCCat)
summary(CountyData$RateHepCCat)

#intakecalls
meanIntakeCalls1000 <- mean(CountyData$RateIntakeCalls1000, na.rm = TRUE)
sdIntakeCalls1000 <- sd(CountyData$RateIntakeCalls1000, na.rm = TRUE)
CountyData$RateIntakeCalls1000Cat <- ifelse(CountyData$RateIntakeCalls1000 < meanIntakeCalls1000 - sdIntakeCalls1000, "below",
                                 ifelse(CountyData$RateIntakeCalls1000 > meanIntakeCalls1000 + sdIntakeCalls1000, "above",
                                        ifelse(CountyData$RateIntakeCalls1000 <= meanIntakeCalls1000 + sdIntakeCalls1000 & CountyData$RateIntakeCalls1000 >= meanIntakeCalls1000 - sdIntakeCalls1000, "within", "Other")))


CountyData$RateIntakeCalls1000Cat <- as.factor(CountyData$RateIntakeCalls1000Cat)
summary(CountyData$RateIntakeCalls1000Cat)

#TBB
meanTBB1K <- mean(CountyData$RateTBB1K, na.rm = TRUE)
sdTBB1K <- sd(CountyData$RateTBB1K, na.rm = TRUE)
CountyData$RateTBB1KCat <- ifelse(CountyData$RateTBB1K < meanTBB1K - sdTBB1K, "below",
                                            ifelse(CountyData$RateTBB1K > meanTBB1K + sdTBB1K, "above",
                                                   ifelse(CountyData$RateTBB1K <= meanTBB1K + sdTBB1K & CountyData$RateTBB1K >= meanTBB1K - sdTBB1K, "within", "Other")))


CountyData$RateTBB1KCat <- as.factor(CountyData$RateTBB1KCat)
summary(CountyData$RateTBB1KCat)

#bupdocs
meanBupDocs <- mean(CountyData$RateBupDocs, na.rm = TRUE)
sdBupDocs <- sd(CountyData$RateBupDocs, na.rm = TRUE)
CountyData$RateBupDocsCat <- ifelse(CountyData$RateBupDocs < meanBupDocs - sdBupDocs, "below",
                                  ifelse(CountyData$RateBupDocs > meanBupDocs + sdBupDocs, "above",
                                         ifelse(CountyData$RateBupDocs <= meanBupDocs + sdBupDocs & CountyData$RateBupDocs >= meanBupDocs - sdBupDocs, "within", "Other")))


CountyData$RateBupDocsCat <- as.factor(CountyData$RateBupDocsCat)
summary(CountyData$RateBupDocsCat)

#accred

meanAccred <- mean(CountyData$RateAccred, na.rm = TRUE)
sdAccred <- sd(CountyData$RateAccred, na.rm = TRUE)
CountyData$RateAccredCat <- ifelse(CountyData$RateAccred < meanAccred - sdAccred, "below",
                                    ifelse(CountyData$RateAccred > meanAccred + sdAccred, "above",
                                           ifelse(CountyData$RateAccred <= meanAccred + sdAccred & CountyData$RateAccred >= meanAccred - sdAccred, "within", "Other")))


CountyData$RateAccredCat <- as.factor(CountyData$RateAccredCat)
summary(CountyData$RateAccredCat)

#MatNarcan

meanMANarcan1000 <- mean(CountyData$RateMANarcan1000, na.rm = TRUE)
sdMANarcan1000 <- sd(CountyData$RateMANarcan1000, na.rm = TRUE)
CountyData$RateMANarcan1000Cat <- ifelse(CountyData$RateMANarcan1000 < meanMANarcan1000 - sdMANarcan1000, "below",
                                   ifelse(CountyData$RateMANarcan1000 > meanMANarcan1000 + sdMANarcan1000, "above",
                                          ifelse(CountyData$RateMANarcan1000 <= meanMANarcan1000 + sdMANarcan1000 & CountyData$RateMANarcan1000 >= meanMANarcan1000 - sdMANarcan1000, "within", "Other")))


CountyData$RateMANarcan1000Cat <- as.factor(CountyData$RateMANarcan1000Cat)
summary(CountyData$RateMANarcan1000Cat)

#MAenrollment

meanEnroll <- mean(CountyData$AvgMonthlyEnrollRate17, na.rm = TRUE)
sdEnroll <- sd(CountyData$AvgMonthlyEnrollRate17, na.rm = TRUE)
CountyData$AvgMonthlyEnrollRate17Cat <- ifelse(CountyData$AvgMonthlyEnrollRate17 < meanEnroll - sdEnroll, "below",
                                   ifelse(CountyData$AvgMonthlyEnrollRate17 > meanEnroll + sdEnroll, "above",
                                          ifelse(CountyData$AvgMonthlyEnrollRate17 <= meanEnroll + sdEnroll & CountyData$AvgMonthlyEnrollRate17 >= meanEnroll - sdEnroll, "within", "Other")))


CountyData$AvgMonthlyEnrollRate17Cat <- as.factor(CountyData$AvgMonthlyEnrollRate17Cat)
summary(CountyData$AvgMonthlyEnrollRate17Cat)


#MAenrollmentkids

meanEnrollKid <- mean(CountyData$AvgMonthlyEnrollRate17_Kids, na.rm = TRUE)
sdEnrollKid <- sd(CountyData$AvgMonthlyEnrollRate17_Kids, na.rm = TRUE)
CountyData$AvgMonthlyEnrollRate17KidCat <- ifelse(CountyData$AvgMonthlyEnrollRate17_Kids < meanEnrollKid - sdEnrollKid, "below",
                                               ifelse(CountyData$AvgMonthlyEnrollRate17_Kids > meanEnrollKid + sdEnrollKid, "above",
                                                      ifelse(CountyData$AvgMonthlyEnrollRate17_Kids <= meanEnrollKid + sdEnrollKid & CountyData$AvgMonthlyEnrollRate17_Kids >= meanEnrollKid - sdEnrollKid, "within", "Other")))


CountyData$AvgMonthlyEnrollRate17KidCat <- as.factor(CountyData$AvgMonthlyEnrollRate17KidCat)
summary(CountyData$AvgMonthlyEnrollRate17KidCat)

levels(CountyData$RatioIDUtoAll_Cat) <- c("within", "above", "below")
levels(CountyData$MedianIncomeCat) <- c("within", "above", "below")
levels(CountyData$AnnualDispCat) <- c("within", "above", "below")
#########################################################
write.csv(CountyData, "CountyData.csv") #final dashboard dataset
#########################################################