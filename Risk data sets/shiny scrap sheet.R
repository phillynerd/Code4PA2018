

str(CountyData$AvgMonthlyEnrollRate17)
str(CountyData$AvgMonthlyEnrollRate17Cat)

############################
###############top 10 list

output$Top10 <- renderTable({
  Top10 <- subset(CountyData[order(CountyData$ODrateRank),], subset = ODrateRank <= 10, select = c("ODrateRank","county","ODrate2016"))
  rename(Top10, "Rank" = ODrateRank, "County" = county, "OD Rate" = ODrate2016)
}, caption =  "<span style='color:#003366'> Overdose Deaths per 100,000 - Top 10 Counties", 
caption.placement = getOption("xtable.caption.placement", "top"), 
caption.width = getOption("xtable.caption.width", NULL))

install.packages("rsconnect")

rsconnect::setAccountInfo(name='phillynerd',
                          token='1D2916E840DF705DC22FDB60DA158CC8',
                          secret='gjhyvpkacD+12yLxzXNY76mbsNch/ahWe7KJqeHf')