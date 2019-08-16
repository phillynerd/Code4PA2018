
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
#library(xtable)

shinyServer(function(input,output)({
  
  #police coverage bargraph
  output$PolCovpie <- renderPlot({  
    ggplot(DFpolcov, aes(x=NaloxoneCoverage, y = NCounties, fill = NaloxoneCoverage)) +
      geom_bar(stat = "identity") +
      theme(legend.position = "none", #removes legend
            axis.text.x = element_text(angle = 45, hjust = 1),
            axis.title.x = element_blank(),
            plot.title = element_text(hjust = .5)) + #angles x axis labels
      ggtitle("Breakdown of Counties by Narcan Coverage /n in Municipal PDs") +
        ylab("N Counties")
    })
########################
# Top Section of App   #
########################
  #Title Panel Info 
  output$title_panel <- renderText({
    title_panel <- paste0(input$County," County")
  })

  #Total Pop Info Box
  output$TotalPop <- renderInfoBox({
    infoBox("Total Population", CountyData$TotalPop2016[CountyData$county == input$County], icon = icon("users"), color = "purple", fill = TRUE)
    
  })
  
  #RateOD info
  output$RateOD <- renderInfoBox({
    infoBox("Overdose Deaths", 
            paste0(CountyData$ODrate2016[CountyData$county == input$County], " per 100,000"), icon = icon("minus-circle"), color = "yellow", fill = TRUE)
  
  })
  
  #RankOD info
  output$Rank <- renderInfoBox({
    infoBox("Rank among PA Counties", 
            paste0(CountyData$ODrateRank[CountyData$county == input$County], " of 64 (3 not reporting)"), 
            icon = icon("sort-numeric-asc"), color = "yellow", fill = TRUE)
    
  })
  
  #Percent Change info
  output$Percent <- renderInfoBox({
    infoBox("% Change in OD Deaths", 
            paste0(CountyData$percentchangeindrugdeaths1516 [CountyData$county == input$County], "% change since prior year"), 
            icon = icon("percent"), color = "yellow", fill = TRUE)
    
  })

  #Nearby Counties info
  output$CountyAbove <- renderInfoBox({
  infoBox("County ranked one above", 
          CountyData$county[na.exclude(CountyData$ODrateRank == (CountyData$ODrateRank[CountyData$county == input$County]-1))], 
          icon = icon("sort-up"), color = "fuchsia", fill = TRUE)
  })

  output$CountyBelow <- renderInfoBox({
  infoBox("County ranked one below", 
          CountyData$county[na.exclude(CountyData$ODrateRank == (CountyData$ODrateRank[CountyData$county == input$County]+1))], 
          icon = icon("sort-down"), color = "fuchsia", fill = TRUE)
  })
  

  ########################
  #median income
  output$Income <- renderInfoBox({
    infoBox("Median Income", 
            CountyData$median.income.estimate[CountyData$county == input$County], 
            icon = icon("dollar"), color = "purple")
  })
  
  output$IncomeCat <- renderInfoBox({
    infoBox("How you compare", 
            paste0("This county is ",CountyData$MedianIncomeCat[CountyData$county == input$County], " 1 standard deviation"), 
            icon = icon("question-circle-o"), color = "purple")
  })
  
  
  #ChildAlleg
  output$ChildAlleg <- renderInfoBox({
    infoBox("Child Abuse Allegations", 
            paste0(CountyData$RateChildAlleg[CountyData$county == input$County]," per 1000"), 
            icon = icon("child"), color = "aqua")
  })
  
  output$ChildAllegCat <- renderInfoBox({
    infoBox("How you compare", 
            paste0("This county is ",CountyData$RateChildAllegCat[CountyData$county == input$County], " 1 standard deviation"), 
            icon = icon("question-circle-o"), color = "aqua")
  })
  
  #Homeless
  output$Homeless <- renderInfoBox({
    infoBox("Homeless PIT", 
            paste0(round(CountyData$RateHomelessPIT[CountyData$county == input$County],1)," per 1000"), 
            icon = icon("home"), color = "purple")
  })
  
  output$HomelessCat <- renderInfoBox({
    infoBox("How you compare", 
            paste0("This county is ",CountyData$RateHomelessPITCat[CountyData$county == input$County], " 1 standard deviation"), 
            icon = icon("question-circle-o"), color = "purple")
  })
  
  #HIV
  output$HIV <- renderInfoBox({
    infoBox("% of HIV from IDU", 
            paste0(round(CountyData$RatioIDUtoAll[CountyData$county == input$County]*100,1),"% of confirmed HIV cases related to intravenous drug use"), 
            icon = icon("heartbeat"), color = "aqua")
  })
  
  output$HIVCat <- renderInfoBox({
    infoBox("How you compare", 
            paste0("This county is ",CountyData$RatioIDUtoAll_Cat[CountyData$county == input$County], " 1 standard deviation"), 
            icon = icon("question-circle-o"), color = "aqua")
  })
  
  #HepC
  output$HepC <- renderInfoBox({
    infoBox("New Hepatitis C Diagnoses", 
            paste0(round(CountyData$RateHepC[CountyData$county == input$County],1)," per 1000"), 
            icon = icon("heartbeat"), color = "purple")
  })
  
  output$HepCCat <- renderInfoBox({
    infoBox("How you compare", 
            paste0("This county is ",CountyData$RateHepCCat[CountyData$county == input$County], " 1 standard deviation"), 
            icon = icon("question-circle-o"), color = "purple")
  })
  
  #Warmline
  output$Warmline <- renderInfoBox({
    infoBox("Warmline Intake Calls", 
            paste0(round(CountyData$RateIntakeCalls1000[CountyData$county == input$County],1)," per 1000"), 
            icon = icon("phone"), color = "purple")
  })
  
  output$WarmlineCat <- renderInfoBox({
    infoBox("How you compare", 
            paste0("This county is ",CountyData$RateIntakeCalls1000Cat [CountyData$county == input$County], " 1 standard deviation"), 
            icon = icon("question-circle-o"), color = "purple")
  })
  
  #MPD
  output$MPD <- renderInfoBox({
    infoBox("Narcan Coverage in Municipal PDs", 
            paste0("This county has ", CountyData$Police.Coverage [CountyData$county == input$County]), 
            icon = icon("medkit"), color = "aqua")
  })
  
  #Disp
  output$Disp <- renderInfoBox({
    infoBox("Opioid Dispensation", 
            paste0(round(CountyData$AnnualDispPer1000[CountyData$county == input$County],1)," per 1000"), 
            icon = icon("pencil-square-o"), color = "purple")
  })
  
  output$DispCat <- renderInfoBox({
    infoBox("How you compare", 
            paste0("This county is ",CountyData$AnnualDispCat [CountyData$county == input$County], " 1 standard deviation"), 
            icon = icon("question-circle-o"), color = "purple")
  })
  
  #bupdocs
  output$BupDoc <- renderInfoBox({
    infoBox("Bupenorphrine Physicians", 
            paste0(round(CountyData$RateBupDocs[CountyData$county == input$County],1)," per 1000"), 
            icon = icon("user-md"), color = "aqua")
  })
  
  output$BupDocCat <- renderInfoBox({
    infoBox("How you compare", 
            paste0("This county is ",CountyData$RateBupDocsCat[CountyData$county == input$County], " 1 standard deviation"), 
            icon = icon("question-circle-o"), color = "aqua")
  })
  #Accred
  output$Accred <- renderInfoBox({
    infoBox("Accredited Treatment Facilities", 
            paste0(round(CountyData$RateAccred[CountyData$county == input$County],1)," per 1000"), 
            icon = icon("hospital-o"), color = "purple")
  })
  
  output$AccredCat <- renderInfoBox({
    infoBox("How you compare", 
            paste0("This county is ",CountyData$RateAccredCat [CountyData$county == input$County], " 1 standard deviation"), 
            icon = icon("question-circle-o"), color = "purple")
  })
  
  #MAenroll
  output$MAenroll <- renderInfoBox({
    infoBox("Avg Monthly MA enrollment", 
            paste0(round(CountyData$AvgMonthlyEnrollRate17[CountyData$county == input$County],1),"people per 1000"), 
            icon = icon("users"), color = "purple")
  })
  
  output$MAenrollCat <- renderInfoBox({
    infoBox("How you compare", 
            paste0("This county is ",CountyData$AvgMonthlyEnrollRate17Cat [CountyData$county == input$County], " 1 standard deviation"), 
            icon = icon("question-circle-o"), color = "purple")
  })
  
  #MAenroll
  output$MAenrollK <- renderInfoBox({
    infoBox("Avg Monthly MA enrollment", 
            paste0(round(CountyData$AvgMonthlyEnrollRate17_Kids[CountyData$county == input$County],1)," minors per 1000"), 
            icon = icon("users"), color = "purple")
  })
  
  output$MAenrollKCat <- renderInfoBox({
    infoBox("How you compare", 
            paste0("This county is ",CountyData$AvgMonthlyEnrollRate17KidCat [CountyData$county == input$County], " 1 standard deviation"), 
            icon = icon("question-circle-o"), color = "purple")
  })
  
  #MANarc
  output$MANarc <- renderInfoBox({
    infoBox("People with filled Narcan scripts", 
            paste0(round(CountyData$RateMANarcan1000[CountyData$county == input$County],1)," per 1000"), 
            icon = icon("users"), color = "aqua")
  })
  
  output$MANarcCat <- renderInfoBox({
    infoBox("How you compare", 
            paste0("This county is ",CountyData$RateMANarcan1000Cat [CountyData$county == input$County], " 1 standard deviation"), 
            icon = icon("question-circle-o"), color = "aqua")
  
  })
  #TBB
  output$TBB <- renderInfoBox({
    infoBox("Take Back Boxes", 
            paste0(round(CountyData$RateTBB1K[CountyData$county == input$County],1)," per 1000"), 
            icon = icon("inbox"), color = "aqua")
  })
  
  output$TBBCat <- renderInfoBox({
    infoBox("How you compare", 
            paste0("This county is ",CountyData$RateTBB1KCat [CountyData$county == input$County], " 1 standard deviation"), 
            icon = icon("question-circle-o"), color = "aqua")
    
  })
  
  output$Top10 <- renderTable({
    Top10 <- subset(CountyData[order(CountyData$ODrateRank),], subset = ODrateRank <= 10, select = c("ODrateRank","county","ODrate2016"))
    rename(Top10, "Rank" = ODrateRank, "County" = county, "OD Rate" = ODrate2016)
  }, caption =  "<span style='color:#003366'> Overdose Deaths per 100,000 - Top 10 Counties", 
  caption.placement = getOption("xtable.caption.placement", "top"), 
  caption.width = getOption("xtable.caption.width", NULL))
  
}))



  