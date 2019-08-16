library(shiny)
library(shinydashboard)


shinyUI(
    dashboardPage(
      dashboardHeader(title = "PA Stands Together: County Dashboard", titleWidth = 400),
      dashboardSidebar(
        width = 250,
        selectInput("County", "What county do you want to explore?", choices = CountyData$county),
        sidebarMenu(
          menuItem("Back to PA Stands Together Homepage"),
          menuItem("PA Stands Together Shortcuts"),
            menuSubItem("Find Resources Near You", icon = icon("map-marker", lib = "font-awesome")),
            menuSubItem("Find a Local Navigator", icon = icon("user-circle")),
            menuSubItem("Become a Local Navigator", icon = icon("user-circle-o")),
            menuSubItem("Trainings/Educational Material", icon = icon("bullhorn", lib = "font-awesome")),
            menuSubItem("Register to Vote", icon = icon("external-link", lib = "font-awesome"))
                          )),
      dashboardBody(
          tabsetPanel(
            tabPanel("Dashboard",
          #header section with quick facts    
              fluidPage(
              
              #column(3, offset = 4, 
                     titlePanel(textOutput("title_panel")), 
                  #),
              infoBoxOutput("TotalPop"),
              infoBoxOutput("Rank"),
              infoBoxOutput("CountyAbove"),
              infoBoxOutput("RateOD"),
              infoBoxOutput("Percent"),
              infoBoxOutput("CountyBelow")
              
              ),       
           #full dashboard   
              fluidPage(
                  tabBox( width = 12,
                    tabPanel("Demographics and Social Determinants of Health",
                      #medincome
                        infoBoxOutput("Income"),
                        infoBoxOutput("IncomeCat"),
                        box(title = "Why it matters", width = 4, solidHeader = TRUE, background = "purple",
                           "Counties with higher median incomes are likely to have a lower overdose death rate"
                            ),
                      #childalleg  
                        infoBoxOutput("ChildAlleg"),
                        infoBoxOutput("ChildAllegCat"),
                        box(title = "Why it matters", width = 4, solidHeader = TRUE, background = "aqua",
                            "Counties with higher rates of validated allegations are likely to have a lower overdose death rate"
                        ),
                      #homeless 
                        infoBoxOutput("Homeless"),
                        infoBoxOutput("HomelessCat"),
                        box(title = "Why it matters", width = 4, solidHeader = TRUE, background = "purple",
                            "Counties with higher rates of homelessness are likely to have a higher overdose death rate"
                        ),
                      #HIV  
                        infoBoxOutput("HIV"),
                        infoBoxOutput("HIVCat"),
                        box(title = "Why it matters", width = 4, solidHeader = TRUE, background = "aqua",
                            "Counties with a higher percent of HIV cases related to IDU are likely to have a lower overdose death rate"
                        ),
                      #HEPc
                      infoBoxOutput("HepC"),
                      infoBoxOutput("HepCCat"),
                      box(title = "Why it matters", width = 4, solidHeader = TRUE, background = "purple",
                          "Counties with higher rates of new Hep C diagnoses among 15 to 34 year olds are likely to have a higher overdose death rate"
                      )
                        ),
                  
                    tabPanel(
                      title = "First Responders",
                      #warmline
                      infoBoxOutput("Warmline"),
                      infoBoxOutput("WarmlineCat"),
                      box(title = "Why it matters", width = 4, solidHeader = TRUE, background = "purple",
                          "Counties with higher call volume are likely to have a higher overdose death rate"),
                      #MPD    
                      infoBoxOutput("MPD"),
                      box(title = "Why it matters", width = 4, solidHeader = TRUE, background = "aqua",
                          "Counties with no municipal police department are likely to have higher overdose death rate"),
                      box(plotOutput("PolCovpie"))
                      ),
                  
                    
                      #tableOutput("MPD"),
                      #plotOutput("PolCovpie")),
                  
                   tabPanel(
                      title = "Prescriptions and Treatment", 
                    #Disp  
                      infoBoxOutput("Disp"),
                      infoBoxOutput("DispCat"),
                      box(title = "Why it matters", width = 4, solidHeader = TRUE, background = "purple",
                          "Counties with higher opioid dispensation rates are likely to have higher overdose death rate"),
                    #BupDocs
                      infoBoxOutput("BupDoc"),
                      infoBoxOutput("BupDocCat"),
                      box(title = "Why it matters", width = 4, solidHeader = TRUE, background = "aqua",
                        "Counties with higher rates of bupenorphrine physicians are likely to have higher overdose death rates"),
                    #Accred  
                    infoBoxOutput("Accred"),
                    infoBoxOutput("AccredCat"),
                    box(title = "Why it matters", width = 4, solidHeader = TRUE, background = "purple",
                        "Counties with higher rates of accredited treatment facilities are likely to have higher overdose death rate"),
                
                    #TBB
                    infoBoxOutput("TBB"),
                    infoBoxOutput("TBBCat"),
                    box(title = "Why it matters", width = 4, solidHeader = TRUE, background = "aqua",
                        "Counties with higher rates of prescription take back boxes are likely to have lower overdose death rates")
                  ),
                    tabPanel(
                      title = "Medical Assistance", 
                      
                      #MANarcan
                        infoBoxOutput("MANarc"),
                        infoBoxOutput("MANarcCat"),
                        box(title = "Why it matters", width = 4, solidHeader = TRUE, background = "aqua",
                            "Counties with higher rates of filled narcan prescriptions in this population are likely to have higher overdose death rates"),
                        
                      
                        #MAenroll  
                        infoBoxOutput("MAenroll"),
                        infoBoxOutput("MAenrollCat"),
                        box(title = "Why it matters", width = 4, solidHeader = TRUE, background = "purple",
                          "While enrollment itself is not related to overdose deaths, the models on medical assistance were the most highly correlated with the overdose death rate"),
                        infoBoxOutput("MAenrollK"),
                        infoBoxOutput("MAenrollKCat")
                        
                        
                      
                      )
                      #
                    
                      #  tableOutput("Enroll"),
                     # tableOutput("EnrollKid"),
                    #  tableOutput("MANarcan")
                     #   )))
          ))),
                  
          tabPanel("Top 10 Counties",
             tableOutput("Top10")      
              )
          
          
            
          
                  
                  ))))
                
              

