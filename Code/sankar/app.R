library(shiny)
library(shinydashboard)

interestRec = readRDS("D:/NUS/FYP/data/interestRec")
strengthRec = readRDS("D:/NUS/FYP/data/interestRec")
upskillRec  = readRDS("D:/NUS/FYP/data/interestRec")
refreshRec  = readRDS("D:/NUS/FYP/data/interestRec")
challengeRec = readRDS("D:/NUS/FYP/data/interestRec")

ui <- dashboardPage( 
  dashboardHeader(title = "Workplace Learning System"),
  dashboardSidebar(
    selectInput(inputId = "user",label = "User Name:",
                choices = list("97d0a65c"="97d0a65c",
                               "b1459d23"="b1459d23",
                               "c80bffb2"="c80bffb2",
                               "c930cc66"="c930cc66",
                               "f10f490e"="f10f490e",
                               "f810564e"="f810564e"
                )),
    actionButton("login", "Login"),
    hr(),
    hr(),
    sidebarMenu("Learn", id = "content",
                menuItem("Streams",tabName ="Streams", icon=icon("inbox","fa-1x")),
                menuItem("Assesments",tabName = "Assesments",icon=icon("diagnoses","fa-1x"))
                )
   
  ),
  dashboardBody(
    tags$head(tags$style(HTML('
    
    /* active selected tab in the sidebarmenu */
                          .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                          background-color: rgb(107,194,0);
                          color: rgb(255,255,255);font-weight: bold;font-size: 18px;
                          }
    /* other links in the sidebarmenu */
                          .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                          background-color: rgb(28, 6, 150);
                          color: rgb(255,255,255);font-weight: bold;
                          }

    /* other links in the sidebarmenu when hovered */
                          .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                          background-color: rgb(232,245,251);color: rgb(0,144,197);font-weight: bold;
                          }                      
                          
                          
    #                           '))),
    uiOutput("userLogged"),
    hr(),
    box(
      title = span( icon("dumbbell","fa-3x"), "Strength"),  background = "blue",
      "You looked similar decks before",
      hr(),
      verbatimTextOutput("strength1"),
      verbatimTextOutput("strength2"),
      verbatimTextOutput("strength3"),
      verbatimTextOutput("strength4"),
      verbatimTextOutput("strength5"),
      splitLayout(cellWidths = c("90%", "90%"),
                  actionLink("sprev", "Prev",icon = icon("chevron-left")),
                  actionLink("snext", "Next",icon = icon("chevron-right"))
      )
      
    ),
    box(
      title = span( icon("graduation-cap","fa-3x"), "Upskill"),  background = "green",
      "you might want to improve on this area ",
      hr(),
      verbatimTextOutput("upskill1"),
      verbatimTextOutput("upskill2"),
      verbatimTextOutput("upskill3"),
      verbatimTextOutput("upskill4"),
      verbatimTextOutput("upskill5"),
      splitLayout(cellWidths = c("90%", "90%"),
                  actionLink("uprev", "Prev",icon = icon("chevron-left")),
                  actionLink("unext", "Next",icon = icon("chevron-right"))
      )
    ),
    box(
      title = span( icon("brain","fa-3x"), "Refresh"),  background = "orange",
      "you might want to refresh your memory ",
      hr(),
      verbatimTextOutput("refresh1"),
      verbatimTextOutput("refresh2"),
      verbatimTextOutput("refresh3"),
      verbatimTextOutput("refresh4"),
      verbatimTextOutput("refresh5"),
      splitLayout(cellWidths = c("90%", "100%"),
                  actionLink("rprev", "Prev",icon = icon("chevron-left")),
                  actionLink("rnext", "Next",icon = icon("chevron-right"))
      )
    ),
    box(
      title = span( icon("heart","fa-3x"), "Interest"),  background = "purple",
      "you might interested on below decks",
      hr(),
      verbatimTextOutput("interest1"),
      verbatimTextOutput("interest2"),
      verbatimTextOutput("interest3"),
      verbatimTextOutput("interest4"),
      verbatimTextOutput("interest5"),
      splitLayout(cellWidths = c("90%", "90%"),
      actionLink("iprev", "Prev",icon = icon("chevron-left")),
      actionLink("inext", "Next",icon = icon("chevron-right"))
      )
    ),
    box(
      title = span( icon("hiking","fa-3x"),"Challenge of the Day"),  background = "aqua",
      "Do you want to Challenge yourself ",
      hr(),
      verbatimTextOutput("challenge1"),
      verbatimTextOutput("challenge2"),
      verbatimTextOutput("challenge3"),
      verbatimTextOutput("challenge4"),
      verbatimTextOutput("challenge5"),
      splitLayout(cellWidths = c("90%", "90%"),
                  actionLink("cprev", "Prev",icon = icon("chevron-left")),
                  actionLink("cnext", "Next",icon = icon("chevron-right"))
      )
    )
    
    
    
   
  )
)

server <- function(input, output) {
  # initialze index value as 1 for all recommendation tab

  values <- reactiveValues(strengthindex = 1,upskillindex = 1,refreshindex = 1,interestindex = 1,challengeindex = 1,logged = "N")
  
  # create empty matrix for streams data
  userInterestDeck = data.frame()
  userStrengthDeck = data.frame()
  userUpkskillDeck = data.frame()
  userRefreshDeck = data.frame()
  userChallengeDeck = data.frame()
  
  # create empty matrix for assesment data
  userInterestDeckAssesment = data.frame()
  userStrengthDeckAssesment = data.frame()
  userUpkskillDeckAssesment = data.frame()
  userRefreshDeckAssesment= data.frame()
  userChallengeDeckAssesment= data.frame()
  
  output$userLogged <-  renderUI({
    h1(paste("Welcome back", userName(),"!  Recommendations for you today  "))
  })
  
 
  observeEvent(input$content, {
     print("event handled")
     print(paste("selected menu",input$content))
     if(values$logged == "Y")
     {
       values$strengthindex = 1
       values$upskillindex = 1
       values$refreshindex = 1
       values$interestindex = 1
       values$challengeindex = 1
       
       strength_load()
       upskill_load()
       refresh_load()
       interest_load()
       challenge_load()
     }
  })
  
  userName <- eventReactive(input$login, {
    
    values$logged = "Y"
    
    # load streams content for the user
    userStrengthDeck  <<- as.matrix(strengthRec [ strengthRec$masked_user_id == input$user,2])
    userUpkskillDeck  <<- as.matrix(upskillRec  [ upskillRec$masked_user_id == input$user,2])
    userRefreshDeck   <<- as.matrix(refreshRec [ refreshRec$masked_user_id == input$user,2])
    userInterestDeck  <<- as.matrix(interestRec [ interestRec$masked_user_id == input$user,2])
    userChallengeDeck <<- as.matrix(challengeRec [ challengeRec$masked_user_id == input$user,2])
    
    # load assesment content for the user
    userStrengthDeckAssesment  <<- as.matrix(strengthRec [ strengthRec$masked_user_id == input$user,2])
    userUpkskillDeckAssesment  <<- as.matrix(upskillRec  [ upskillRec$masked_user_id == input$user,2])
    userRefreshDeckAssesment  <<- as.matrix(refreshRec [ refreshRec$masked_user_id == input$user,2])
    userInterestDeckAssesment  <<- as.matrix(interestRec [ interestRec$masked_user_id == input$user,2])
    userChallengeDeckAssesment <<- as.matrix(challengeRec [ challengeRec$masked_user_id == input$user,2])
    
    strength_load()
    upskill_load()
    refresh_load()
    interest_load()
    challenge_load()
    input$user
    
    })
  
 strength_load = function()
  { 
    if (input$content == "Streams")
      strengthdeck = userStrengthDeck [row(userStrengthDeck) >= values$strengthindex & row(userStrengthDeck) < values$strengthindex + 5  ,]
    else
      strengthdeck = userStrengthDeckAssesment [row(userStrengthDeckAssesment) >= values$strengthindex & row(userStrengthDeckAssesment) < values$strengthindex + 5  ,]
    loadStrengthText(strengthdeck)  
 }
 
  upskill_load = function()
 { 
    if (input$content == "Streams")
        upskilldeck = userUpkskillDeck [row(userUpkskillDeck) >= values$upskillindex & row(userUpkskillDeck) < values$upskillindex + 5  ,]
    else
        upskilldeck = userUpkskillDeckAssesment [row(userUpkskillDeckAssesment) >= values$upskillindex & row(userUpkskillDeckAssesment) < values$upskillindex + 5  ,]
   loadUpskillText(upskilldeck)  
  }
  
  refresh_load = function()
  { 
    if (input$content == "Streams")
       refreshdeck = userRefreshDeck [row(userRefreshDeck) >= values$refreshindex & row(userRefreshDeck) < values$refreshindex + 5  ,]
    else
      refreshdeck = userRefreshDeckAssesment [row(userRefreshDeckAssesment) >= values$refreshindex & row(userRefreshDeckAssesment) < values$refreshindex + 5  ,]
    loadRefreshText(refreshdeck)  
  }
  
  interest_load = function()
  { 
    if (input$content == "Streams")
        interestdeck = userInterestDeck [row(userInterestDeck) >= values$interestindex & row(userInterestDeck) < values$interestindex + 5  ,]
    else
      interestdeck = userInterestDeckAssesment [row(userInterestDeckAssesment) >= values$interestindex & row(userInterestDeckAssesment) < values$interestindex + 5  ,]
    loadInterestText(interestdeck)  
  }
  
  challenge_load = function()
  { 
    if (input$content == "Streams")
        challengedeck = userChallengeDeck [row(userChallengeDeck) >= values$challengeindex & row(userChallengeDeck) < values$challengeindex + 5  ,]
    else
      challengedeck = userChallengeDeckAssesment [row(userChallengeDeckAssesment) >= values$challengeindex & row(userChallengeDeckAssesment) < values$challengeindex + 5  ,]
    loadChallengeText(challengedeck)  
  }
  
  observeEvent(input$sprev, {
    if (values$strengthindex >1)
      values$strengthindex = values$strengthindex -5 
    strength_load()
  })
  
  observeEvent(input$snext, {
    
    if (values$strengthindex < 45)
      values$strengthindex = values$strengthindex + 5 
    strength_load()
    
  })
  
  
  observeEvent(input$uprev, {
    if (values$upskillindex >1)
      values$upskillindex = values$upskillindex -5 
    upskill_load()
  })
  
  observeEvent(input$unext, {
    
    if (values$upskillindex < 45)
      values$upskillindex = values$upskillindex + 5 
    upskill_load()
    
  })
  
  
  observeEvent(input$rprev, {
    if (values$refreshindex >1)
      values$refreshindex = values$refreshindex -5 
    refresh_load()
  })
  
  observeEvent(input$rnext, {
    
    if (values$refreshindex < 45)
      values$refreshindex = values$refreshindex + 5 
    refresh_load()
    
  })
  
  
  observeEvent(input$iprev, {
    if (values$interestindex >1)
      values$interestindex = values$interestindex -5 
    interest_load()
  })
  
  observeEvent(input$inext, {
    
    if (values$interestindex < 45)
      values$interestindex = values$interestindex + 5 
    interest_load()
    
  })
  
  
  observeEvent(input$cprev, {
    if (values$challengeindex >1)
      values$challengeindex = values$challengeindex -5 
    challenge_load()
  })
  
  observeEvent(input$cnext, {
    
    if (values$challengeindex < 45)
      values$challengeindex = values$challengeindex + 5 
    challenge_load()
    
  })
  
  

  

    
  loadStrengthText= function(strengthdeck)
  {
    output$strength1 <- renderText({
      if(!is.null( strengthdeck)) 
        strengthdeck[1]
    })
    
    output$strength2 <- renderText({
      if(!is.null( strengthdeck)) 
        strengthdeck[2]
    })
    
    output$strength3 <- renderText({
      if(!is.null( strengthdeck)) 
        strengthdeck[3]
    })
    
    output$strength4 <- renderText({
      if(!is.null( strengthdeck)) 
        strengthdeck[4]
    })
    
    output$strength5 <- renderText({
      if(!is.null( strengthdeck)) 
        strengthdeck[5]
    })
  }
  
 loadInterestText= function(interestdeck)
  {
 
  output$interest1 <- renderText({
    if(!is.null(interestdeck)) 
      interestdeck[1]
  })
  
  output$interest2 <- renderText({
    if(!is.null(interestdeck)) 
    interestdeck[2]
  })
  
  output$interest3 <- renderText({
    if(!is.null(interestdeck)) 
    interestdeck[3]
  })
  
  output$interest4 <- renderText({
    if(!is.null(interestdeck)) 
    interestdeck[4]
  })
  
  output$interest5 <- renderText({
    if(!is.null(interestdeck)) 
    interestdeck[5]
  })
 }
 
 loadUpskillText= function(upskilldeck)
 {
   output$upskill1 <- renderText({
     if(!is.null(upskilldeck)) 
       upskilldeck[1]
   })
   
   output$upskill2 <- renderText({
     if(!is.null(upskilldeck)) 
       upskilldeck[2]
   })
   
   output$upskill3 <- renderText({
     if(!is.null(upskilldeck)) 
       upskilldeck[3]
   })
   
   output$upskill4 <- renderText({
     if(!is.null(upskilldeck)) 
       upskilldeck[4]
   })
   
   output$upskill5 <- renderText({
     if(!is.null(upskilldeck)) 
       upskilldeck[5]
   })
 }
 loadRefreshText= function(refreshdeck)
 {
   output$refresh1 <- renderText({
     if(!is.null(refreshdeck)) 
       refreshdeck[1]
   })
   
   output$refresh2 <- renderText({
     if(!is.null(refreshdeck)) 
       refreshdeck[2]
   })
   
   output$refresh3 <- renderText({
     if(!is.null(refreshdeck)) 
       refreshdeck[3]
   })
   
   output$refresh4 <- renderText({
     if(!is.null(refreshdeck)) 
       refreshdeck[4]
   })
   
   output$refresh5 <- renderText({
     if(!is.null(refreshdeck)) 
       refreshdeck[5]
   })
 }
 
 loadChallengeText= function(challengedeck)
 {
   output$challenge1 <- renderText({
     if(!is.null(challengedeck)) 
       challengedeck[1]
   })
   
   output$challenge2 <- renderText({
     if(!is.null(challengedeck)) 
       challengedeck[2]
   })
   
   output$challenge3 <- renderText({
     if(!is.null(challengedeck)) 
       challengedeck[3]
   })
   
   output$challenge4 <- renderText({
     if(!is.null(challengedeck)) 
       challengedeck[4]
   })
   
   output$challenge5 <- renderText({
     if(!is.null(challengedeck)) 
       challengedeck[5]
   })
 }
}

shinyApp(ui, server)