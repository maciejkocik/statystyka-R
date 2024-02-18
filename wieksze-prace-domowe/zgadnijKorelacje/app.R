library(shiny)
library(shinyjs)
library(MASS)
library(readr)

ui <- fluidPage(
    titlePanel("Zgadnij korelację"),

    sidebarLayout(
        sidebarPanel(
            useShinyjs(),
            tags$div(id = "audio"),
            actionButton("fame", "HALL OF FAME ♛", class="btn-warning btn-block"),
            actionButton("newGame", "Nowa gra", class="btn-primary btn-block", style="margin-bottom:20px; margin-top:10px;"),
            hidden(
              sliderInput("odp",
                          "Twoja odpowiedź:",
                          min = -1,
                          max = 1,
                          value = 0.0, step=0.01),
            actionButton("check", "Sprawdź odpowiedź", class="btn-success"),
            actionButton("nextBoard", "Dalej", class="btn-primary")
            ),
            
        ),


        mainPanel(
            fluidRow(
              column(4,
                     span("Wynik: "),
                     span(textOutput("score")),
                     span(textOutput("scoreDiff"), style="color:green; font-weight:bold;"),
                     ),
              column(4,
                     span("Życia: "),
                     span(textOutput("lives")),
                     span(htmlOutput("livesDiff")),
                     ),
              column(4,
                     span("Combo: "),
                     span(textOutput("combo")),
              ),
            ),

            plotOutput("mainPlot")
        )
    )
)


server <- function(input, output) {
  
    fit <- data.frame()
    shot <- data.frame()
    
    score <- reactiveVal()
    lives <- reactiveVal()
    combo <- reactiveVal()
    maxCombo <- reactiveVal()
    kor <- reactiveVal()
    hasChecked <- reactiveVal()
    
    observeEvent(input$fame, {
      highscore <- read.csv("highscore.csv")
      highscore <- highscore[order(-highscore$Wynik, -highscore$Combo),]
      highscore <- transform(highscore, Combo = as.integer(Combo))
      
      if(nrow(highscore) >= 3){
        highscore[1, "Nick"] <- paste("🥇 ", highscore[1, "Nick"])
        highscore[2, "Nick"] <- paste("🥈 ", highscore[2, "Nick"])
        highscore[3, "Nick"] <- paste("🥉 ", highscore[3, "Nick"])
      }
      
      showModal(modalDialog(
        renderTable(highscore),
        title = "Najlepsze wyniki",
        footer = modalButton("Zamknij")
      ))
    })
    
    playSound <- function(sound){
      insertUI(selector = "#audio",
               where = "afterEnd",
               ui = tags$audio(src = sound, type = "audio/mp3", autoplay = TRUE, controls = NA, style="display:none;")
               , immediate = TRUE)
    }
    
    gameOver <- function(){
      playSound("over.mp3")
      shinyjs::disable("nextBoard")
      showModal(modalDialog(
        HTML(paste("Twój wynik to ", strong(score()))),
        textInput("nick", "Wpisz swój nick: "),
        title = "Game over!",
        footer = tagList(
          actionButton("save", "Zapisz", class="btn-primary"),
          modalButton("Wyjdź"))
      ))
    }
    
    observeEvent(input$save, {
      write_file(paste("\"", input$nick, "\", \"", score(), "\", \"", maxCombo(), "\" \n", sep=""), file = "highscore.csv", append=T)
      removeModal()
    })
    
    # funkcja losująca punkty
    shots <- function(){
      rho <- (sample(1:200, 1)-100)
      mvrnorm(100, mu=c(100, 100), Sigma=matrix(c(100, rho, rho, 100), 2))
    }
    
    # tworzenie nowej planszy
    newPlot <- function(){
      shot <<- shots() # <<- bo zmienna globalna
      fit <<- lm(shot[,1] ~ shot[,2])
      kor(round(as.double(fit$coefficients[2]), digits = 2)) # ustawia współczynnik korelacji
      output$mainPlot <- renderPlot({
        plot(shot, ylab="", xlab="")
      })
    }
    
    observeEvent(input$newGame, {
      playSound("sao.mp3")
      
      combo(0)
      maxCombo(0)
      score(0)
      lives(3)
      hasChecked(F)
      
      output$livesDiff <- renderText({""})
      output$scoreDiff <- renderText({""})
      
      shinyjs::show("odp")
      shinyjs::show("check")
      shinyjs::show("nextBoard")
      shinyjs::enable("check")
      shinyjs::disable("nextBoard")
      
      newPlot()
    })

    
    # sprawdzanie odpowiedzi
    observeEvent(input$check, {
      if(!hasChecked()){
        hasChecked(T)
        
        shinyjs::disable("check")
        shinyjs::enable("nextBoard")
        
        offset <- abs(input$odp - kor())
        
        ilePkt <- 0
        ileZyc <- 0
        
        if (offset <= 0.05){
          ilePkt = 5
          ileZyc = 1
          playSound("wow.mp3")
        } else if (offset <= 0.1){
          ilePkt = 1
          playSound("nice.mp3")
        } else {
          ileZyc = -1
          if (lives() - 1 > 0)
              playSound("sznuk.mp3")
        }
  
        score(score() + ilePkt)
        
        if (ilePkt != 0){
          combo(combo()+1)
          if(combo() > maxCombo())
            maxCombo(combo())
          output$scoreDiff <- renderText({paste("+",ilePkt, sep="")})
        }
        else {
          combo(0)
          output$scoreDiff <- renderText({""})
        }
        
        lives(lives() + ileZyc)
          
        if (ileZyc == -1)
          output$livesDiff <- renderText({"<font color='red'><b>-1</b></font>"})
        else if (ileZyc == +1)
          output$livesDiff <- renderText({"<font color='green'><b>+1</b></font>"})
        else
          output$livesDiff <- renderText({""})
          
        if (lives() == 0)
          gameOver()
  
        output$mainPlot <- renderPlot({
          plot(shot, main = kor(), ylab="", xlab="")
          abline(fit)
        })
        
      }
    })
    
    # następna plansza
    observeEvent(input$nextBoard, {
      hasChecked(F)
      shinyjs::enable("check")
      shinyjs::disable("nextBoard")
      newPlot()
    })
  
    
    output$score <- renderText({
      score()                     
    })
    
    output$lives <- renderText({
      lives()
    })
    
    output$combo <- renderText({
      combo()
    })
    
    

}

shinyApp(ui = ui, server = server)
