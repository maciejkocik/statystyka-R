# TODO:
# porządny reset za każdym razem kiedy jest nowa gra i nowy poziom
# wyświetlanie linii regresji po odp
# przedział w zależności od poziomu (łatwy - 0.1, trudny - 0.05), można dodać inne
# życia (3 życia i game over, 2 na trudnym, 1 na hardcore)
# punkty (na łatwym 3 pkt za poprawna, na trudnym 5, na hardcore 10)

# dodatkowe rzeczy:
# timer
# dźwięki
# ładniejsze UI


library(shiny)
library(MASS)

levelList <- c("Łatwy", "Trudny")

shots <- function(){
  rho <- (sample(1:200, 1)-100)
  mvrnorm(100, mu=c(100, 100), Sigma=matrix(c(100, rho, rho, 100), 2))
}

ui <- fluidPage(
    titlePanel("Zgadnij korelację"),

    sidebarLayout(
        sidebarPanel(
            actionButton("new", "Nowa gra", class="btn-lg btn-primary", style="margin-bottom:20px;"),
            sliderInput("odp",
                        "Twoja odpowiedź:",
                        min = -1,
                        max = 1,
                        value = 0.0, step=0.01),
            actionButton("check", "Sprawdź odpowiedź", class="btn-success"),
            actionButton("end", "Zakończ grę", class="btn-danger")
        ),


        mainPanel(
            p("Wynik: "),
            textOutput("score"),
           plotOutput("mainPlot")
        )
    )
)


server <- function(input, output) {
  
    score <- reactiveVal(0)
    shot <- reactiveVal()
    
    observeEvent(input$new, {
      showModal(modalDialog(
        selectInput("level", "Nowa gra", levelList),
        title = "Wybierz poziom trudności",
        footer = tagList(
          actionButton("newGame", "Rozpocznij", class="btn-primary")
        )
      ))
    })
    
    observeEvent(input$newGame, {
      removeModal()
      score(0)
      shot <- shots()
      fit <- lm(shot[,1] ~ shot[,2])
      kor <- as.double(fit$coefficients[2])
      output$mainPlot <- renderPlot({
        plot(shot, sub=kor)
        if (input$level == "Łatwy")
          abline(fit)
        
      })
    })

    
    # sprawdzanie
    
    observeEvent(input$check, {
      cat(c(input$odp, as.double(fit$coefficients[2])))
      if (abs(input$odp - as.double(fit$coefficients[2]) <= 0.1))
        newScore <- score() + 1
      else
        newScore <- score() - 1
      
      score(newScore)
    })
    
    
    output$score <- renderText({
      score()                     
    })
    

}

shinyApp(ui = ui, server = server)
