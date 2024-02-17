library(shiny)

ui <- fluidPage(

    titlePanel("Centralne Twierdzenie Graniczne"),

    sidebarLayout(
        sidebarPanel(
            sliderInput("nrep",
                        "Liczba losowań",
                        min = 1,
                        max = 50000,
                        value = 10000),
            sliderInput("n",
                        "Liczebność próby",
                        min = 5,
                        max = 100,
                        value = 20),
            sliderInput("mean1",
                        "Średnia pierwszej populacji",
                        min = 0,
                        max = 200,
                        value = 165),
            sliderInput("mean2",
                        "Średnia drugiej populacji",
                        min = 0,
                        max = 200,
                        value = 175),
            sliderInput("sd1",
                        "Odchylenie standardowe pierwszej populacji",
                        min = 0,
                        max = 30,
                        value = 20),
            sliderInput("sd2",
                        "Odchylenie standardowe drugiej populacji",
                        min = 0,
                        max = 30,
                        value = 10),
        ),


        mainPanel(
            plotOutput("mainPlot")
        )
    )
)

server <- function(input, output) {

    output$mainPlot <- renderPlot({
      t_red = adjustcolor('Red', alpha.f=0.5)
      t_blue = adjustcolor('Blue', alpha.f=0.5)
      
      p1 <- matrix(rnorm(input$n*input$nrep, input$mean1, input$sd1), input$nrep)
      p2 <- matrix(rnorm(input$n*input$nrep, input$mean2, input$sd2), input$nrep)
      mean_p1 <- apply(p1, 1, mean)
      mean_p2 <- apply(p2, 1, mean)
      
      bins <- seq(min(c(mean_p1, mean_p2))-1,
                  max(c(mean_p1, mean_p2))+1,
                  by=1)
      
      hist1 = hist(mean_p1, plot=F)
      hist2 = hist(mean_p2, plot=F)
    
      n <- input$n
      mu1 <- input$mean1
      sd1 <- input$sd1
      mu2 <- input$mean2
      sd2 <- input$sd2
      
      hist(mean_p1, col=t_red, freq=F, 

           ylim=c(0, max(hist1$density, hist2$density)),
           breaks=bins, main="")
           
                        
      hist(mean_p2, col=t_blue, breaks=bins, freq=F, add=T)
      curve(dnorm(x, mean = mu1, sd = sd1), col ="red", add=T)
      curve(dnorm(x, mean = mu2, sd = sd2), col ="blue", add=T)
      
      title(  
        bquote(
          atop(
          n[1] == .(n) * "," ~
          mu[1] == .(mu1) * "," ~
          sigma[1] == .(sd1),
          
          n[1] == .(n) * "," ~
          mu[1] == .(mu2) * "," ~
          sigma[1] == .(sd2))
          )
      )

    })

}

shinyApp(ui = ui, server = server)



