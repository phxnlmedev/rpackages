
library(shiny)
library(shinyjs)

ui <- fluidPage(

  headerPanel("Title"), shinyjs::useShinyjs(),

  sidebarPanel(id = "mySideBar",
             #  radioButtons("toggle", "Some Buttons",
             #               choices = list("Choice 1" = 1,
             #                              "Choice 2" = 2),
             #               selected = "Choice 2")),
             checkboxInput("Infusions","Infusions",value=FALSE)),

  sidebarPanel(id = "mySideBar2",
               radioButtons("otherButtons","Buttons",
                            choices = list("Choice 1"=1,
                                           "Choice 2"=2)),
               radioButtons("moreButtons","Buttons",
                            choices = list("Choice 1"= 1,
                                           "Choice 2" = 2))

  )
)


server<-function(input,output,session){

  shinyjs::onclick("advanced2",
                   shinyjs::toggle(id = "advanced2", anim = TRUE))

  observeEvent(input$Infusions, {
   # if(input$toggle == "1"){
    if ( input$Infusions == TRUE ) {
      shinyjs::disable(id = "mySideBar2")
    } else {
      shinyjs::enable(id = "mySideBar2")
    }
  })

}

shinyApp(ui,server)
