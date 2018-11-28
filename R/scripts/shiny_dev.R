library(shiny)

ui <- fluidPage(
  h1("Memory"),
  sidebarLayout(
    sidebarPanel(
      numericInput("val", "Next Value", 10)
    ),
    mainPanel(
      verbatimTextOutput("curval"),
      verbatimTextOutput("lstval")
    )
  )
)

server <- function(input,output,session) {
  rv <- reactiveValues(lstval = 0, curval = 0)

  observeEvent(input$val, {rv$lstval <- rv$curval; rv$curval <- input$val})

  curre <- reactive({req(input$val);  input$val; rv$curval})
  lstre <- reactive({req(input$val);  input$val; rv$lstval})

  output$curval <- renderPrint({sprintf("cur:%d",curre())})
  output$lstval <- renderPrint({sprintf("lst:%d",lstre())})
}
options(shiny.reactlog = TRUE)
shinyApp(ui, server)
