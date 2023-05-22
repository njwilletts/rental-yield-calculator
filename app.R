# LIBRARIES
library(tidyverse)
library(shiny)
library(shinythemes)

# HELPER FUNCTION
# Outputs bold html colour code depending on if x is positive or negative
  # negative = red, positive = green
colour_coding <- function(x){
  if (x < 0) {
    colour <- "<font color=\"#FF0000\"><b>"
  } else {
    colour <- "<font color=\"#1EBF52\"><b>"
  }
}

# USER INTERFACE
ui <- fluidPage(theme = shinytheme("simplex"),
  column(1),
  column(10,
        h3("Rental yield calculator"),
        hr(),
        h4("Property"),
        fluidRow(
          column(3, numericInput("land", "Land value", value = 0, step = 10000, min = 0)),
          column(3, numericInput("house", "House value", value = 0, step = 10000, min = 0)),
          column(3, numericInput("loan", "Loan amount", value = 0, step = 10000, min = 0)),
          column(3, numericInput("interest", "Interest rate (%)", value = 6.1, min = 0))
        ),
        h4("Annual income"),
        fluidRow(
          column(3, numericInput("rent", "Rent", value = 0, step = 1000, min = 0))
        ),
        h4("Annual expenses"),
        fluidRow(
          column(3, numericInput("insurance", "Insurance", value = 0, step = 1000, min = 0)),
          column(3, numericInput("rates", "Council rates", value = 0, step = 500, min = 0)),
          column(3, numericInput("bodycorp", "Bodycorp", value = 0, step = 500, min = 0)),
          column(3, numericInput("maintenance", "Maintenace (%)", value =1.5, min = 0))
        ),
        hr(),
        fluidRow(
          column(3, numericInput("target_yield", "Target yield (%)", value = 5.5))
        ),
        fluidRow(
          column(3, actionButton("calculate", "Calculate"))
        ),
        hr(),
        fluidRow(
          column(3, verbatimTextOutput("text_summary")),
          column(9, htmlOutput("text_profit"), htmlOutput("text_yield"))
        )
  ),
  column(1)
)

# SERVER
server <- function(input, output, session){
  
  # Create reactive variables from user supplied input
  land <- eventReactive(input$calculate, {input$land})
  house <- eventReactive(input$calculate, {input$house})
  loan <- eventReactive(input$calculate, {input$loan})
  interest <- eventReactive(input$calculate, {input$interest})
  rent <- eventReactive(input$calculate, {input$rent})
  insurance <- eventReactive(input$calculate, {input$insurance})
  rates <- eventReactive(input$calculate, {input$rates})
  bodycorp <- eventReactive(input$calculate, {input$bodycorp})
  maintenance <- eventReactive(input$calculate, {input$maintenance})
  target_yield <- eventReactive(input$calculate, {input$target_yield})
  
  # Derived reactive variables
  property <- eventReactive(input$calculate, {land() + house()})
  equity <- eventReactive(input$calculate, {property() - loan()})
  interest_amt <- eventReactive(input$calculate, {loan() * (interest()/100)})
  maintenance_amt <- eventReactive(input$calculate, {house() * (maintenance()/100)})
  net_profit <- eventReactive(input$calculate, {rent() - interest_amt() - maintenance_amt() - insurance() - rates() - bodycorp()})
  yield <- eventReactive(input$calculate, {
    if (equity() == 0) {
      0
    } else {
      net_profit() / equity()
      }
    })
  fair_value <- eventReactive(input$calculate, {
    if (net_profit() <= 0) {
      0
    } else {
      round(net_profit() / (target_yield() / 100), digits = 0)
    }
  })
  over_under_val <- eventReactive(input$calculate, {
   if (net_profit() <=0) {
     0
   } else {
     round((((property() / fair_value()) - 1) * 100), digits = 0)
   }
  })
  
  # Render text summary of net profit calculation
  output$text_summary <- renderText({
    paste(
      "Summary",
      "\n----------------------------",
      paste("\nProperty: ", format(property(), big.mark = ",", scientific = FALSE)), 
      paste("\nLoan: ", format(loan(), big.mark = ",", scientific = FALSE)), 
      paste("\nEquity: ", format(equity(), big.mark = ",", scientific = FALSE)), 
      "\n----------------------------",
      paste("\nRent: ", format(rent(), big.mark = ",", scientific = FALSE)), 
      paste("\nInterest: -", format(interest_amt(), big.mark = ",", scientific = FALSE)),
      paste("\nMaintenance: -", format(maintenance_amt(), big.mark = ",", scientific = FALSE)),
      paste("\nInsurance: -", format(insurance(), big.mark = ",", scientific = FALSE)),
      paste("\nRates: -", format(rates(), big.mark = ",", scientific = FALSE)),
      paste("\nBodycorp: -", format(bodycorp(), big.mark = ",", scientific = FALSE)),
      "\n----------------------------",
      paste("\nNet profit: ", format(net_profit(), big.mark = ",", scientific = FALSE)),
      paste("\nFair value: ", format(fair_value(), big.mark = ",", scientific = FALSE)) 
    )
  })
  
  # Render text describing net profit and return on equity
  output$text_profit <- renderText({
      paste("This property has a net profit of", colour_coding(net_profit()), format(net_profit(), big.mark = ",", scientific = FALSE), "</b></font>", 
            "which is a", colour_coding(yield()), sprintf("%0.1f%%", yield() * 100), "</b></font>", 
            "return on", "<b>",format(equity(), big.mark = ",", scientific = FALSE), "</b>", "equity.") 
  })
  # Render text describing if the property is under or over valued
  output$text_yield <- renderText({
    if (net_profit() < 0) {
      paste("The net profit of this property is negative so", colour_coding(net_profit()),"avoid at all costs.", "</b></font>")  
    } else if (property() > fair_value()) {
      paste("When targeting a yield of", "<b>", sprintf("%0.1f%%", target_yield()), "</b>", "this property is", 
            colour_coding(-over_under_val()), sprintf("%1.f%%", over_under_val()), "overvalued", "</b></font>",
            "compared to the fair value of", "<b>", paste(format(fair_value(), big.mark = ",", scientific = FALSE), ".", sep = ""), "</b>"
            )  
    } else if (property() < fair_value()){
      paste("When targeting a yield of", "<b>", sprintf("%0.1f%%", target_yield()), "</b>", "this property is", 
            colour_coding(-over_under_val()), sprintf("%1.f%%", -over_under_val()), "undervalued", "</b></font>",
            "compared to the fair value of", "<b>", paste(format(fair_value(), big.mark = ",", scientific = FALSE), ".", sep = ""), "</b>"
            )      
    }
  })
}

# RUN APP
shinyApp(ui = ui, server = server)