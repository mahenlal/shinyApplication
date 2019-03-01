#
# Shiny web application Server Logic
# application by clicking 'Run App' above. application by clicking 'Run App' above.
#


library(shiny)
library(ggplot2)
library(dplyr)
# select columns to be used in the analysis
diam <- diamonds[,c(1:4,7)]
# Define server logic required to draw a plot
shinyServer(function(input, output) {
  output$distPlot <- renderPlot({
    # select diamonds (user input)
    diam <- filter(diamonds, grepl(input$cut, cut), grepl(input$col, color), grepl(input$clar, clarity))
    # linear regression model
    fit <- lm( price~carat, diam)
    # price prediction 
    pred <- predict(fit, newdata = data.frame(carat = input$car,
                                              cut = input$cut,
                                              color = input$col,
                                              clarity = input$clar))
    # plot using ggplot2
    plot <- ggplot(data=diam, aes(x=carat, y = price))+
      geom_point(aes(color = cut), alpha = 0.3)+
      geom_smooth(method = "lm")+
      geom_vline(xintercept = input$car, color = "red")+
      geom_hline(yintercept = pred, color = "green")
    plot
  })
  output$result <- renderText({
    # renders the text (predicted Price)
    diam <- filter(diamonds, grepl(input$cut, cut), grepl(input$col, color), grepl(input$clar, clarity))
    fit <- lm( price~carat, diam)
    pred <- predict(fit, newdata = data.frame(carat = input$car,
                                              cut = input$cut,
                                              color = input$col,
                                              clarity = input$clar))
    res <- paste(round(pred, digits = 2), "$")
    res
  })
  
})
