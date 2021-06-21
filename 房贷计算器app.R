#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
repayment1 = function(s,p,r,β,m){   # 面积、单位价格、按揭比例、月利息、期数（月）
  j = (1:m)          # 定义向量，用于中间过程计算
  X = s*p*r*β*(1+β)^(m)/((1+β)^(m)-1)              # 每期应还(本+息)
  I=c()
  I[j] = s*p*r*β*(1+β)^(j-1) - X*((1+β)^(j-1)-1)   # 每期应还利息
  c=c()              # 定义一个空向量用于计算等额偿还本金
  c=X-I              # 每期应还本金
  a=c()
  a[j] = s*p*r*(1+β)^j - ((1+β)^j-1)/(β) * X 
  round(sum(I),2)}      # 总贷款
repayment2 = function(s,p,r,β,m){   # 面积、单位价格、按揭比例、月利息、期数（月）
  j = (1:m)          # 定义向量，用于中间过程计算
  X = s*p*r*β*(1+β)^(m)/((1+β)^(m)-1)              # 每期应还(本+息)
  I=c()
  I[j] = s*p*r*β*(1+β)^(j-1) - X*((1+β)^(j-1)-1)   # 每期应还利息
  c=c()              # 定义一个空向量用于计算等额偿还本金
  c=X-I              # 每期应还本金
  a=c()
  a[j] = s*p*r*(1+β)^j - ((1+β)^j-1)/(β) * X 
  round(I,2)+s*p*r/m}      # 总本息
loan2 = function(L0, i, n, m){
  P = L0 / (m*n)
  L = (1 - (1:(m*n))/(m*n)) * L0
  j = (1+i)^(1/m)-1
  I = j * c(L0, L[-m*n])
  R = P + I
  
  return(sum(R))
}
loan3 = function(L0, i, n, m){
  P = L0 / (m*n)
  L = (1 - (1:(m*n))/(m*n)) * L0
  j = (1+i)^(1/m)-1
  I = j * c(L0, L[-m*n])
  R = P + I
  
  return(sum(I))
}
loan4 = function(L0, i, n, m){
  P = L0 / (m*n)
  L = (1 - (1:(m*n))/(m*n)) * L0
  j = (1+i)^(1/m)-1
  I = j * c(L0, L[-m*n])
  R = P + I
  
  return(R)
}
# Define UI for application that draws a histogram
shinyApp(
  ui = tagList(
    shinythemes::themeSelector(),
    navbarPage(
      "还款方式",
      tabPanel("等额本息",
               sidebarLayout(
                 sidebarPanel(
                   numericInput(inputId = "s",
                                label = "房子面积:",
                                value = 100,min=0,max=1000000),
                   numericInput(inputId = "p",
                                label = "房子单价:",
                                value = 100,min=0,max=1000000),
                   
                   selectInput(inputId = "r",
                               label = "按揭比例", 
                               choices = list("0.7" = 0.7, "0.8" = 0.8, "0.9" = 0.9,"1" = 1), 
                               selected = 1),
                   numericInput(inputId = "β",
                                label = "月利息:",
                                value = 0.01,min=0,max=0.2),
                   numericInput(inputId = "m",
                                label = "期数:",
                                value = 12,min=0,max=3600),
                   submitButton("计算"),
                 ),
                 mainPanel(
                   # Output: Formatted text for caption ----
                   h4("贷款额"),
                   textOutput("text3"),
                   h4("总利息"),
                   textOutput("text1"),
                   h4("总本息"),
                   textOutput("text4"),
                   h4("每月应还"),
                   textOutput("text2")))
               
               
      ),
      tabPanel("等额本金",sidebarLayout(
        sidebarPanel(
          numericInput(inputId = "s",
                       label = "房子面积:",
                       value = 100,min=0,max=1000000),
          numericInput(inputId = "p",
                       label = "房子单价:",
                       value = 100,min=0,max=1000000),
          
          selectInput(inputId = "r",
                      label = "按揭比例", 
                      choices = list("0.7" = 0.7, "0.8" = 0.8, "0.9" = 0.9,"1" = 1), 
                      selected = 1),
          numericInput(inputId = "i",
                       label = "年利息:",
                       value = 0.12,min=0,max=0.2),
          numericInput(inputId = "m1",
                       label = "年数:",
                       value = 1,min=0,max=3600),
          submitButton("计算"),
        ),
        mainPanel(
          # Output: Formatted text for caption ----
          h4("贷款额"),
          textOutput("text7"),
          h4("总利息"),
          textOutput("text6"),
          h4("总本息"),
          textOutput("text5"),
          h4("每月应还"),
          textOutput("text8")
        ))), tabPanel("小组成员",h4("201808010128郑佳明，201808010127赵奎"))
    )),
  
  
  # Define server logic required to draw a histogram
  server = function(input, output) {
    
    output$text1<- renderText( repayment1(input$s,input$p,as.numeric(input$r),input$β,input$m))
    
    output$text2<- renderText( repayment2(input$s,input$p,as.numeric(input$r),input$β,input$m))
    output$text3<- renderText(input$s*input$p*as.numeric(input$r))
    output$text4<- renderText(input$s*input$p*as.numeric(input$r)+ repayment1(input$s,input$p,as.numeric(input$r),input$β,input$m))
    output$text5<- renderText(loan2(input$s*input$p*as.numeric(input$r),input$i,input$m1,12))
    output$text6<- renderText(loan3(input$s*input$p*as.numeric(input$r),input$i,input$m1,12))
    output$text7<- renderText(input$s*input$p*as.numeric(input$r))
    output$text8<- renderText(loan4(input$s*input$p*as.numeric(input$r),input$i,input$m1,12))
  })


