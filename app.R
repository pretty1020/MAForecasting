library(shiny)
library(data.table)
library(TTR)
library(forecast)
library(shinythemes)
library(rhandsontable)





DF = data.frame(Actual_data = 1:48, Moving_Average4 = 1:48, Moving_Average6 = 1:48, Moving_Average8 = 1:48, Moving_Average10 = 1:48,
                MA4_MAD = 1:48,MA6_MAD = 1:48,MA8_MAD = 1:48, MA10_MAD = 1:48,
                stringsAsFactors = FALSE)
numberofrows <- nrow(DF)

shinyApp(
    ui = tagList(
        
        navbarPage(
            theme = shinytheme("slate"),
            
            "MA Forecasting using DM Test",
            
            tabPanel("Moving Average (MA) Model Comparison", sidebarPanel(
                
                h4("Mean Absolute Deviation (MAD)", style = "color:#319177"),
                
                helpText("Moving Average 4", style = "color:#2E5894"),
                verbatimTextOutput("hotable4"),
                
                helpText("Moving Average 6", style = "color:#2E5894"),
                verbatimTextOutput("hotable5"),
                
                helpText("Moving Average 8", style = "color:#2E5894"),
                verbatimTextOutput("hotable6"),
                
                helpText("Moving Average 10", style = "color:#2E5894"),
                verbatimTextOutput("hotable7"),
                
                h4("Diebold-Mariano (DM) Test of Predictive Accuracy",style = "color:#319177"),
                helpText("The D.M test compares the forecast accuracy of two forecast methods (In this tool, we used alternative = less to determine  the best model.)", style = "color:#FFE4CD"),
                helpText("If p-value is <0.05, method 1 is more accurate than method 2.
                         If p-value >0.995, method 2 is more accurate than method 1. Otherwise, both methods have the same accuracy level.", style = "color:#FFE4CD"),
                
                helpText("Method1: MA4 vs Method2: MA6", style = "color:#0081AB"),
                verbatimTextOutput("hotable3"),
                
                helpText("Method1: MA4 vs Method2: MA8", style = "color:#0081AB"),
                verbatimTextOutput("hotable8"),
                
                helpText("Method1: MA4 vs Method2: MA10", style = "color:#0081AB"),
                verbatimTextOutput("hotable9"),
                
                helpText("Method1: MA6 vs Method2: MA8", style = "color:#0081AB"),
                verbatimTextOutput("hotable10"),
                
                helpText("Method1: MA6 vs Method2: MA10", style = "color:#0081AB"),
                verbatimTextOutput("hotable11"),
                
                
                helpText("Method1: MA8 vs Method2: MA10", style = "color:#0081AB"),
                verbatimTextOutput("hotable12"),
            ),
            
            mainPanel(
                
                helpText("This calculator will help you with short term forecast using different Moving Average (MA) model.
                         Aside from examining the Mean Absolute deviation (MAD) as the error measure, DM test is also helpful when choosing the best MA model", style = "color:#FFE4CD"),
                
                helpText("Direction : Highlight the Actual_data column and press delete. 
                Enter your historical data to run the past forecast and compare the error measures
                         of the Moving Average model based on MAD. Check if the chosen model is 
                         significantly different from other MA methods via DM test.", style = "color:#FFE4CD"),
                
                helpText("Note that the lower Mean Absolute Deviation (MAD), the better model but 
                         check also D.M test of predictive accuracy to compare the levels of accuracy ", style = "color:#FFE4CD"),
                
                helpText("To forecast the next period, check the value of each MA model based on the last actual row.
                         ", style = "color:#FFE4CD"),
                
                rHandsontableOutput("hotable2"),style = "color:#6B3FA0",
               
                
                
            )
            
            ),
            
            
            
            
            
            tabPanel("Contact", "Any Recommendations, Questions or Concerns?", 
                     helpText("Please send email: marianpailden@gmail.com", style="color:#0066CC"),
                     tags$a(href="https://github.com/pretty1020",
                            "Visit my Github Here", style="color:#0066CC"),
                     style="color:#6CDAE7",)
            
        )
    ),
    
    server = function(input, output, session) {
        
        
        
        output$hotable1 <- renderRHandsontable({rhandsontable(MyChanges1())})
        
        # Initiate your table
        previous2 <- reactive({DF})
        
        MyChanges2 <- reactive({
            
            if(is.null(input$hotable2)){return(previous2())}
            else if(!identical(previous2(),input$hotable2)){
                # hot.to.df function will convert your updated table into the dataframe
                mytable2 <- as.data.frame(hot_to_r(input$hotable2))
                
                mytable2 <- mytable2[1:numberofrows,]
                
                
                
                # Add some test cases
                mytable2[1:48,1][is.na(mytable2[,1])] <- 0
                
                mytable2[,2][is.na(mytable2[,2])] <- NA
                mytable2[,3][is.na(mytable2[,3])] <- NA
                mytable2[,4][is.na(mytable2[,4])] <- NA
                mytable2[,5][is.na(mytable2[,5])] <- NA
                mytable2[,6][is.na(mytable2[,6])] <- NA
                mytable2[,7][is.na(mytable2[,7])] <- NA
                mytable2[,8][is.na(mytable2[,8])] <- NA
                mytable2[,9][is.na(mytable2[,9])] <- NA
                
                SMA4 <- SMA(mytable2[,1], n=4,na.rm =TRUE )
                
                
                SMA6 <- SMA(mytable2[,1], n=6, na.rm =TRUE)
                SMA8 <- SMA(mytable2[,1], n=8,na.rm =TRUE)
                SMA10 <- SMA(mytable2[,1], n=10,na.rm =TRUE)
                
                mytable2[,2] <- SMA4
                mytable2[,3] <- SMA6
                mytable2[,4] <- SMA8
                mytable2[,5] <- SMA10
                
                #MAD
                
                
                
                #DMtest
                
                mytable2[1:3,6] <- NA
                mytable2[1:5,7] <- NA
                mytable2[1:7,8] <- NA
                mytable2[1:9,9] <- NA
                
                mytable2[4:48,6] <- abs(as.numeric(mytable2[4:48,1])- as.numeric(mytable2[4:48,2]))
                mytable2[6:48,7] <- abs(as.numeric(mytable2[6:48,1])- as.numeric(mytable2[6:48,3]))
                mytable2[8:48,8] <- abs(as.numeric(mytable2[8:48,1])- as.numeric(mytable2[8:48,4]))
                mytable2[10:48,9] <- abs(as.numeric(mytable2[10:48,1])- as.numeric(mytable2[10:48,5]))
                
                mytable2[,1:9]
                
                
            }
            
            
            
        })
        
        
        output$hotable2 <- renderRHandsontable({rhandsontable(MyChanges2())})
        
        
        # Initiate your table
        previous2 <- reactive({DF})
        
        MyChanges3 <- reactive({
            
            if(is.null(input$hotable2)){return(previous2())}
            else if(!identical(previous2(),input$hotable2)){
                # hot.to.df function will convert your updated table into the dataframe
                mytable2 <- as.data.frame(hot_to_r(input$hotable2))
                
                mytable2 <- mytable2[1:numberofrows,]
                
                
                
                # Add some test cases
                mytable2[1:48,1][is.na(mytable2[,1])] <- 0
                
                mytable2[,2][is.na(mytable2[,2])] <- NA
                mytable2[,3][is.na(mytable2[,3])] <- NA
                mytable2[,4][is.na(mytable2[,4])] <- NA
                mytable2[,5][is.na(mytable2[,5])] <- NA
                mytable2[,6][is.na(mytable2[,6])] <- NA
                mytable2[,7][is.na(mytable2[,7])] <- NA
                mytable2[,8][is.na(mytable2[,8])] <- NA
                mytable2[,9][is.na(mytable2[,9])] <- NA
                
                SMA4 <- SMA(mytable2[,1], n=4,na.rm =TRUE )
                
                
                SMA6 <- SMA(mytable2[,1], n=6, na.rm =TRUE)
                SMA8 <- SMA(mytable2[,1], n=8,na.rm =TRUE)
                SMA10 <- SMA(mytable2[,1], n=10,na.rm =TRUE)
                
                mytable2[,2] <- SMA4
                mytable2[,3] <- SMA6
                mytable2[,4] <- SMA8
                mytable2[,5] <- SMA10
                
                #MAD
                
                mytable2[1,6] <- mean(mytable2[4:48,6][mytable2[4:48,1]!=0])
                mytable2[1,7] <- mean(mytable2[6:48,7][mytable2[6:48,1]!=0])
                mytable2[1,8] <- mean(mytable2[8:48,8][mytable2[8:48,1]!=0])
                mytable2[1,9] <- mean(mytable2[10:48,9][mytable2[10:48,1]!=0])
                
                #DMtest
                dmtest1 <- dm.test(SMA4,SMA6,alternative = "less",
                                   h = 1,
                                   power = 2
                )
                
                dmtest1
                
            }
            
            
            
        })
        
        output$hotable3 <- renderPrint({(MyChanges3())})
        
        
        previous2 <- reactive({DF})
        
        MyChanges8 <- reactive({
            
            if(is.null(input$hotable2)){return(previous2())}
            else if(!identical(previous2(),input$hotable2)){
                # hot.to.df function will convert your updated table into the dataframe
                mytable2 <- as.data.frame(hot_to_r(input$hotable2))
                
                mytable2 <- mytable2[1:numberofrows,]
                
                
                
                # Add some test cases
                mytable2[1:48,1][is.na(mytable2[,1])] <- 0
                
                mytable2[,2][is.na(mytable2[,2])] <- NA
                mytable2[,3][is.na(mytable2[,3])] <- NA
                mytable2[,4][is.na(mytable2[,4])] <- NA
                mytable2[,5][is.na(mytable2[,5])] <- NA
                mytable2[,6][is.na(mytable2[,6])] <- NA
                mytable2[,7][is.na(mytable2[,7])] <- NA
                mytable2[,8][is.na(mytable2[,8])] <- NA
                mytable2[,9][is.na(mytable2[,9])] <- NA
                
                SMA4 <- SMA(mytable2[,1], n=4,na.rm =TRUE )
                
                
                SMA6 <- SMA(mytable2[,1], n=6, na.rm =TRUE)
                SMA8 <- SMA(mytable2[,1], n=8,na.rm =TRUE)
                SMA10 <- SMA(mytable2[,1], n=10,na.rm =TRUE)
                
                mytable2[,2] <- SMA4
                mytable2[,3] <- SMA6
                mytable2[,4] <- SMA8
                mytable2[,5] <- SMA10
                
                #MAD
                
                mytable2[1,6] <- mean(mytable2[4:48,6][mytable2[4:48,1]!=0])
                mytable2[1,7] <- mean(mytable2[6:48,7][mytable2[6:48,1]!=0])
                mytable2[1,8] <- mean(mytable2[8:48,8][mytable2[8:48,1]!=0])
                mytable2[1,9] <- mean(mytable2[10:48,9][mytable2[10:48,1]!=0])
                
                #DMtest
                dmtest2 <- dm.test(SMA4,SMA8,alternative = "less",
                                   h = 1,
                                   power = 2
                )
                
                dmtest2
                
            }
            
            
            
        })
        
        output$hotable8 <- renderPrint({(MyChanges8())})
        
        previous2 <- reactive({DF})
        
        MyChanges9 <- reactive({
            
            if(is.null(input$hotable2)){return(previous2())}
            else if(!identical(previous2(),input$hotable2)){
                # hot.to.df function will convert your updated table into the dataframe
                mytable2 <- as.data.frame(hot_to_r(input$hotable2))
                
                mytable2 <- mytable2[1:numberofrows,]
                
                
                
                # Add some test cases
                mytable2[1:48,1][is.na(mytable2[,1])] <- 0
                
                mytable2[,2][is.na(mytable2[,2])] <- NA
                mytable2[,3][is.na(mytable2[,3])] <- NA
                mytable2[,4][is.na(mytable2[,4])] <- NA
                mytable2[,5][is.na(mytable2[,5])] <- NA
                mytable2[,6][is.na(mytable2[,6])] <- NA
                mytable2[,7][is.na(mytable2[,7])] <- NA
                mytable2[,8][is.na(mytable2[,8])] <- NA
                mytable2[,9][is.na(mytable2[,9])] <- NA
                
                SMA4 <- SMA(mytable2[,1], n=4,na.rm =TRUE )
                
                
                SMA6 <- SMA(mytable2[,1], n=6, na.rm =TRUE)
                SMA8 <- SMA(mytable2[,1], n=8,na.rm =TRUE)
                SMA10 <- SMA(mytable2[,1], n=10,na.rm =TRUE)
                
                mytable2[,2] <- SMA4
                mytable2[,3] <- SMA6
                mytable2[,4] <- SMA8
                mytable2[,5] <- SMA10
                
                #MAD
                
                mytable2[1,6] <- mean(mytable2[4:48,6][mytable2[4:48,1]!=0])
                mytable2[1,7] <- mean(mytable2[6:48,7][mytable2[6:48,1]!=0])
                mytable2[1,8] <- mean(mytable2[8:48,8][mytable2[8:48,1]!=0])
                mytable2[1,9] <- mean(mytable2[10:48,9][mytable2[10:48,1]!=0])
                
                #DMtest
                dmtest3 <- dm.test(SMA4,SMA10,alternative = "less",
                                   h = 1,
                                   power = 2
                )
                
                dmtest3
                
            }
            
            
            
        })
        
        output$hotable9 <- renderPrint({(MyChanges9())})
        
        previous2 <- reactive({DF})
        
        MyChanges10 <- reactive({
            
            if(is.null(input$hotable2)){return(previous2())}
            else if(!identical(previous2(),input$hotable2)){
                # hot.to.df function will convert your updated table into the dataframe
                mytable2 <- as.data.frame(hot_to_r(input$hotable2))
                
                mytable2 <- mytable2[1:numberofrows,]
                
                
                
                # Add some test cases
                mytable2[1:48,1][is.na(mytable2[,1])] <- 0
                
                mytable2[,2][is.na(mytable2[,2])] <- NA
                mytable2[,3][is.na(mytable2[,3])] <- NA
                mytable2[,4][is.na(mytable2[,4])] <- NA
                mytable2[,5][is.na(mytable2[,5])] <- NA
                mytable2[,6][is.na(mytable2[,6])] <- NA
                mytable2[,7][is.na(mytable2[,7])] <- NA
                mytable2[,8][is.na(mytable2[,8])] <- NA
                mytable2[,9][is.na(mytable2[,9])] <- NA
                
                SMA4 <- SMA(mytable2[,1], n=4,na.rm =TRUE )
                
                
                SMA6 <- SMA(mytable2[,1], n=6, na.rm =TRUE)
                SMA8 <- SMA(mytable2[,1], n=8,na.rm =TRUE)
                SMA10 <- SMA(mytable2[,1], n=10,na.rm =TRUE)
                
                mytable2[,2] <- SMA4
                mytable2[,3] <- SMA6
                mytable2[,4] <- SMA8
                mytable2[,5] <- SMA10
                
                #MAD
                
                mytable2[1,6] <- mean(mytable2[4:48,6][mytable2[4:48,1]!=0])
                mytable2[1,7] <- mean(mytable2[6:48,7][mytable2[6:48,1]!=0])
                mytable2[1,8] <- mean(mytable2[8:48,8][mytable2[8:48,1]!=0])
                mytable2[1,9] <- mean(mytable2[10:48,9][mytable2[10:48,1]!=0])
                
                #DMtest
                dmtest4 <- dm.test(SMA6,SMA8,alternative = c("two.sided","less","greater"),
                                   h = 1,
                                   power = 2
                )
                
                dmtest4
                
            }
            
            
            
        })
        
        output$hotable10 <- renderPrint({(MyChanges10())})
        
        
        previous2 <- reactive({DF})
        
        MyChanges11 <- reactive({
            
            if(is.null(input$hotable2)){return(previous2())}
            else if(!identical(previous2(),input$hotable2)){
                # hot.to.df function will convert your updated table into the dataframe
                mytable2 <- as.data.frame(hot_to_r(input$hotable2))
                
                mytable2 <- mytable2[1:numberofrows,]
                
                
                
                # Add some test cases
                mytable2[1:48,1][is.na(mytable2[,1])] <- 0
                
                mytable2[,2][is.na(mytable2[,2])] <- NA
                mytable2[,3][is.na(mytable2[,3])] <- NA
                mytable2[,4][is.na(mytable2[,4])] <- NA
                mytable2[,5][is.na(mytable2[,5])] <- NA
                mytable2[,6][is.na(mytable2[,6])] <- NA
                mytable2[,7][is.na(mytable2[,7])] <- NA
                mytable2[,8][is.na(mytable2[,8])] <- NA
                mytable2[,9][is.na(mytable2[,9])] <- NA
                
                SMA4 <- SMA(mytable2[,1], n=4,na.rm =TRUE )
                
                
                SMA6 <- SMA(mytable2[,1], n=6, na.rm =TRUE)
                SMA8 <- SMA(mytable2[,1], n=8,na.rm =TRUE)
                SMA10 <- SMA(mytable2[,1], n=10,na.rm =TRUE)
                
                mytable2[,2] <- SMA4
                mytable2[,3] <- SMA6
                mytable2[,4] <- SMA8
                mytable2[,5] <- SMA10
                
                #MAD
                
                mytable2[1,6] <- mean(mytable2[4:48,6][mytable2[4:48,1]!=0])
                mytable2[1,7] <- mean(mytable2[6:48,7][mytable2[6:48,1]!=0])
                mytable2[1,8] <- mean(mytable2[8:48,8][mytable2[8:48,1]!=0])
                mytable2[1,9] <- mean(mytable2[10:48,9][mytable2[10:48,1]!=0])
                
                #DMtest
                dmtest5 <- dm.test(SMA6,SMA10,alternative = "less",
                                   h = 1,
                                   power = 2
                )
                
                dmtest5
                
            }
            
            
            
        })
        
        output$hotable11 <- renderPrint({(MyChanges11())})
        
        
        
        previous2 <- reactive({DF})
        
        MyChanges12 <- reactive({
            
            if(is.null(input$hotable2)){return(previous2())}
            else if(!identical(previous2(),input$hotable2)){
                # hot.to.df function will convert your updated table into the dataframe
                mytable2 <- as.data.frame(hot_to_r(input$hotable2))
                
                mytable2 <- mytable2[1:numberofrows,]
                
                
                
                # Add some test cases
                mytable2[1:48,1][is.na(mytable2[,1])] <- 0
                
                mytable2[,2][is.na(mytable2[,2])] <- NA
                mytable2[,3][is.na(mytable2[,3])] <- NA
                mytable2[,4][is.na(mytable2[,4])] <- NA
                mytable2[,5][is.na(mytable2[,5])] <- NA
                mytable2[,6][is.na(mytable2[,6])] <- NA
                mytable2[,7][is.na(mytable2[,7])] <- NA
                mytable2[,8][is.na(mytable2[,8])] <- NA
                mytable2[,9][is.na(mytable2[,9])] <- NA
                
                SMA4 <- SMA(mytable2[,1], n=4,na.rm =TRUE )
                
                
                SMA6 <- SMA(mytable2[,1], n=6, na.rm =TRUE)
                SMA8 <- SMA(mytable2[,1], n=8,na.rm =TRUE)
                SMA10 <- SMA(mytable2[,1], n=10,na.rm =TRUE)
                
                mytable2[,2] <- SMA4
                mytable2[,3] <- SMA6
                mytable2[,4] <- SMA8
                mytable2[,5] <- SMA10
                
                #MAD
                
                mytable2[1,6] <- mean(mytable2[4:48,6][mytable2[4:48,1]!=0])
                mytable2[1,7] <- mean(mytable2[6:48,7][mytable2[6:48,1]!=0])
                mytable2[1,8] <- mean(mytable2[8:48,8][mytable2[8:48,1]!=0])
                mytable2[1,9] <- mean(mytable2[10:48,9][mytable2[10:48,1]!=0])
                
                #DMtest
                dmtest6 <- dm.test(SMA8,SMA10,alternative = "less",
                                   h = 1,
                                   power = 2
                )
                
                dmtest6
                
            }
            
            
            
        })
        
        output$hotable12 <- renderPrint({(MyChanges12())})
        
        
        previous2 <- reactive({DF})
        
        MyChanges4 <- reactive({
            
            if(is.null(input$hotable2)){return(previous2())}
            else if(!identical(previous2(),input$hotable2)){
                # hot.to.df function will convert your updated table into the dataframe
                mytable2 <- as.data.frame(hot_to_r(input$hotable2))
                
                mytable2 <- mytable2[1:numberofrows,]
                
                
                
                # Add some test cases
                mytable2[1:48,1][is.na(mytable2[,1])] <- 0
                
                mytable2[,2][is.na(mytable2[,2])] <- NA
                mytable2[,3][is.na(mytable2[,3])] <- NA
                mytable2[,4][is.na(mytable2[,4])] <- NA
                mytable2[,5][is.na(mytable2[,5])] <- NA
                mytable2[,6][is.na(mytable2[,6])] <- NA
                mytable2[,7][is.na(mytable2[,7])] <- NA
                mytable2[,8][is.na(mytable2[,8])] <- NA
                mytable2[,9][is.na(mytable2[,9])] <- NA
                
                SMA4 <- SMA(mytable2[,1], n=4,na.rm =TRUE )
                
                
                SMA6 <- SMA(mytable2[,1], n=6, na.rm =TRUE)
                SMA8 <- SMA(mytable2[,1], n=8,na.rm =TRUE)
                SMA10 <- SMA(mytable2[,1], n=10,na.rm =TRUE)
                
                mytable2[,2] <- SMA4
                mytable2[,3] <- SMA6
                mytable2[,4] <- SMA8
                mytable2[,5] <- SMA10
                
                #MAD
                
                mytable2[1,6] <- mean(mytable2[4:48,6][mytable2[4:48,1]!=0])
                mytable2[1,7] <- mean(mytable2[6:48,7][mytable2[6:48,1]!=0])
                mytable2[1,8] <- mean(mytable2[8:48,8][mytable2[8:48,1]!=0])
                mytable2[1,9] <- mean(mytable2[10:48,9][mytable2[10:48,1]!=0])
                
                #DMtest
                
                mytable2[2:3,6] <- NA
                mytable2[2:5,7] <- NA
                mytable2[2:7,8] <- NA
                mytable2[2:9,9] <- NA
                
                mytable2[4:48,6] <- abs(as.numeric(mytable2[4:48,1])- as.numeric(mytable2[4:48,2]))
                mytable2[6:48,7] <- abs(as.numeric(mytable2[6:48,1])- as.numeric(mytable2[6:48,3]))
                mytable2[8:48,8] <- abs(as.numeric(mytable2[8:48,1])- as.numeric(mytable2[8:48,4]))
                mytable2[10:48,9] <- abs(as.numeric(mytable2[10:48,1])- as.numeric(mytable2[10:48,5]))
                
                mytable2[1,6]
                
                
            }
            
            
            
        })
        
        
        output$hotable4 <- renderPrint({(MyChanges4())})
        
        previous2 <- reactive({DF})
        
        MyChanges5 <- reactive({
            
            if(is.null(input$hotable2)){return(previous2())}
            else if(!identical(previous2(),input$hotable2)){
                # hot.to.df function will convert your updated table into the dataframe
                mytable2 <- as.data.frame(hot_to_r(input$hotable2))
                
                mytable2 <- mytable2[1:numberofrows,]
                
                
                
                # Add some test cases
                mytable2[1:48,1][is.na(mytable2[,1])] <- 0
                
                mytable2[,2][is.na(mytable2[,2])] <- NA
                mytable2[,3][is.na(mytable2[,3])] <- NA
                mytable2[,4][is.na(mytable2[,4])] <- NA
                mytable2[,5][is.na(mytable2[,5])] <- NA
                mytable2[,6][is.na(mytable2[,6])] <- NA
                mytable2[,7][is.na(mytable2[,7])] <- NA
                mytable2[,8][is.na(mytable2[,8])] <- NA
                mytable2[,9][is.na(mytable2[,9])] <- NA
                
                SMA4 <- SMA(mytable2[,1], n=4,na.rm =TRUE )
                
                
                SMA6 <- SMA(mytable2[,1], n=6, na.rm =TRUE)
                SMA8 <- SMA(mytable2[,1], n=8,na.rm =TRUE)
                SMA10 <- SMA(mytable2[,1], n=10,na.rm =TRUE)
                
                mytable2[,2] <- SMA4
                mytable2[,3] <- SMA6
                mytable2[,4] <- SMA8
                mytable2[,5] <- SMA10
                
                #MAD
                
                mytable2[1,6] <- mean(mytable2[4:48,6][mytable2[4:48,1]!=0])
                mytable2[1,7] <- mean(mytable2[6:48,7][mytable2[6:48,1]!=0])
                mytable2[1,8] <- mean(mytable2[8:48,8][mytable2[8:48,1]!=0])
                mytable2[1,9] <- mean(mytable2[10:48,9][mytable2[10:48,1]!=0])
                
                #DMtest
                
                mytable2[2:3,6] <- NA
                mytable2[2:5,7] <- NA
                mytable2[2:7,8] <- NA
                mytable2[2:9,9] <- NA
                
                mytable2[4:48,6] <- abs(as.numeric(mytable2[4:48,1])- as.numeric(mytable2[4:48,2]))
                mytable2[6:48,7] <- abs(as.numeric(mytable2[6:48,1])- as.numeric(mytable2[6:48,3]))
                mytable2[8:48,8] <- abs(as.numeric(mytable2[8:48,1])- as.numeric(mytable2[8:48,4]))
                mytable2[10:48,9] <- abs(as.numeric(mytable2[10:48,1])- as.numeric(mytable2[10:48,5]))
                
                mytable2[1,7]
                
                
            }
            
            
            
        })
        
        
        output$hotable5 <- renderPrint({(MyChanges5())})
        
        previous2 <- reactive({DF})
        
        MyChanges6 <- reactive({
            
            if(is.null(input$hotable2)){return(previous2())}
            else if(!identical(previous2(),input$hotable2)){
                # hot.to.df function will convert your updated table into the dataframe
                mytable2 <- as.data.frame(hot_to_r(input$hotable2))
                
                mytable2 <- mytable2[1:numberofrows,]
                
                
                
                # Add some test cases
                mytable2[1:48,1][is.na(mytable2[,1])] <- 0
                
                mytable2[,2][is.na(mytable2[,2])] <- NA
                mytable2[,3][is.na(mytable2[,3])] <- NA
                mytable2[,4][is.na(mytable2[,4])] <- NA
                mytable2[,5][is.na(mytable2[,5])] <- NA
                mytable2[,6][is.na(mytable2[,6])] <- NA
                mytable2[,7][is.na(mytable2[,7])] <- NA
                mytable2[,8][is.na(mytable2[,8])] <- NA
                mytable2[,9][is.na(mytable2[,9])] <- NA
                
                SMA4 <- SMA(mytable2[,1], n=4,na.rm =TRUE )
                
                
                SMA6 <- SMA(mytable2[,1], n=6, na.rm =TRUE)
                SMA8 <- SMA(mytable2[,1], n=8,na.rm =TRUE)
                SMA10 <- SMA(mytable2[,1], n=10,na.rm =TRUE)
                
                mytable2[,2] <- SMA4
                mytable2[,3] <- SMA6
                mytable2[,4] <- SMA8
                mytable2[,5] <- SMA10
                
                #MAD
                
                mytable2[1,6] <- mean(mytable2[4:48,6][mytable2[4:48,1]!=0])
                mytable2[1,7] <- mean(mytable2[6:48,7][mytable2[6:48,1]!=0])
                mytable2[1,8] <- mean(mytable2[8:48,8][mytable2[8:48,1]!=0])
                mytable2[1,9] <- mean(mytable2[10:48,9][mytable2[10:48,1]!=0])
                
                #DMtest
                
                mytable2[2:3,6] <- NA
                mytable2[2:5,7] <- NA
                mytable2[2:7,8] <- NA
                mytable2[2:9,9] <- NA
                
                mytable2[4:48,6] <- abs(as.numeric(mytable2[4:48,1])- as.numeric(mytable2[4:48,2]))
                mytable2[6:48,7] <- abs(as.numeric(mytable2[6:48,1])- as.numeric(mytable2[6:48,3]))
                mytable2[8:48,8] <- abs(as.numeric(mytable2[8:48,1])- as.numeric(mytable2[8:48,4]))
                mytable2[10:48,9] <- abs(as.numeric(mytable2[10:48,1])- as.numeric(mytable2[10:48,5]))
                
                mytable2[1,8]
                
                
            }
            
            
            
        })
        
        
        output$hotable6 <- renderPrint({(MyChanges6())})
        
        previous2 <- reactive({DF})
        
        MyChanges6 <- reactive({
            
            if(is.null(input$hotable2)){return(previous2())}
            else if(!identical(previous2(),input$hotable2)){
                # hot.to.df function will convert your updated table into the dataframe
                mytable2 <- as.data.frame(hot_to_r(input$hotable2))
                
                mytable2 <- mytable2[1:numberofrows,]
                
                
                
                # Add some test cases
                mytable2[1:48,1][is.na(mytable2[,1])] <- 0
                
                mytable2[,2][is.na(mytable2[,2])] <- NA
                mytable2[,3][is.na(mytable2[,3])] <- NA
                mytable2[,4][is.na(mytable2[,4])] <- NA
                mytable2[,5][is.na(mytable2[,5])] <- NA
                mytable2[,6][is.na(mytable2[,6])] <- NA
                mytable2[,7][is.na(mytable2[,7])] <- NA
                mytable2[,8][is.na(mytable2[,8])] <- NA
                mytable2[,9][is.na(mytable2[,9])] <- NA
                
                SMA4 <- SMA(mytable2[,1], n=4,na.rm =TRUE )
                
                
                SMA6 <- SMA(mytable2[,1], n=6, na.rm =TRUE)
                SMA8 <- SMA(mytable2[,1], n=8,na.rm =TRUE)
                SMA10 <- SMA(mytable2[,1], n=10,na.rm =TRUE)
                
                mytable2[,2] <- SMA4
                mytable2[,3] <- SMA6
                mytable2[,4] <- SMA8
                mytable2[,5] <- SMA10
                
                #MAD
                
                mytable2[1,6] <- mean(mytable2[4:48,6][mytable2[4:48,1]!=0])
                mytable2[1,7] <- mean(mytable2[6:48,7][mytable2[6:48,1]!=0])
                mytable2[1,8] <- mean(mytable2[8:48,8][mytable2[8:48,1]!=0])
                mytable2[1,9] <- mean(mytable2[10:48,9][mytable2[10:48,1]!=0])
                
                #DMtest
                
                mytable2[2:3,6] <- NA
                mytable2[2:5,7] <- NA
                mytable2[2:7,8] <- NA
                mytable2[2:9,9] <- NA
                
                mytable2[4:48,6] <- abs(as.numeric(mytable2[4:48,1])- as.numeric(mytable2[4:48,2]))
                mytable2[6:48,7] <- abs(as.numeric(mytable2[6:48,1])- as.numeric(mytable2[6:48,3]))
                mytable2[8:48,8] <- abs(as.numeric(mytable2[8:48,1])- as.numeric(mytable2[8:48,4]))
                mytable2[10:48,9] <- abs(as.numeric(mytable2[10:48,1])- as.numeric(mytable2[10:48,5]))
                
                mytable2[1,8]
                
                
            }
            
            
            
        })
        
        
        output$hotable6 <- renderPrint({(MyChanges6())})
        
        previous2 <- reactive({DF})
        MyChanges7 <- reactive({
            
            if(is.null(input$hotable2)){return(previous2())}
            else if(!identical(previous2(),input$hotable2)){
                # hot.to.df function will convert your updated table into the dataframe
                mytable2 <- as.data.frame(hot_to_r(input$hotable2))
                
                mytable2 <- mytable2[1:numberofrows,]
                
                
                
                # Add some test cases
                mytable2[1:48,1][is.na(mytable2[,1])] <- 0
                
                mytable2[,2][is.na(mytable2[,2])] <- NA
                mytable2[,3][is.na(mytable2[,3])] <- NA
                mytable2[,4][is.na(mytable2[,4])] <- NA
                mytable2[,5][is.na(mytable2[,5])] <- NA
                mytable2[,6][is.na(mytable2[,6])] <- NA
                mytable2[,7][is.na(mytable2[,7])] <- NA
                mytable2[,8][is.na(mytable2[,8])] <- NA
                mytable2[,9][is.na(mytable2[,9])] <- NA
                
                SMA4 <- SMA(mytable2[,1], n=4,na.rm =TRUE )
                
                
                SMA6 <- SMA(mytable2[,1], n=6, na.rm =TRUE)
                SMA8 <- SMA(mytable2[,1], n=8,na.rm =TRUE)
                SMA10 <- SMA(mytable2[,1], n=10,na.rm =TRUE)
                
                mytable2[,2] <- SMA4
                mytable2[,3] <- SMA6
                mytable2[,4] <- SMA8
                mytable2[,5] <- SMA10
                
                #MAD
                
                mytable2[1,6] <- mean(mytable2[4:48,6][mytable2[4:48,1]!=0])
                mytable2[1,7] <- mean(mytable2[6:48,7][mytable2[6:48,1]!=0])
                mytable2[1,8] <- mean(mytable2[8:48,8][mytable2[8:48,1]!=0])
                mytable2[1,9] <- mean(mytable2[10:48,9][mytable2[10:48,1]!=0])
                
                #DMtest
                
                mytable2[2:3,6] <- NA
                mytable2[2:5,7] <- NA
                mytable2[2:7,8] <- NA
                mytable2[2:9,9] <- NA
                
                mytable2[4:48,6] <- abs(as.numeric(mytable2[4:48,1])- as.numeric(mytable2[4:48,2]))
                mytable2[6:48,7] <- abs(as.numeric(mytable2[6:48,1])- as.numeric(mytable2[6:48,3]))
                mytable2[8:48,8] <- abs(as.numeric(mytable2[8:48,1])- as.numeric(mytable2[8:48,4]))
                mytable2[10:48,9] <- abs(as.numeric(mytable2[10:48,1])- as.numeric(mytable2[10:48,5]))
                
                mytable2[1,9]
                
                
            }
            
            
            
        })
        
        
        output$hotable7 <- renderPrint({(MyChanges7())})
        
        
        previous2 <- reactive({DF})
        MyChanges13 <- reactive({
            
            if(is.null(input$hotable2)){return(previous2())}
            else if(!identical(previous2(),input$hotable2)){
                # hot.to.df function will convert your updated table into the dataframe
                mytable2 <- as.data.frame(hot_to_r(input$hotable2))
                
                mytable2 <- mytable2[1:numberofrows,]
                
                
                
                # Add some test cases
                mytable2[1:48,1][is.na(mytable2[,1])] <- 0
                
                mytable2[,2][is.na(mytable2[,2])] <- NA
                mytable2[,3][is.na(mytable2[,3])] <- NA
                mytable2[,4][is.na(mytable2[,4])] <- NA
                mytable2[,5][is.na(mytable2[,5])] <- NA
                mytable2[,6][is.na(mytable2[,6])] <- NA
                mytable2[,7][is.na(mytable2[,7])] <- NA
                mytable2[,8][is.na(mytable2[,8])] <- NA
                mytable2[,9][is.na(mytable2[,9])] <- NA
                
                SMA4 <- SMA(mytable2[,1], n=4,na.rm =TRUE )
                
                
                SMA6 <- SMA(mytable2[,1], n=6, na.rm =TRUE)
                SMA8 <- SMA(mytable2[,1], n=8,na.rm =TRUE)
                SMA10 <- SMA(mytable2[,1], n=10,na.rm =TRUE)
                
                mytable2[,2] <- SMA4
                mytable2[,3] <- SMA6
                mytable2[,4] <- SMA8
                mytable2[,5] <- SMA10
                
                #MAD
                
                mytable2[1,6] <- mean(mytable2[4:48,6][mytable2[4:48,1]!=0])
                mytable2[1,7] <- mean(mytable2[6:48,7][mytable2[6:48,1]!=0])
                mytable2[1,8] <- mean(mytable2[8:48,8][mytable2[8:48,1]!=0])
                mytable2[1,9] <- mean(mytable2[10:48,9][mytable2[10:48,1]!=0])
                
                #DMtest
                
                mytable2[2:3,6] <- NA
                mytable2[2:5,7] <- NA
                mytable2[2:7,8] <- NA
                mytable2[2:9,9] <- NA
                
                mytable2[4:48,6] <- abs(as.numeric(mytable2[4:48,1])- as.numeric(mytable2[4:48,2]))
                mytable2[6:48,7] <- abs(as.numeric(mytable2[6:48,1])- as.numeric(mytable2[6:48,3]))
                mytable2[8:48,8] <- abs(as.numeric(mytable2[8:48,1])- as.numeric(mytable2[8:48,4]))
                mytable2[10:48,9] <- abs(as.numeric(mytable2[10:48,1])- as.numeric(mytable2[10:48,5]))
                
                mytable2 <- c(mytable2[1:48,1],mytable2[1:48,2])
                mytable2 
                
            }
            
            
            
        })
        
        
        
        
        
    })



