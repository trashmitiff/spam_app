
library(tidyverse)
library(tidytuesdayR)
library(reshape2)
library(caTools)
library(e1071) 
library(caret)
library(randomForest)

#Prepare and clean data derived from tidy tuesday
tuesdata <- tidytuesdayR::tt_load('2023-08-15')
spam <- tuesdata$spam
spam_clean = spam %>% 
  na.omit()%>%
  rename("status"=yesno)%>%
  mutate(status=as.factor(recode(status,"y"=1,"n"=0)))

#Read in dataset on the performance metrics of the two ML models used
summary = read.csv("../spam_app/summary.csv", header=TRUE)

#Prepare models
svmModel = svm(formula = status ~ ., 
              data = spam_clean, 
              type = 'C-classification', 
              kernel = 'radial',
              cost = 9,
              probability = TRUE) 

rfModel = randomForest(x=spam_clean[-7],
                       y=spam_clean$status,
                       ntree=500,
                       mtry = 2)
ui <- fluidPage(

    # Application title
    titlePanel("Spam Email Classification"),

    sidebarLayout(
        sidebarPanel(
            selectInput("alg",
                        label = "Select the Machine Learning Algorithm:",
                        choices = c("Support Vector Machines"="svm","RandomForest"="rf")),
            sliderInput("crl.tot",
                        label = "Total length of uninterrupted sequences of capitals:",
                        min = 1,
                        max = 500,
                        value = 10,
                        step = 1),
            sliderInput("dollar",
                        label = "Occurences of '$' (% of total characters)",
                        min = 0,
                        max = 4,
                        value = 1,
                        step = 0.01),
            sliderInput("bang",
                        label = "Occurences of '!' (% of total characters)",
                        min = 0,
                        max = 4,
                        value = 1,
                        step = 0.01),
            sliderInput("money",
                        label = "Occurrences of ‘money’ (% of total characters)",
                        min = 0,
                        max = 4,
                        value = 1,
                        step = 0.01),
            sliderInput("n000",
                        label = "Occurrences of ‘000’ (% of total words)",
                        min = 0,
                        max = 4,
                        value = 1,
                        step = 0.01),
            sliderInput("make",
                        label = "Occurrences of 'make’ (% of total words)",
                        min = 0,
                        max = 4,
                        value = 1,
                        step = 0.01),
            actionButton("submitbutton", "Submit", class = "btn btn-primary")
        ),

        # main panel
        mainPanel(
          tabsetPanel(
            tabPanel('Prediction',
             tags$label('Status/Output'),
             verbatimTextOutput('contents'),
             tableOutput('tabledata') # Prediction results table
            ),
            tabPanel('Performance',
              plotOutput('perform_plot')
            )
          )
        )
    )
)

# Define server logic
server <- function(input, output, session) {
    #dataset table to show predicted possibilities that it is/is not spam
    datasetInput <- reactive({
        df <- data.frame(
          Name = c('crl.tot',
                   'dollar',
                   'bang',
                   'money',
                   'n000',
                   'make'),
          Value = as.numeric(c(input$crl.tot,
                                 input$dollar,
                                 input$bang,
                                 input$money,
                                 input$n000,
                                 input$make))
        )%>%
          pivot_wider(names_from = Name, values_from = Value)
        if(input$alg == "svm"){
          svm_predict <- predict(svmModel, newdata=df, probability = TRUE)
          svm_output <- as.data.frame(round(attr(svm_predict,"probabilities"),2))%>%
            rename(spam = as.factor(1), `not spam` = as.factor(0))
          print(svm_output)
        }
        else if(input$alg == "rf"){
          rf_predict = predict(rfModel, newdata=df)
          rf_output = as.data.frame(predict(rfModel, newdata=df, type="prob"))%>%
            rename(spam = as.factor(1), `not spam` = as.factor(0))%>%
            select(`spam`,`not spam`)
          print(rf_output)
        }
      })
    
    #Output text box
    output$contents <- renderPrint({
      if(input$submitbutton>0){
        isolate("Calculation complete")
      }else {
        return("Server is ready for calculation.")
      }
    })
    # Prediction results table
    output$tabledata <- renderTable({
      if (input$submitbutton>0) { 
        isolate(datasetInput()) 
      } 
    })
    
    #Output plot
    output$perform_plot <- renderPlot({
      ggplot(summary,aes(x=metric,y=value,fill=model))+
        geom_col(position="dodge")+
        geom_text(aes(label=round(value,3), y=value+0.05), position=position_dodge(width = 1))+
        labs(title="Performance Metrics of Models", y="", x="")+
        scale_fill_manual(values = c("indianred2","dodgerblue3"))+
        theme_minimal()+
        theme(
          axis.text.x = element_text(size = 20),
          plot.title =element_text(hjust = 0.5,size = 20),
          legend.position = "top",
          legend.text=element_text(size=15))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
