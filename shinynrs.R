
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
  navbarPage("News Recommendation System",
  tabPanel("Data",
  sidebarLayout
  (
    sidebarPanel
    (
      conditionalPanel
      (
        'input.dataset === "UserStudy"',
        checkboxGroupInput('show_vars', 'Columns in Yow data to show:',
                           names(UserStudy), selected = names(UserStudy))
      ),
      conditionalPanel
      (
        'input.dataset === "PreProcess"',
        helpText('Dataset after pre-processing i.e. normalising the values,
                 replacing missing values with the mean etc.')
        ),
      conditionalPanel
      (
        'input.dataset === "Euclidean"',
        helpText('The Euclidean distance between two user profiles.')
      )
      ),
    mainPanel
    (
      tabsetPanel
      (
        id = 'dataset',
        tabPanel('UserStudy', DT::dataTableOutput('mytable1')),
        tabPanel('PreProcess', DT::dataTableOutput('mytable2')),
        tabPanel('Euclidean', DT::dataTableOutput('mytable3'))
      )
    )
  )
  ),
  tabPanel("Neighbors",
           sidebarLayout(
             sidebarPanel(
               helpText('The Plot shows neighbors of the active user 67.')
               ),
          img(class="img-polaroid",
              src=paste0("https://s21.postimg.org/",
                 "rg1sfo36f/",
                 "neighborhood67.png"))
              )
        ),
  tabPanel("Recommendation & Evaluation",
           
               tabsetPanel
               (
                 id = 'rec',
                 tabPanel('Recommend', DT::dataTableOutput('mytable4')),
                 tabPanel('Evaluation', DT::dataTableOutput('mytable5'))
               )
             
          ),
  tabPanel("F Measure",
           sidebarLayout(
             sidebarPanel(
               helpText('The Plot shows the change in accuracy of recommendation as weights are changed')
             ),
             img(class="img-polaroid",
                 src=paste0("https://s25.postimg.org/",
                            "junhwnchr/", 
                            "fmeasure_graph.png"))
           )
          ),
  tabPanel("Twitter Trends",
           
           tabsetPanel
           (
             id = 'tweet',
             tabPanel('Twitter', DT::dataTableOutput('mytable6'))
           )
           
          ),
    tabPanel("Final Recommendations",
             tabsetPanel(
               id='news',
               tabPanel('Top News',DT::dataTableOutput('mytable7'))
             )
            )  
  )
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
   
  # choose columns to display
  UserStudy<-read.csv("E:/yow_userstudy_raw.csv")
  
  output$mytable1 <- DT::renderDataTable({
    DT::datatable(UserStudy[, input$show_vars, drop = FALSE])
  })
  
  # sorted columns are colored now because CSS are attached to them
  PreProcess<-read.csv("E:/SEM8/Project/new_data_news_recommend.csv")
  output$mytable2 <- DT::renderDataTable({
    DT::datatable(PreProcess, options = list(orderClasses = TRUE))
  })
  
  # customize the length drop-down menu; display 5 rows per page by default
  Euclidean<-read.csv("E:/SEM8/Project/e_distance_data.csv")
  output$mytable3 <- DT::renderDataTable({
    DT::datatable(Euclidean, options = list(orderClasses=TRUE))
  })
  Recommend<-read.csv("E:/SEM8/Project/recommendations.csv")
  output$mytable4 <- DT::renderDataTable({
    DT::datatable(Recommend, options = list(orderClasses=TRUE))
  })
  Evaluation<-read.csv("E:/SEM8/Project/rec_score.csv")
  output$mytable5 <- DT::renderDataTable({
    DT::datatable(Evaluation, options = list(orderClasses=TRUE))
  })
  Tweet<-read.csv("E:/SEM8/Project/twitter/cleaned_twitter_trends_25417.csv")
  output$mytable6 <- DT::renderDataTable({
    DT::datatable(Tweet, options = list(orderClasses=TRUE))
  })
  news<-read.csv("E:/SEM8/Project/final11.csv")
  output$mytable7 <- DT::renderDataTable({
    DT::datatable(news, options = list(orderClasses=TRUE))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

