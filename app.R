library(shiny.semantic)
library(semantic.dashboard)
library(DT)
library(knitr)
library(Rfast)
#library(rsconnect)

movies <- read.csv("movie_metadata.csv")[, c('movie_title',
                                             'director_name',
                                             "budget",
                                             "gross",
                                             "country",
                                             "title_year",
                                             "imdb_score",
                                             "num_voted_users",
                                             "color" )
                                         ]
movies <- na.omit(movies)

numericData = c("budget", 
                  "gross", 
                  "title_year", 
                  "budget", 
                  "imdb_score",
                  "num_voted_users")

documentation <- c("dokumentacja.Rmd")
sapply(documentation, knit, quiet = T)

sidebar <- dashboardSidebar(sidebarMenuOutput("menu"),
                            size = "", 
                            side = "left", 
                            inverted = TRUE, center = TRUE,
                            menuItem(text = "Info", 
                                     tabName = "info",
                                     icon = icon("info circle")),
                            menuItem(text = "Film database", 
                                     tabName = "database",
                                     icon = icon("bars")),
                            menuItem(text = "Histogram", 
                                     tabName = "hist",
                                     icon = icon("chart bar")),
                            menuItem(text = "Clustering", 
                                     tabName = "clustering",
                                     icon = icon("asterisk")),
                            menuItem(text = "Regression", 
                                     tabName = "regression",
                                     icon = icon("chart line")),
                            menuItem(text = "Documentation", 
                                     tabName = "doc",
                                     icon = icon("book"))
                            )

body <- dashboardBody(

  tabItems(
      tabItem(tabName = "info",
        fluidRow( 
          box(title = "Summary",
            value_box("Films", 
                      nrow(movies), 
                      icon = icon("film"), 
                      color = "blue", 
                      size = "huge"),
            value_box("In Color", 
                      length(which(movies$color == "Color")), 
                      icon = icon("sitemap"), 
                      color = "teal", 
                      size = "huge"),
            value_box("Directors", length(unique(movies$director_name)), 
                      icon = icon("user"), 
                      color = "teal",
                      size = "huge"),
            value_box("Countries", length(unique(movies$country)), 
                      icon = icon("flag icon"), 
                      color = "teal",
                      size = "huge")

            ),
          column(width = 7,
            box(title = "Info"
              ),
            box(title = "Rankings",
                value_box("Best film overall", movies$movie_title[(which.max(movies$imdb_score))], 
                          icon = icon("thumbs up"), 
                          color = "teal",
                          size = "tiny"),
                
                value_box("Worst film overall", movies$movie_title[(which.min(movies$imdb_score))], 
                          icon = icon("thumbs down"), 
                          color = "teal",
                          size = "tiny"),
                value_box("Highest gross", movies$movie_title[(which.max(movies$gross))], 
                          icon = icon("dollar sign icon"), 
                          color = "olive",
                          size = "tiny")
            )
            )
        )
      ),
      tabItem(tabName = "database", 
              fluidRow(DT::dataTableOutput("moviesTable"))),
      tabItem(tabName = "hist",
              box(width = 16, title_side  = "top",
                  title = "Parameters",
                  selectInput(inputId = "choice_input",
                              label = "Column name:",
                              choices = numericData,
                              selected = numericData[[2]]
                              )
                  ),
                  box(title = "Histogram",
                      width = 16,
                      plotOutput("hist1")
                      )   
          ),
    tabItem(tabName = "clustering",
            h2("k-means"),
            fluidRow(
              column(width = 6,
                     box(width = 6,
                         title_side  = "top left",
                          title = "Parameters", 
                          solidHeader = TRUE,
                          status = "primary",
                          selectInput('xcol', '
                                      X', 
                                      numericData,
                                      selected = numericData[[5]]),
                          selectInput('ycol', 
                                      'Y', 
                                      numericData,
                                      selected = numericData[[6]]),
                          sliderInput("clusters", "Number of clusters: ", 
                                      min = 1, max = 8, value = 3)
                  ),
                box(width = 6,
                    title_side  = "top left",
                    title = "Centers positions", 
                    solidHeader = TRUE,
                    status = "primary",
                    verbatimTextOutput("clusters_info")
                    )),
                box(width = 10,
                    title = "Plot with clusters", 
                    solidHeader = TRUE, 
                    status = "primary",
                    plotOutput("plot1")
                    )
                )
            ),
    # Regresja UI
    tabItem(tabName = "regression",selectInput("outcome", label = h3("Outcome"),
                                               choices =numericData, selected = 1),
            
            selectInput("indepvar", label = h3("Explanatory variable"),
                        choices = numericData, selected = 1),
            
            plotOutput("scatterplot")
            ),
    tabItem(tabName = "doc", htmlOutput("inc"))
    )
)

ui <- dashboardPage(
      theme = "spacelab",
      header = dashboardHeader(title = "Filmy 1916 - 2016", 
                               inverted = TRUE),
      sidebar = sidebar,
      body = body
)

server <- shinyServer(function(input, output) {
  
  getPage<-function() {
    return(includeHTML("dokumentacja.html"))
  }
  output$inc<-renderUI({getPage()})
  
  selectedData2 <- reactive({
    movies[, input$choice_input]
  })
  
  output$hist1 <- reactivePlot(function() {
    
    hist(selectedData2(),
           xlab = input$choice_input,
           main = "Number of films",include.lowest = TRUE)
  })
  
  selectedData <- reactive({
    movies[, c(input$xcol, input$ycol)]
  })
  
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })
  

  output$clusters_info <- renderText({ 
    
    all <- ""
    for (i in 1:input$clusters) 
      all = paste(all, "Center ", i," = (",  
                            format(round(clusters()$centers[i, 1], 2), nsmall=2),"; ",
                            format(round(clusters()$centers[i, 2], 2), nsmall=2),")\n")     
    paste(all, sep = "\n")
    })
    
  output$plot1 <- renderPlot({
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF"))
    
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, 
         cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })
  output$moviesTable <-DT::renderDataTable(
    DT::datatable(
      rownames = FALSE,
      colnames = c("Title", "Director", "Budget", "Gross", 
                   "Country", "Year", "IMDB Rating", "Number of Votes", "Color/BW"),
      
      filter = 'top',
      options = list(
        pageLength = 12, autoWidth = TRUE
      ),
      data <- {
        stateFilter2 <- subset(movies, movies$num_voted_users > 15000)
      }
    )
  )
  # Regresja 
  output$scatterplot <- renderPlot({
    plot(swiss[,input$indepvar], 
         swiss[,input$outcome], 
         main="Scatterplot",
         xlab=input$indepvar, 
         ylab=input$outcome, 
         pch=19)
    abline(lm(swiss[,input$outcome] ~ swiss[,input$indepvar]), col="red")
    lines(lowess(swiss[,input$indepvar],swiss[,input$outcome]), col="blue")
  }, height=400)
}
)

shinyApp(ui = ui, server = server)