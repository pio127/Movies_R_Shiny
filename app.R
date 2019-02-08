library(shiny.semantic)
library(semantic.dashboard)
library(DT)
library(knitr)
#library(rsconnect)

# Preprocessing danych
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

# Dokumentacja
documentation <- c("dokumentacja.Rmd")
sapply(documentation, knit, quiet = T)

# UI
sidebar <- dashboardSidebar(sidebarMenuOutput("menu"),
                            size = "thin",
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
    
    
    # Informacje o bazie
      tabItem(tabName = "info",
        fluidRow( 
          box(title = "Quick info",
              title_side  = "top left", 
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
                 box(title = "Years",
                   value_box("From year", min(movies$title_year), 
                             icon = icon("arrow left"), 
                             color = "teal",
                             size = "tiny"),
                    value_box("To year", max(movies$title_year), 
                              icon = icon("arrow right"), 
                              color = "teal",
                              size = "tiny")),
              box(title = "Rankings",
                  value_box("Best film overall", 
                            movies$movie_title[(which.max(movies$imdb_score))], 
                            icon = icon("thumbs up"), 
                            color = "teal",
                            size = "tiny"),
                value_box("Worst film overall", 
                          movies$movie_title[(which.min(movies$imdb_score))], 
                          icon = icon("thumbs down"), 
                          color = "teal",
                          size = "tiny"),
                value_box("Highest gross", 
                          movies$movie_title[(which.max(movies$gross))], 
                          icon = icon("dollar sign icon"), 
                          color = "olive",
                          size = "tiny")
                )
          )
        )
      ),
      
      
      # Baza danych w formie tabeli
      tabItem(tabName = "database", 
              fluidRow(DT::dataTableOutput("moviesTable"))),
      
      
      # Histogram
      tabItem(tabName = "hist",
              box(width = 6, title_side  = "top left",
                  title = "Parameters",
                  selectInput(inputId = "choice_input",
                              label = "Column name:",
                              choices = numericData,
                              selected = numericData[[2]]
                              ),
                  sliderInput(inputId = "bins",
                              label = "Number of bins:",
                              min = 1,
                              max = 50,
                              value = 30
                              )
                  ),
                  box(title = "Histogram",
                      width = 10,
                      plotOutput("hist1")
                      )   
          ),
      
      
      # Clustering
      tabItem(tabName = "clustering",
              h2("k-means"),
              fluidRow(
                column(width = 6,
                       box(width = 6,
                           title_side  = "top left",
                           title = "Parameters", 
                           solidHeader = TRUE,
                           status = "primary",
                           selectInput("xcol",
                                       "X",
                                      numericData,
                                      selected = numericData[[5]]),
                          selectInput("ycol", 
                                      "Y", 
                                      numericData,
                                      selected = numericData[[6]]),
                          sliderInput("clusters", "Number of clusters: ", 
                                      min = 1, max = 8, value = 3)
                  ),
                box(width = 6,
                    title_side  = "top left",
                    title = "Center positions", 
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
      
      
      # Regresja
      tabItem(tabName = "regression",
            fluidRow(
              column(
                width = 7,
                box(title ="Plot",
                    title_side  = "top left",
                    plotOutput("scatterplot")
                ),
                box(title = "Parameters",
                    title_side  = "top left",
                    selectInput("xcol2", 
                    label = h3("X"),
                    choices = numericData, 
                    selected = numericData[[2]]
                    ),  
                    selectInput("ycol2", 
                                label = h3("Y"),
                                choices = numericData, 
                                selected = numericData[[5]]
                               )
                    )
              ),
             column(width = 8,
                    box(title = "Summary",
                        verbatimTextOutput("regression_summary")
                )
             )
          )
        ),
      
      
    # Dokumentacja
    tabItem(tabName = "doc", htmlOutput("inc"))
  )
)

ui <- dashboardPage(
      theme = "spacelab",
      header = dashboardHeader(inverted = TRUE),
      sidebar = sidebar,
      body = body
)




server <- shinyServer(function(input, output) {
  
  # Dokumentacja  
  getPage<-function() {
    return(includeHTML("dokumentacja.html"))
  }
  output$inc<-renderUI({getPage()})
  
  # Histogram
  selectedData2 <- reactive({
    movies[, input$choice_input]
  })
  
  output$hist1 <- renderPlot( {
  bins <- seq(min(selectedData2()), 
              max(selectedData2()), 
              length.out = input$bins + 1)
  
  hist(selectedData2(),
       xlab = input$choice_input,
       xlim = c(floor(min(selectedData2())), ceiling(max(selectedData2()))),
       breaks = bins, 
       main = "Number of films",
       include.lowest = TRUE)
  })
  
  # Clustering
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
                            format(round(clusters()$centers[i, 1], 2),
                                   nsmall=2),"; ",
                            format(round(clusters()$centers[i, 2], 2), 
                                   nsmall=2),")\n", sep = "")     
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
  
  # Datatable
  output$moviesTable <- DT::renderDataTable(
    DT::datatable(
      rownames = FALSE,
      colnames = c("Title", "Director", "Budget", "Gross", 
                   "Country", "Year", "IMDB Rating", 
                   "Number of Votes", "Color/BW"),
      
      filter = 'top',
      options = list(
        pageLength = 12, autoWidth = TRUE
      ),
      data <- movies
    )
  )
  
  # Regresja 
  output$regression_summary <- renderPrint({
    fit <- lm(movies[,input$ycol2] ~ movies[,input$xcol2])
    names(fit$coefficients) <- c("Intercept", input$var2)
    summary(fit)
  })
  
  selectedData3 <- reactive({
    movies[, c(input$xcol2, input$ycol2)]
  })
  output$scatterplot <- renderPlot({
    plot(selectedData3(),
         main="Regression",
         xlab=input$xcol2, 
         ylab=input$ycol2, 
         pch=19)
    abline(lm(movies[,input$ycol2] ~ movies[,input$xcol2]), col="red")
    lines(lowess(movies[,input$xcol2], movies[,input$ycol2]), col="blue")
    
  })

})

shinyApp(ui = ui, server = server)