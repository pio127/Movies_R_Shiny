library(shiny.semantic)
library(semantic.dashboard)
library(DT)

movies <- read.csv("movie_metadata.csv")[, c('movie_title',
                                             'director_name',
                                             "budget",
                                             "gross",
                                             "country",
                                             "title_year",
                                             "imdb_score",
                                             "num_voted_users",
                                             "color"
                                             )]
movies <- na.omit(movies)
forClustering = c("budget", 
                  "gross", 
                  "title_year", 
                  "budget", 
                  "imdb_score",
                  "num_voted_users")

sidebar <- dashboardSidebar(sidebarMenuOutput("menu"),
                            menuItem(text = "Info", 
                                     tabName = "info",
                                     icon = icon("info circle")
                                    ),
                            menuItem(text = "Film database", 
                                     tabName = "database",
                                     icon = icon("bars")
                                    ),
                            menuItem(text = "Histogram", 
                                     tabName = "hist",
                                     icon = icon("chart bar")
                            ),
                            menuItem(text = "Clustering", 
                                     tabName = "clustering",
                                     icon = icon("asterisk")
                                    ),
                            menuItem(text = "Regression", 
                                     tabName = "regression",
                                     icon = icon("chart line")
                                    )
                            )

body <- dashboardBody(

  tabItems(
      tabItem(tabName = "info",
        fluidRow(
       
          box(title = "About database",
            # A static infoBox
            value_box("Movies", 
                      nrow(movies), 
                      icon = icon("film"), 
                      color = "blue", 
                      size = "huge"),
            value_box("In Color", 
                      length(which(movies$color == "Color")), 
                      icon = icon("sitemap"), 
                      color = "teal", 
                      width = 5,
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
          box(title = "Top"
              

          )
          )
        ),
    tabItem(tabName = "database",
            fluidRow(DT::dataTableOutput("moviesTable"))
    ),
    tabItem(tabName = "clustering",
      h2("k-means"),
      fluidRow(
        column(width = 6,
        box(width = 6,
          title = "Parameters", solidHeader = TRUE,status = "primary",
          selectInput('xcol', '
                      X', 
                      forClustering,
                      selected = forClustering[[5]]),
          selectInput('ycol', 
                      'Y', 
                      forClustering,
                      selected = forClustering[[6]]),
          sliderInput("clusters", "Number of clusters: ", 
                      min = 1, max = 8, value = 3)
          # numericInput('clusters', 'Cluster count', 3,
          #              min = 1, max = 12)
        ),
        box(width = 6,
            title = "Info", solidHeader = TRUE,status = "primary",
            print("jest"))
        ),
        box(width = 10,
          title = "Clusters", solidHeader = TRUE, status = "primary",
          plotOutput('plot1'))
      )
    )
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
  
  selectedData <- reactive({
    movies[, c(input$xcol, input$ycol)]
  })
  
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
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
      data = {
        stateFilter2 <- subset(movies, movies$num_voted_users > 15000)
      }
    )
  )
}
)

shinyApp(ui = ui, server = server)