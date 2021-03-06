---
title: "Dokumentacja projektu R Filmy"
author: "Piotr N"
date: "19 stycznia 2019"
output:
  html_fragment
---



# Dokumentacja projektu
&nbsp;

# 1. Za�o�enia

Aplikacja webowa napisana w j�zyku R, wykorzystuj�ca pakiet Shiny i ShinyDashboard ze wsparciem Semantic UI.
Pozwala na przegl�danie bazy danych film�w(pobranej z tej strony: https://www.kaggle.com/carolzhangdc/imdb-5000-movie-dataset), rysowanie histogram�w oraz wykonywanie grupowania(algortym k-�rednich), regresji liniowej na podstawie dw�ch dowolnych danych numerycznych.
&nbsp;

# 2. Wykorzystane pakiety
* [shiny.semantic](https://cran.r-project.org/web/packages/shiny.semantic)
* [semantic.dashboard](https://cran.r-project.org/web/packages/semantic.dashboard)
* [DT](https://cran.r-project.org/web/packages/DT/)
* [knitr](https://cran.r-project.org/web/packages/knitr)

# 3. Przygotowanie danych(Preprocessing)

**Wst�pne wczytanie u�ywanych w projekcie danych, odrzucenie tych pustych lub niepe�nych oraz wyznaczenie nadaj�cych si� do grupowania(clusteringu):**

---

```r
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
```
---

**Wybranie tych nazw kolumn, kt�re zawieraj� warto�ci numeryczne i mo�na wykorzysta� je przy wykresach.**

---

```r
numericData = c("budget", 
                  "gross", 
                  "title_year", 
                  "budget", 
                  "imdb_score",
                  "num_voted_users")
```
---

# 4. Interfejs aplikacji

**Cia�o i pasek bo boczny strony zosta�y zdefiniowane w odzielnych zmiennych i przes�ane poni�ej, do wygenerowania interfejsu u�ytkownika. Wybrany motyw pochodzi z tej strony: http://semantic-ui-forest.com/themes/ .**

---

```r
ui <- dashboardPage(
      theme = "spacelab",
      header = dashboardHeader(inverted = TRUE),
      sidebar = sidebar,
      body = body
)
```
---

**Cz�� opisuj�ca pasek boczny sk�ada si� z przycisk�w menu z dobranymi odpowiednio ikonami semantic UI(https://semantic-ui.com/elements/icon.html). **

---

```r
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
```
---

**Wyst�puje podzia� na odzielne zak�adki, kt�re s� prze��czane za pomoc� przycisk�w na panelu bocznym. Najcz�sniej wykorzystywana jest funkcja box, do opakowywania element�w w rozwijalne okienka z etykietami. Dla przejrzysto�ci dokumentacji, zosta�a poni�ej pomini�ta cz�� implementacji poszczeg�lnych element�w interfejsu zak�adek.**

---

```r
body <- dashboardBody(
  tabItems(
    # Informacje o bazie
    tabItem(tabName = "info", ...),
    # Baza danych w formie tabeli
    tabItem(tabName = "database", ...),
    # Histogram
    tabItem(tabName = "hist", ...),     
    # Clustering
    tabItem(tabName = "clustering", ...),
    # Regresja
    tabItem(tabName = "regression", ...),
    # Dokumentacja
    tabItem(tabName = "doc", ...)
  )
)
```
---

# 5. Cz�� po stronie serwera

**Generacja tablicy danych z wkorzystaniem pakietu DT. Podmiana nazw column na bardziej czytelne.**

---

```r
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
```
---

**Rysowanie histogramu na podstawie wybranej kolumny i liczbie odst�p�w prostok�t�w w UI przez u�ytkownika. **

---

```r
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
```
---

**Clustering danych dla dw�ch wybranych przez u�ytkownika kolumn o warto�ciach numerycznych. Powsta� na podstawie przyk�ad�w w galerii na stronie shinyapps. ** 

---

```r
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
```
---

**Regresja liniowa na wykresie danych opartych o dwie dowolne kolumny z warto�ciami numerycznymi(kolumny wybrane przez u�ytkownika w UI). Pokazane s� informacje o prostej najlepiej dopasowuj�cej si� do danych. Dla por�wnania jest przedstawiona lokalnie wa�ona regresja liniowa(wykorzystanie lowess()) w postaci wynikowej wyg�adzonej krzywej ze wszystkich sk�adowych. **

---

```r
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
```
---
