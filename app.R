########################################################
#App zur Ermittlung der monatlichen Kalt- und Warmmiete
#E-Mail
# Datum (2022-12-12)
########################################################

#Import der Bibliotheken
library(shiny)
library(shinydashboard,warn.conflicts = FALSE)
library(shinyWidgets)
library(shinycssloaders)
library(ggplot2)
library(plotly,warn.conflicts = FALSE)
library(leaflet)
library(ranger)

#Einlesen verschiedener CSV Dateien
histo<-read.csv('https://raw.githubusercontent.com/tobiarnold/shiny-app/main/Histogram.csv')
ml<-read.csv('https://raw.githubusercontent.com/tobiarnold/shiny-app/main/ml.csv')
ml2<-read.csv('https://raw.githubusercontent.com/tobiarnold/shiny-app/main/ml2.csv')
durchschnittsmieten<-read.csv('https://raw.githubusercontent.com/tobiarnold/shiny-app/main/Durchschnittsmieten.csv')
corr<-read.csv('https://raw.githubusercontent.com/tobiarnold/shiny-app/main/corr.csv')
corr<-as.matrix(corr)
geo<-read.csv('https://raw.githubusercontent.com/tobiarnold/shiny-app/main/geo_immo.csv')
mietquoten<-read.csv('https://raw.githubusercontent.com/tobiarnold/shiny-app/main/mietquoten.csv')
einnahmen<-read.csv('https://raw.githubusercontent.com/tobiarnold/shiny-app/main/einnahmen.csv')

#User Interface
ui <- dashboardPage(skin="yellow", 
  dashboardHeader(title = "Mietpreis Kalkulator"),
  dashboardSidebar(sidebarMenu(
                   menuItem("Kalkulator", tabName = "Kalkulator", icon = icon("gauge-high")),
                   menuItem("Datenbank", tabName = "Datenbank", icon = icon("table")),
                   menuItem("Grafiken",tabName = "Grafiken", icon = icon("chart-column")),
                   menuItem("Karte",tabName = "Karte", icon = icon("map")),
                   menuItem("Modell", tabName = "Modell", icon = icon("brain")),
                   menuItem("Über die App & FAQ",tabName = "FAQ", icon = icon("question"))
                  )),
  dashboardBody(
    setBackgroundImage(src = "https://raw.githubusercontent.com/tobiarnold/shiny-app/main/bg.jpg",shinydashboard = TRUE),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),),
    tags$head(
      tags$style(HTML( ".skin-yellow .main-sidebar {background-color:  #232b2b;}",
                       ".skin-yellow .sidebar-menu > li.active > a,.skin-yellow .sidebar-menu > li:hover > a {border-left-color: #ff0000;}"),),
    ),
    tabItems(
#Tab1
      tabItem(
      tabName = "Kalkulator",
      h2(div(HTML("🏡 <em>Berechnen Sie die geschätzen monatlichen Mieteinnahmen für Ihre Immobilie!</em>"))),
      sidebarPanel(
        p("👨‍💻 Bitte haben Sie beim erstmaligen Starten der App etwas Geduld!"),
        setSliderColor(c("#f89c14", "#f89c14","#f89c14"), c(1, 2, 3)),
        sliderInput("Wohnraum", "Wohnraum",min = 8, max = 500,value = 50),
        sliderInput("Anzahl_Räume", "Anzahl Räume",min = 1.0, max = 15.0,value = 3.0,step = 0.5),
        sliderInput("Baujahr", "Baujahr",min = 1500, max = 2020,value = 1960,step=1,sep = ""),
        verbatimTextOutput("parameter1"),
        selectInput("Bundesland", "Bundesland auswählen",  choices = list( "Baden-Württemberg"=12, "Bayern"=14, "Berlin"=15,
                                                              "Brandenburg"=5, "Bremen"=9, "Hamburg"=16, "Hessen"=13,
                                                              "Mecklenburg-Vorpommern"=3,"Niedersachsen"=7,
                                                              "Nordrhein-Westfalen"=8, "Rheinland-Pfalz"=11, 
                                                              "Saarland"=6, "Sachsen"=4, "Sachsen-Anhalt"=1,
                                                              "Schleswig-Holstein"=10, "Thüringen"=2)), 
        radioButtons("Großstadt", "Befindet sich die Immobilie in einer Großstadt (Einwohner >100.000)?",choices = list("Ja" = 1, "Nein" = 0),selected = 1),
        actionButton("action", "Kalkulation durchführen"),
        ),
      mainPanel(
        h4("Die prognostizierte monatliche Miete für das Wohnobjekt beträgt:"),
        p(""),
        textOutput("pred"),
        tags$head(tags$style("#pred{color: red;
                                 font-size: 20px;
                                 font-weight: bold;
                                 font-style: italic;
                                 }")),
        textOutput("pred2"),
        tags$head(tags$style("#pred2{color: blue;
                                 font-size: 20px;
                                 font-weight: bold;
                                 font-style: italic;
                                 }")),
        HTML("<br>"),
        textOutput("histbox"),
        tags$head(tags$style("#histbox{color: black;
                                 font-size: 17px;
                                 }")),
        p(""),
        splitLayout(cellWidths = c("50%", "50%"),plotly::plotlyOutput("fig_1")%>% withSpinner(color="#f89c14",type = 4,size = 1.5), 
                    plotly::plotlyOutput("fig_2")%>% withSpinner(color="#f89c14",type = 5,size = 0)),
        p(""),
        textOutput("info"),
        tags$head(tags$style("#info{color: black;
                                 font-size: 16px;
                                 background-color: lightgray;
                                display: table;
                                border-spacing: 15px 2px;
                                border-radius: 25px;
                                 }"))
      ),),
#Tab2
      tabItem(
      tabName = "Datenbank",
      h3("🗃️ Auszug aus unserer Datenbank"),
      p("Die Tabelle enthält die aggregierten Daten, gruppiert nach der Postleitzahl."),
      p("Mit den Filtern oberhalb der Spalten oder der Suchfunktion rechts kann gezielt nach spezifischen Werten gesucht werden."),
      p("Durch Klicken auf die Spaltenüberschriften lassen sich die Spalten aufsetigend oder absteigend sortieren."),
      DT::dataTableOutput("mytable"),
      ),
#Tab3
      tabItem(
      tabName = "Grafiken",
      h3("📊 Grafiken aus dem Datensatz"),
      p("Dieser Abschnitt enthält Grafiken, die Erkentnisse aus dem Datensatz anschaulich visualisieren."),
      plotly::plotlyOutput("fig_3"),
      p(""),
      plotly::plotlyOutput("fig_4"),
      p(""),
      plotly::plotlyOutput("fig_5"),
      p(""),
      plotly::plotlyOutput("fig_6"),
      p(""),
      tags$div(class = "liste",tags$ul (
        p("In Deutschland leben im Durchschnitt 53% zur Miete. Die höchste Mietquote weist Berlin auf, die niedrigste das Saarland."))),
      plotly::plotlyOutput("fig_10"),
      p(""),
      tags$div(class = "liste",tags$ul (
        p("Die Ertäge der Vermieter steigen jedes Jahr kontinuierlich."))),
      plotly::plotlyOutput("fig_11"),
      
      
      tags$div(class = "liste",tags$ul (
        p("Quellen:"),
        tags$a(href="https://www.kaggle.com/datasets/corrieaar/apartment-rental-offers-in-germany", "Apartment rental offers in Germany"),
        p(""),
        tags$a(href="https://www.destatis.de/DE/Themen/Gesellschaft-Umwelt/Wohnen/Tabellen/eigentuemerquote-nach-bundeslaender.html", 'Statistisches Bundesamt: "Eigentümerquote nach Bundesländern im Zeitvergleich"'),
        p(""),
        tags$a(href="https://www.iwd.de/artikel/immer-mehr-private-vermieter-in-deutschland-536832/", 'Institut der deutschen Wirtschaft: "Immer mehr private Vermieter in Deutschland"')
      ))
      
      ),
#Tab4
      tabItem(
        tabName = "Karte",
        h3("🗺️ Interaktive Karte"),
        p("Die Karte enthält alle PLZs aus dem Datensatz mit der dazugehörigen Anzahl an Wohnobjekten und der jeweiligen Durchschnittsmiete."),
        p(""),
        p("Zum zoomen auf gewünschte Stelle klicken, Scrollrad der Maus benutzen oder Plus/Minus Button auf der Karte"),
        p(""),
        leafletOutput("mymap",height="100vh"),
      ),
#Tab5
      tabItem(
        tabName = "Modell",
        h3("👩‍💻 Machine Learning Model"),
        p(""),
        p(class="bold","verwendeter Algorithmus: Random Forest mittels Ranger Bibliothek."),
        tags$a(href="https://cran.r-project.org/web/packages/ranger/ranger.pdf", "Leitfaden Ranger Bibliothek "),
        p("Für die Modelle werden 50.656 Wohnobjekte vom Algorithmus verarbeitet."),
        p(""),
        h4(class="background","🌡️️ Modell 1 zur Kalkulation der Miete warm:"),
        verbatimTextOutput("randomforest"),
        p(""),
        h4(class="background","❄️ Modell 2 zur Kalkulation der Miete kalt:"),
        verbatimTextOutput("randomforest2"),
        p(""),
        tags$div(tags$ul (class="liste",
          p("Folgende Features wurden zur Kalkulation der Miete genutzt:"),
          tags$li(tags$span(tags$b("livingSpaceRange:"), "Wohnfläche, aufgeteilt in Binnings 1-7 und", tags$b("livingSpace:"), "Wohnfläche in m²")),
          tags$li(tags$span(tags$b("noRoomsRange:"), "Anzahl Räume, aufgeteilt in Binnings 1-5 und", tags$b("noRooms:"), "Anzahl Räume")),
          tags$li(tags$span(tags$b("yearConstructedRange:"), "Baujahr Gesamtgebäude, aufgeteilt in Binnings 1-9 und", tags$b("yearConstructed:"), "Baujahr Gesamtgebäude")),
          tags$li(tags$span(tags$b("regio1_numeric:"), "Bundesland in dem die Wohnung liegt und", tags$b("big_city:"), "Ort mit >100.000 Einwohnern")),
          )),
        p(""),
        plotly::plotlyOutput("fig_7"),
        p(""),
        tags$div(tags$ul (class="liste",
          p("Beide Modelle weisen die höchste Wichtigkeit bei den Features Wohnraum und dem Bundesland auf."))),
        plotly::plotlyOutput("fig_8"),
        p(""),
        plotly::plotlyOutput("fig_9"),
        p(""),
      ),
#Tab6
      tabItem(
        tabName = "FAQ",
        h3("💡 Über die App"),
        tags$div(class = "liste",tags$ul(
        tags$li("Die App wurde im Rahmen der Vorlesung Anwendungsentwicklung des Studiengangs Data Science und Business Analytics an der Hochshule Aalen entwickelt. "),
        p(""),
        tags$li("Mit der App lässt sich anhand der Eingaben des Nutzers die kalt und warm Miete für ein Wohnobjekt berechnen."),
        p(""),
        tags$li("Zusätzlich lassen sich die aggregierten Daten in Form einer Tabelle, verschiedene Grafiken, eine interkative Karte sowie Informationen zu dem verwendeten Modell betrachten."),
        )),
        h3("❓ Frequently Asked Questions (FAQ)"),
        tags$div(class = "liste",tags$ul (
          tags$li(tags$span("Woher stammen die Daten?")),
          p(""),
          p(class="italic","Die Daten wurden von der Seite ImmoScout24 gescrapt und auf der Plattform Kaggle bereitgestellt."),
          p(class="italic","Der Datensatz wurde von uns aufbereitet und weitere Spalten wie z.B. Miete pro qm, Breiten- und Längengrade sowie weitere Merklmale hinzugefügt."),
          p(""),
          tags$a(href="https://www.samples-of-thoughts.com/2018/scraping-the-web-or-how-to-find-a-flat/", "Blogeintrag über das Web Scraping mit R"),
          p(""),
          tags$a(href="https://www.kaggle.com/datasets/corrieaar/apartment-rental-offers-in-germany", 'Datensatz "Apartment rental offers in Germany"'),
          p(""),
          tags$a(href="https://public.opendatasoft.com/explore/dataset/georef-germany-postleitzahl/table/", 'Datensatz "Postleitzahlen - Germany"'),
          p(""),
          tags$li(tags$span("Wer ist die Zielgruppe der App?")),
          p(""),
          p(class="italic","Die App richtet sich an (zukünftige) Vermieter aber auch an Mieter, die Ihren Mietpreis überprüfen möchten."),
          p(""),
          tags$li(tags$span("Welches Modell wird für die Kalkulation verwendet?")),
          p(""),
          p(class="italic","Für die Prognose wird der Algorithmus Random Forest der Ranger Bibliothek verwendet. Der Algorithmus wurde in der Programmiersprache C++ geschrieben und ermöglicht so eine schnelle Performance."),
          p(class="italic","Bei Random Forest handelt es sich um einen baumbasierten Algorithmus des überwachten Lernens, bei dem mehrere zufällige Entscheidungsbäume erzeugt werden und die Vorhersage nach der Mehrheitsentscheidung getroffen wird."),
          p(""),
          tags$li(tags$span("Wie valide ist die Kalkulation der Miete?")),
          p(""),
          p(class="italic","Für die Kaltmiete liegt das Bestimmtheitsmaß bei 77% und für die Warmmiete bei 79%."),
          p(class="italic","Die Prognose ist für einen ersten Überblick geeignet, kann aber aufgrund weiterer Faktoren, die den Mietpreis beinflussen nicht immer eine hundertprozentige Vohersage ermöglichen."),
          ))))))

# Server  
server <- function(input, output, session) {
#Parameter
output$parameter1<-renderText({paste( "Wohnraum:",input$Wohnraum,"qm","|Anzahl Räume:",input$Anzahl_Räume,"|Baujahr:", input$Baujahr)})
output$parameter2<-renderText({paste( "Bundesland:",input$Bundesland,"|Großstadt:",input$Großstadt)})
#Tabelle
tabelle <-readr::read_csv('https://raw.githubusercontent.com/tobiarnold/shiny-app/main/tabelle.csv',show_col_types = FALSE)
output$mytable <- DT::renderDataTable(tabelle,style="auto",filter="top",callback=JS('$(\'div.has-feedback input[type="search"]\').attr( "placeholder", "Filter" )'),
                                      options = list(language = list(sInfoThousands=".",zeroRecords="Keine Daten gefunden.",lengthMenu="Zeige _MENU_ Einträge",search = "Suche",info = 'Insgesamt _TOTAL_ Zeilen.  Angezeigt werden _START_ bis _END_ Elemente.',sInfoFiltered	="(gefiltert von _MAX_ Einträgen)",paginate =list('next'="vor", previous="zurück"))))
#Grafiken
output$fig_3 <- plotly::renderPlotly(plot_ly(data = durchschnittsmieten,x =~Bundesland, y=~Durchschnittsmiete,type = "bar", orientation = "v") %>% 
                               config(modeBarButtonsToRemove = c("pan2d", "resetScale2d","hoverClosestCartesian","hoverCompareCartesian","zoom2d","select2d","lasso2d"),displaylogo=FALSE)
                             %>%layout(xaxis = list(categoryorder = "total ascending"),title=list(text="Durchschnittsmiete (warm) je Bundesland",y = 0.97, x = 0.5)))
output$fig_4 <- plotly::renderPlotly(plot_ly(data = durchschnittsmieten,x =~Bundesland, y=~Durchschnittsmiete_pro_qm,type = "bar", orientation = "v",  marker = list(color = durchschnittsmieten$Durchschnittsmiete_pro_qm,showscale=T,colorscale="Viridis")) %>% 
                               config(modeBarButtonsToRemove = c("pan2d", "resetScale2d","hoverClosestCartesian","hoverCompareCartesian","zoom2d","select2d","lasso2d"),displaylogo=FALSE)
                             %>%layout(xaxis = list(categoryorder = "total ascending"),title=list(text="Miete pro qm (kalt) je Bundesland",y = 0.97, x = 0.5)))
output$fig_5 <- plotly::renderPlotly(plot_ly(data = durchschnittsmieten,x =~Bundesland, y=~Preistrend, type = "bar", orientation = "v",  marker = list(color = durchschnittsmieten$Preistrend,showscale=T,colorscale="Viridis")) %>% 
                               config(modeBarButtonsToRemove = c("pan2d", "resetScale2d","hoverClosestCartesian","hoverCompareCartesian","zoom2d","select2d","lasso2d"),displaylogo=FALSE)
                             %>%layout(xaxis = list(categoryorder = "total ascending"),title=list(text="Preistrend je Bundesland in %",y = 0.97, x = 0.5)))
output$fig_6 <- plotly::renderPlotly(plot_ly(data = durchschnittsmieten,x =~Bundesland, y=~Durchschnittswohnraum, type = "bar", orientation = "v",  marker = list(color = durchschnittsmieten$Durchschnittswohnraum,showscale=T,colorscale="Viridis")) %>% 
                                       config(modeBarButtonsToRemove = c("pan2d", "resetScale2d","hoverClosestCartesian","hoverCompareCartesian","zoom2d","select2d","lasso2d"),displaylogo=FALSE)
                                     %>%layout(xaxis = list(categoryorder = "total ascending"),title=list(text="Durchschnittlicher Wohnraum in qm je Bundesland",y = 0.97, x = 0.5)))
output$fig_10  <- plotly::renderPlotly(plot_ly(data=mietquoten, x = ~bundesland, y = ~jahr_1998, type = 'bar', name = '1998')
                                       %>% add_trace(y = ~jahr_2002, name = '2002')
                                       %>% add_trace(y = ~jahr_2006, name = '2006')
                                       %>% add_trace(y = ~jahr_2010, name = '2010')
                                       %>% add_trace(y = ~jahr_2014, name = '2014')
                                       %>% add_trace(y = ~jahr_2018, name = '2018')
                                       %>%config(modeBarButtonsToRemove = c("pan2d", "resetScale2d","hoverClosestCartesian","hoverCompareCartesian","zoom2d","select2d","lasso2d"),displaylogo=FALSE)
                                       %>%layout(xaxis = list(categoryorder = "total ascending",title ="Bundesländer" ),title=list(text="Mietquoten in % je Bundesland und Jahr",y = 0.97, x = 0.5), yaxis = list(title = 'Mietquote in %'))) 
output$fig_11  <- plotly::renderPlotly(plot_ly(data=einnahmen, x = ~Jahr, y = ~Einnahmen, type = 'scatter', mode = 'lines')
                                       %>%config(modeBarButtonsToRemove = c("pan2d", "resetScale2d","hoverClosestCartesian","hoverCompareCartesian","zoom2d","select2d","lasso2d"),displaylogo=FALSE)
                                       %>%layout(title=list(text="Median der jährlichen Mieteinnahmen in Deutschland abzügl. Investitionskosten",y = 0.97, x = 0.5), yaxis = list(title = 'Einnahmen in €')))
#Random Forest mit ranger
set.seed(1234)
rf <- ranger(formula = totalRent ~ ., data = ml, num.trees = 200,max.depth = 30,importance = "impurity")
output$randomforest<-renderPrint({ rf })
#Modell Grafiken
output$fig_7 <- plotly::renderPlotly(plot_ly(x=colnames(corr), y=rownames(corr), z = corr,colorscale= "RdBu",type = "heatmap")%>%
                                       config(modeBarButtonsToRemove = c("pan2d", "resetScale2d","hoverClosestCartesian","hoverCompareCartesian","zoom2d","select2d","lasso2d"),displaylogo=FALSE)
                                     %>%layout(title=list(text="Korrelationen der Spalte Gesamte Miete mit den anderen für das Machine Learning verwendeten Spalten",y = 0.99, x = 0.5)
                                     ))
fi1<-(as.data.frame(importance(rf)))
fi1<-setNames(cbind(rownames(fi1), fi1, row.names = NULL),c("features", "importance"))
output$fig_8 <- plotly::renderPlotly(plot_ly(data = fi1,x =~features, y=~importance,type = "bar", orientation = "v") %>% 
                                       config(modeBarButtonsToRemove = c("pan2d", "resetScale2d","hoverClosestCartesian","hoverCompareCartesian","zoom2d","select2d","lasso2d"),displaylogo=FALSE) 
                                      %>%layout(title=list(text="Feature Importances Modell 1",y = 0.99, x = 0.5)))

rf2 <- ranger(formula = baseRent ~ ., data = ml2, num.trees = 200,max.depth = 30,importance = "impurity")
output$randomforest2<-renderPrint({ rf2 })
fi2<-(as.data.frame(importance(rf2)))
fi2<-setNames(cbind(rownames(fi2), fi2, row.names = NULL),c("features", "importance"))
output$fig_9 <- plotly::renderPlotly(plot_ly(data = fi2,x =~features, y=~importance,type = "bar", orientation = "v") %>% 
                                       config(modeBarButtonsToRemove = c("pan2d", "resetScale2d","hoverClosestCartesian","hoverCompareCartesian","zoom2d","select2d","lasso2d"),displaylogo=FALSE)
                                      %>%layout(title=list(text="Feature Importances Modell 2",y = 0.99, x = 0.5)))

#Karte
output$mymap <- renderLeaflet({
  leaflet(options = leafletOptions(preferCanvas = TRUE))%>%
    addProviderTiles(providers$OpenStreetMap.DE,
                     options = providerTileOptions(noWrap = TRUE)) %>% setView(9, 50, zoom = 6)%>%
    addMarkers(clusterOptions = markerClusterOptions(),lat = geo$lat, lng = geo$long,popup =geo$popup) 
})
#Prediction mittels Ation Button
prediction_rf <- eventReactive (input$action,{
  df<-data.frame(
    livingSpace=input$Wohnraum,
    livingSpaceRange =(
    if (input$Wohnraum<47){
      livingSpaceRange=1
    } else if (input$Wohnraum<56){
      livingSpaceRange=2
    } else if (input$Wohnraum<63){
      livingSpaceRange=3
    } else if (input$Wohnraum<71){
      livingSpaceRange=4
    } else if (input$Wohnraum<83){
      livingSpaceRange=5
    } else if (input$Wohnraum<102){
      livingSpaceRange=6
    } else if (input$Wohnraum>=102){
      livingSpaceRange=7
    }),
    noRooms =input$Anzahl_Räume,
    noRoomsRange=(
    if (input$Anzahl_Räume<2){
      noRoomsRange=1
    } else if (input$Anzahl_Räume==2){
      noRoomsRange=2
    } else if (input$Anzahl_Räume<3){
      noRoomsRange=3
    } else if (input$Anzahl_Räume==3){
      noRoomsRange=4
    } else if (input$Anzahl_Räume>3){
      noRoomsRange=5
    }),
    yearConstructed=as.integer(input$Baujahr),
    yearConstructedRange=(
    if (input$Baujahr<1905){
      yearConstructedRange=1
    } else if (input$Baujahr<1930){
      yearConstructedRange=2
    } else if (input$Baujahr<1958){
      yearConstructedRange=3
    } else if (input$Baujahr<1968){
      yearConstructedRange=4
    } else if (input$Baujahr<1977){
      yearConstructedRange=5
    } else if (input$Baujahr<1990){
      yearConstructedRange=6
    } else if (input$Baujahr<1998){
      yearConstructedRange=7
    } else if (input$Baujahr<2017){
      yearConstructedRange=8
    } else if (input$Baujahr>=2017){
      yearConstructedRange=9
    }),
    regio1_numeric =as.integer(input$Bundesland),
    big_city =as.integer(input$Großstadt)
  )
  pred_rf <- predict(rf,df)$predictions
  return(pred_rf)
})
prediction_rf2 <- eventReactive (input$action,{
  df<-data.frame(
    livingSpace=input$Wohnraum,
    livingSpaceRange =(
      if (input$Wohnraum<47){
        livingSpaceRange=1
      } else if (input$Wohnraum<56){
        livingSpaceRange=2
      } else if (input$Wohnraum<63){
        livingSpaceRange=3
      } else if (input$Wohnraum<71){
        livingSpaceRange=4
      } else if (input$Wohnraum<83){
        livingSpaceRange=5
      } else if (input$Wohnraum<102){
        livingSpaceRange=6
      } else if (input$Wohnraum>=102){
        livingSpaceRange=7
      }),
    noRooms =input$Anzahl_Räume,
    noRoomsRange=(
      if (input$Anzahl_Räume<2){
        noRoomsRange=1
      } else if (input$Anzahl_Räume==2){
        noRoomsRange=2
      } else if (input$Anzahl_Räume<3){
        noRoomsRange=3
      } else if (input$Anzahl_Räume==3){
        noRoomsRange=4
      } else if (input$Anzahl_Räume>3){
        noRoomsRange=5
      }),
    yearConstructed=as.integer(input$Baujahr),
    yearConstructedRange=(
      if (input$Baujahr<1905){
        yearConstructedRange=1
      } else if (input$Baujahr<1930){
        yearConstructedRange=2
      } else if (input$Baujahr<1958){
        yearConstructedRange=3
      } else if (input$Baujahr<1968){
        yearConstructedRange=4
      } else if (input$Baujahr<1977){
        yearConstructedRange=5
      } else if (input$Baujahr<1990){
        yearConstructedRange=6
      } else if (input$Baujahr<1998){
        yearConstructedRange=7
      } else if (input$Baujahr<2017){
        yearConstructedRange=8
      } else if (input$Baujahr>=2017){
        yearConstructedRange=9
      }),
    regio1_numeric =as.integer(input$Bundesland),
    big_city =as.integer(input$Großstadt)
  )
  pred_rf2 <- predict(rf2,df)$predictions
  return(pred_rf2)
})
#Output Tab1
hibo <- eventReactive (input$action,{
  hibo <- "Histogramm und Boxplot der Kaltmiete des ausgewählten Bundeslandes anhand des verwendeten Datensatzes:"})

info<-eventReactive (input$action,{
  info<-"Weitere Informationen zur Vermietung und zur Funktionsweise der App finden Sie in den Reitern der linken Sidebar."})


output$pred<-renderText({paste("Ihre monatliche Warmmiete beträgt zwischen ",(ceiling(prediction_rf()*0.98)),"€ und ",(ceiling(prediction_rf()*1.02)), "€.")})
output$pred2<-renderText({paste("Ihre monatliche Kaltmiete beträgt zwischen ",(ceiling(prediction_rf2()*0.98)),"€ und ",(ceiling(prediction_rf2()*1.02)), "€.")})
output$histbox <-renderText({paste(hibo())})
output$info <-renderText({paste(info())})

histogram_boxplot <- eventReactive (input$action,{
histogram=1
histogram=as.integer(input$Bundesland)
histo<-histo[histo[, "regio1"]==histogram, ]})

output$fig_1 <- renderPlotly({ hist_out <- histogram_boxplot()
  plot_ly(data=histo, x =hist_out$baseRent, type = 'histogram',name = "Verteilung Miete")%>%
    add_trace(x = c(ceiling(prediction_rf2()*0.98),ceiling(prediction_rf2()*0.98),ceiling(prediction_rf2()*1.02),ceiling(prediction_rf2()*1.02)),y = c(0,0,0,0),
              type = 'scatter', mode = 'lines+markers',fill = 'tonexty',fillcolor = "orange",line=list(color="orange", width = 4),
              marker = list(size = 10),
              text = paste("Ihre prognostizierte Kaltmiete liegt zwischen",(ceiling(prediction_rf2()*0.98)),"€ und ",(ceiling(prediction_rf2()*1.02)), "€."),
              hoverinfo = 'text',name = "Ihre prognostizierte Miete")%>%
    config(modeBarButtonsToRemove = c("pan2d", "resetScale2d","hoverClosestCartesian","hoverCompareCartesian","zoom2d","select2d","lasso2d","toImage"),displaylogo=FALSE)%>%plotly::layout(legend = list(orientation = "h",xanchor = "left", x = 0.0,borderwidth=1,itemclick=FALSE,itemdoubleclick=FALSE, groupclick = FALSE),title = list(font=list(size = 14), text=sprintf("Histogramm der Miete (kalt) <br> und Ihre prognostizierte Miete"),y = 0.95, x = 0.5),xaxis = list(),yaxis=list(title ="Anzahl"))})
    
output$fig_2 <- renderPlotly({ hist_out <- histogram_boxplot()
plot_ly(data=histo, x =hist_out$baseRent, type = 'box',hoverinfo = 'x')%>%
  layout(hovermode = "x",showlegend = FALSE)%>% 
  layout(yaxis = list(range = c(-1,2))) %>%
  add_trace(x = ~c(ceiling(prediction_rf2())), y=c(0), type='scatter', mode='markers',marker=list(color="orange", size = 15),
          text = paste("Ihre prognostizierte Kaltmiete liegt zwischen",(ceiling(prediction_rf2()*0.98)),"€ und ",(ceiling(prediction_rf2()*1.02)), "€."),
            hoverinfo = "text",name = "Ihre prognostizierte Miete")%>%
config(modeBarButtonsToRemove = c("pan2d", "resetScale2d","hoverClosestCartesian","hoverCompareCartesian","zoom2d","select2d","lasso2d","toImage"),displaylogo=FALSE)%>%plotly::layout(title = list(font=list(size = 14),text=sprintf("Boxplot der Miete (kalt) <br> und Ihre prognostizierte Miete"),y = 0.95, x = 0.5),xaxis = list(title ="Mietpreis (kalt) in €",zeroline = FALSE),yaxis=list(showticklabels = FALSE))})
}
#Aufruf der App
shinyApp(ui, server)
