library(shinydashboard)
library(dplyr)
library(leaflet)
library(stringr)

ui <- dashboardPage(
  dashboardHeader(title = "Amsterdam door de ogen van computer vision", titleWidth = 600),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Inleiding", tabName = "introduction", icon = icon("dashboard")),
      menuItem("Beelden classificaties", tabName = "imagestab", icon = icon("th")),
      selectInput("imagelabel", "image label", clf_classes2$entities)
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "introduction",
        h4("Introduction"),
        list(
            p("De gemeente Amsterdam heeft panoramabeelden van Amsterdam beschikbaar gesteld."),
            a(href="https://api.data.amsterdam.nl/panorama/", "zie hier"),
            
            p("Een camera auto heeft door Amsterdam gereden en foto's genomen. Ik heb per buurt in Amsterdam 400 random images gepakt
              en deze door het clarifai image model heen gehaald. Per image krijg je dan een aantal labels"),
            p(" "),
            p("Wordcloud image labels van Amsterdam"),
            img(SRC="amsterdamlabels.png", height = 300),
            p(" "),
            p("Deze shiny app laat het percentage per buurt zien van een gekozen label"),
            p("Cheers, Longhow")
                )
              
              ),
      tabItem(tabName = "imagestab",
              h5("percentage per buurt van gekozen label"),
              fluidRow(
                leafletOutput('images', width  = "1400px", height = "650px" )
              )
      )
      
  )
    )
)


###############################################################



server <- function(input, output) {

  output$images = renderLeaflet({
    
    #### create the shapes
    Amsterdam_image_perc = Amsterdam_Clarifai_classes %>% 
      mutate(
        imlabel = entities == input$imagelabel 
      ) %>% 
      group_by(BU_NAAM) %>% 
      summarise(n=n(), imlabel = mean(imlabel))
   
    AmsterdamWijken3 = AmsterdamWijken
    
    pp = left_join(AmsterdamWijken3@data, Amsterdam_image_perc, by = c("BU_NAAM" = "BU_NAAM"))
    AmsterdamWijken3@data = pp
    
    polpopup = paste0(
      "<b>",
      AmsterdamWijken3$BU_NAAM, ": ", 
      input$imagelabel, " percentage ", round(100*AmsterdamWijken3$imlabel,1), "%", 
      "</b>",
      "<br/>",
      "<img border='0' src='http://www.lhldsd.nl/amsterdam/",
      fn,
      "' width='325' height='200'>"
    )
    
    #### create set for markers
    locs = Amsterdam_Clarifai_classes2 %>% 
      filter(
        entities == input$imagelabel 
      )
    polpopup2 = paste0(
      "<img border='0' src='",
      locs$link,
      "' width='325' height='200'>"
    )
    ### maak leaflet
    m = leaflet()
    m = m %>%
      setView(lng = 4.899431, lat = 52.379189, zoom = 12) %>%
      addTiles() %>%
      addPolygons(
        weight=2,
        smoothFactor = 0.2, fillOpacity = 0.55, 
        color = ~colorQuantile(n = 7, "Greens", AmsterdamWijken3$imlabel)(imlabel),
        data = AmsterdamWijken3,
        popup = polpopup )  %>% 
      addCircleMarkers(
        lng=~x, lat = ~y, radius = 0.5,
        opacity = .21,
        popup = polpopup2,
        data = locs
      )
    m
    
  })
}

shinyApp(ui, server)