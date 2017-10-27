library(shiny)
library(leaflet)
library(RColorBrewer)

# zie https://rstudio.github.io/leaflet/shiny.html


adreslocaties <- read.csv("data/huisartsenlocaties.csv")
# we sorteren hier Descending, zodat de kleinste bollen bovenaan liggen
adreslocaties <-adreslocaties[order(-adreslocaties$Aantal.artsen),]

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                sliderInput("rangeArtsen", "Aantal artsen in de praktijk", min(adreslocaties$Aantal.artsen), max(adreslocaties$Aantal.artsen),
                            value = range(adreslocaties$Aantal.artsen), step = 1
                ),
                selectInput("intego", "Intego deelnemer?", choices=c(0, 1) ),
                selectInput("colors", "Kleurenschema",
                            rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                ),
                checkboxInput("legend", "Show legend", TRUE)
  )
)

server <- function(input, output, session) {
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    adreslocaties[adreslocaties$Aantal.artsen >= input$rangeArtsen[1] & adreslocaties$Aantal.artsen <= input$rangeArtsen[2] & adreslocaties$INTEGO == input$intego,]
  })
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  colorpal <- reactive({
    colorNumeric(input$colors, adreslocaties$Aantal.artsen)
  })
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet() %>% 
      #addTiles() %>% 
      addProviderTiles(providers$Stamen.TonerLite ) %>%
      setView(lng = 4.4695, lat = 51.21611, zoom = 12)
  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    pal <- colorpal()
      leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
        addCircles(radius = ~Aantal.artsen * 50, weight = 1, color = "#777777", fillColor = ~pal(Aantal.artsen), 
                   fillOpacity = 0.7, popup = ~paste(Straat,Nummer,"<br/>",Postcode,Plaatsnaam,"<br/>",Aantal.artsen,"arts(en) in deze praktijk")
     
    )
  })
  
  # Use a separate observer to recreate the legend as needed.
  observe({
    #proxy <- leafletProxy("map", data = quakes)
    proxy <- leafletProxy("map", data = adreslocaties)
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend) {
      pal <- colorpal()
      proxy %>% addLegend(position = "bottomright",
                          pal = pal, values = ~Aantal.artsen 
      )
    }
  })
}

shinyApp(ui, server)