library(shiny)
library(leaflet)
library(RColorBrewer)


#dataprep


normalise <- function(x){(x-min(x,na.rm=T))/(max(x,na.rm=T)-min(x,na.rm=T))}


# rsconnect::deployApp('C:/Users/kdh10kg/Documents/github/darkspots_shiny/darkspots_shiny')
load("app_data.RData")
# simplifiedBoundaries <- tdawg3#boundaries # rmapshaper::ms_simplify(boundaries)


# Define UI for app that draws a histogram ----
ui <- fluidPage(
  tags$style(type="text/css", "div.info.legend.leaflet-control br {clear: both;}"),


  # App title ----
  fluidRow(
    column(2,
           ""
    ),
    column(10,
           ""
    )
  ),
  titlePanel("Variables"),
  sidebarLayout(

    sidebarPanel(

      selectInput(inputId = "Species",
                  label = "Number of species left to observe (time-to-event model)",
                  c("Ignore" = "Ignore",
                    "Areas with the most species (wcvp-gbif output)" = "Avoid",
                    "Areas with the most species (time-to-event model)" = "Target"),
                 selected = "Target"),

      selectInput(inputId = "Discoveries",
                  label = "Discoveries by year (Skyline)",
                  c("Ignore" = "Ignore",
                    "2010-2019" = "y2010",
                    "1980-1989" = "y1980",
                    "1950-1959" = "y1950",
                    "difference betwen 2010s and 1970s" = "y30_diff",
                    "peak discovery year" = "max_year")),

      selectInput(inputId = "Descriptions",
                  label = "Descriptions by year (Skyline)",
                  c("Ignore" = "Ignore",
                    "2010-2019" = "y2010",
                    "1980-1989" = "y1980",
                    "1950-1959" = "y1950",
                    "difference betwen 2010s and 1970s" = "y30_diff",
                    "peak description year" = "max_year")),

      selectInput(inputId = "Travel",
                  label = "Proportion of area >6h travel from town",
                  c("Ignore" = "Ignore",
                    "Collect in areas that can be accessed easily" = "Avoid",
                    "Collect in remote areas" = "Target")),


      selectInput(inputId = "Research",
                  label = "Research capacity",
                  c("Ignore" = "Ignore",
                    "Areas with less research capacity" = "Avoid",
                    "Areas with more research capacity" = "Target")),


      selectInput(inputId = "Movement",
                  label = "Freedom of movement",
                  c("Ignore" = "Ignore",
                    "Areas that are accessible only to men" = "Avoid",
                    "Areas that are accessible to everyone" = "Target")),


      selectInput(inputId = "Poverty",
                  label = "Wealth (per capita gdp)",
                  c("Ignore"= "Ignore",
                    "Encourage countries with wealth to descibe their species" = "Avoid",
                    "Invest in poorer areas"  = "Target")),

      selectInput(inputId = "War",
                  label = "Conflicts over last 20 years",
                  c("Ignore"= "Ignore",
                    "Focus on collecting species in peaceful regions" = "Avoid",
                    "Focus on collecting species in conflict regions" = "Target"))
    ),

    mainPanel(
      leafletOutput("map", height = 800)#, width="50%",height="80%")#,
      # tags$head(tags$style("#myplot{height:100vh !important;}"))

    )
  )
)



# Define server logic required to draw a histogram ----
server <- function(input, output, session) {

  filteredData <- reactive({
    table = tdwg3@data
    table["toplot"] = 0
    count=0


    # Species left to discover
    if(input$Species == "Avoid"){
       count=count+1
       var = normalise(table$`left_spp_wcvp`)#table$`left_spp_wcvp`#
       table["toplot"] =  table["toplot"]+ (var)
    }
    if(input$Species == "Target"){
      count=count+1
      var = table$`SR_shortfalls`#normalise(table$`SR_shortfalls`)#
      table["toplot"] =  table["toplot"]+ (var)
    }

    # Discoveries
    if(input$Discoveries == "y2010"){
      count=count+1
      var = table$`discoveries_y2010`#normalise(table$`discoveries_y2010`)#normalise(table$`discoveries`)
      table["toplot"] =  table["toplot"]+ (var)
    }
    if(input$Discoveries == "y1980"){
      count=count+1
      var = table$`discoveries_y1980`#normalise(table$`discoveries_y1980`)#normalise(table$`discoveries`)
      table["toplot"] =  table["toplot"]+ (var)
    }
    if(input$Discoveries == "y1950"){
      count=count+1
      var = table$`discoveries_y1950`#normalise(table$`discoveries_y1950`)#normalise(table$`discoveries`)
      table["toplot"] =  table["toplot"]+ (var)
    }
    if(input$Discoveries == "y30_diff"){
      count=count+1
      var = table$`discoveries_y30_diff`#normalise(table$`discoveries_max_year`)#normalise(table$`discoveries`)
      table["toplot"] =  table["toplot"]+ (var)
    }

    if(input$Discoveries == "max_year"){
      count=count+1
      var = table$`discoveries_max_year`#normalise(table$`discoveries_max_year`)#normalise(table$`discoveries`)
      table["toplot"] =  table["toplot"]+ (var)
    }


    # Descriptions
    if(input$Descriptions == "y2010"){
      count=count+1
      var = table$`descriptions_y2010`#normalise(table$`descriptions_y2010`)#
      table["toplot"] =  table["toplot"]+ (var)
    }
    if(input$Descriptions == "y1980"){
      count=count+1
      var = table$`descriptions_y1980`#normalise(table$`descriptions_y1980`)#
      table["toplot"] =  table["toplot"]+ (var)
    }
    if(input$Descriptions == "y1950"){
      count=count+1
      var = table$`descriptions_y1950`#normalise(table$`descriptions_y1950`)#
      table["toplot"] =  table["toplot"]+ (var)
    }
    if(input$Descriptions == "y30_diff"){
      count=count+1
      var =table$`descriptions_y30_diff`# normalise(table$`descriptions_max_year`)#t
      table["toplot"] =  table["toplot"]+ (var)
    }
    if(input$Descriptions == "max_year"){
      count=count+1
      var =table$`descriptions_max_year`# normalise(table$`descriptions_max_year`)#t
      table["toplot"] =  table["toplot"]+ (var)
    }


    # Travel
    if(input$Travel == "Avoid"){
      count=count+1
      var = normalise(table$`Perc_6hrs`)
      table["toplot"] =  table["toplot"]+ (1-var)
    }
    if(input$Travel == "Target"){
      count=count+1
      var = normalise(table$`Perc_6hrs`)
      table["toplot"] =  table["toplot"]+ (var)
    }


    # Research
    if(input$Research == "Avoid"){
      count=count+1
      var = normalise(table$`num_instit`) # v2cafres
      table["toplot"] =  table["toplot"]+ (1-var)
    }
    if(input$Research == "Target"){
      count=count+1
      var = normalise(table$`num_instit`)
      table["toplot"] =  table["toplot"]+ (var)
    }

    # Movement
    if(input$Movement == "Avoid"){
      count=count+1
      var = normalise(table$`v2cldmovem`)
      table["toplot"] =  table["toplot"]+ (var)
    }
    if(input$Movement == "Target"){
      count=count+1
      var = normalise(table$`v2cldmovew`)
      table["toplot"] =  table["toplot"]+ (var)
    }


    # Poverty
    if(input$Poverty == "Avoid"){
      count=count+1
      var = normalise(table$`e_gdppc`)
      table["toplot"] =  table["toplot"]+ (var)
    }
    if(input$Poverty == "Target"){
      count=count+1
      var = normalise(table$`e_gdppc`)
      table["toplot"] =  table["toplot"]+ (1 - var)
    }

    # War
    if(input$War == "Avoid"){
      count=count+1
      dta = apply(table[c("e_miinteco", # international armed conflict
              "e_miinterc",
              "e_civil_war")],1,sum, na.rm=TRUE)
      var = normalise(dta)
      table["toplot"] =  table["toplot"]+ (1-var)
    }
    if(input$War == "Target"){
      count=count+1
      dta = apply(table[c("e_miinteco", # international armed conflict
                          "e_miinterc",
                          "e_civil_war")],1,sum, na.rm=TRUE)
      var = normalise(dta)
      table["toplot"] =  table["toplot"]+ (var)
    }
    if (count==0){
      table["toplot"] =  (table["toplot"])
    } else{
      table["toplot"] =  (table["toplot"])/count
    }

    tdwg3@data = table

    return(tdwg3)

  })

  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(c(0,0)) %>% addTiles() %>%
      fitBounds(-50, -50, 50,50)
  })

  observe({

  spatial = filteredData()

  pal2 <- colorNumeric(palette = "plasma", domain=as.numeric(spatial[["toplot"]]),
                       na.color = "#000000", reverse =TRUE)#,
                       #n=length((spatial[["toplot"]])))
  pal2data <- as.numeric(spatial[["toplot"]])


  leafletProxy("map", data = spatial) %>%
    clearShapes() %>%
    clearControls() %>%
    addPolygons(data = spatial,
                color = ~pal2(pal2data),
                fillOpacity = 0.75,
                weight  = 1,
                popup = #~LEVEL3_COD) %>%
                  paste0(
                      "<div>",
                      "<h3>",
                      spatial@data$LEVEL3_COD,
                      "</h3>",
                      "Effectiveness: ",
                      round(spatial@data$toplot,2),
                      "<br>",
                      "Country data: ",
                      spatial@data$COUNTRY,
                      "<br>",
                      "TDWG name: ",
                      spatial@data$LEVEL3_NAM,
                      "</div>"
                    )) %>%
    leaflet::addLegend("bottomright", pal = pal2,
              values = spatial[["toplot"]],#toplot,
              title = "Effectiveness Index",
              # na.label = "missing",
              # labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)),
              opacity = 1#, bins = 3
    )
  })

}

shinyApp(ui, server)



