library(shiny)
library(leaflet)
library(RColorBrewer)


#dataprep


normalise <- function(x){(x-min(x,na.rm=T))/(max(x,na.rm=T)-min(x,na.rm=T))}


# rsconnect::deployApp('C:/Users/kdh10kg/Documents/github/darkspots_shiny/darkspots_shiny')
load("app_data.RData")
# simplifiedBoundaries <- tdwg3#boundaries # rmapshaper::ms_simplify(boundaries)


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
                  label = "Number of species left to observe",
                  c("Ignore" = "Ignore",
                    "Areas with the most species (wcvp output)" = "Avoid",
                    "Areas with the most species (model output)" = "Target"),
                  selected = "Avoid"),

      # selectInput(inputId = "Discoveries",
      #             label = "Discoveries",
      #             c("Ignore" = "Ignore",
      #               "Sample in less known areas" = "Avoid",
      #               "Most sampled areas" = "Target")),
      #
      # selectInput(inputId = "Descriptions",
      #             label = "Descriptions",
      #             c("Ignore" = "Ignore",
      #               "Describe species in less known areas" = "Avoid",
      #               "Most described areas" = "Target"),
      #             selected = "Target"),

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
    table["benefit"] = 0
    table["cost"] = 0
    benefit_count=0
    cost_count=0


    # Species left to discover
    if(input$Species == "Avoid"){
      benefit_count=benefit_count+1
      var = table$`left_spp_wcvp`#normalise(table$`left_spp_wcvp`)
      table["benefit"] =  table["benefit"]+ (var)
    }
    if(input$Species == "Target"){
      benefit_count=benefit_count+1
      var = table$`spp_left`#normalise(table$`spp_left`)
      table["benefit"] =  table["benefit"]+ (var)
    }

    # # Discoveries
    # if(input$Species == "Avoid"){
    #   benefit_count=benefit_count+1
    #   var = table$`discoveries`#normalise(table$`discoveries`)
    #   table["benefit"] =  table["benefit"]+ (1-var)
    # }
    # if(input$Discoveries == "Target"){
    #   count=count+1
    #   var = table$`discoveries`#normalise(table$`discoveries`)
    #   table["toplot"] =  table["toplot"]+ (var)
    # }
    #
    # # Descriptions
    # if(input$Descriptions == "Avoid"){
    #   count=count+1
    #   var =normalise(table$`descriptions`)
    #   table["toplot"] =  table["toplot"]+ (1-var)
    # }
    # if(input$Descriptions == "Target"){
    #   count=count+1
    #   var = normalise(table$`descriptions`)
    #   table["toplot"] =  table["toplot"]+ (var)
    # }

    # Travel
    if(input$Travel == "Avoid"){
      cost_count=cost_count+1
      var = normalise(table$`Perc_6hrs`)*100
      table["cost"] =  table["cost"]+ (var)
    }
    if(input$Travel == "Target"){
      cost_count=cost_count+1
      var = normalise(table$`Perc_6hrs`)*100
      table["cost"] =  table["cost"]+ (max(var, na.rm = TRUE)-var)
    }


    # Research
    if(input$Research == "Avoid"){
      cost_count=cost_count+1
      var = normalise(table$`num_instit`)*100 # v2cafres
      table["cost"] =  table["cost"]+ (var)
    }
    if(input$Research == "Target"){
      cost_count=cost_count+1
      var = normalise(table$`num_instit`)*100
      table["cost"] =  table["cost"]+ (max(var, na.rm = TRUE)-var)
    }

    # Movement
    if(input$Movement == "Avoid"){
      cost_count=cost_count+1
      var = normalise(table$`v2cldmovem`)*100
      table["cost"] =  table["cost"] + (max(var, na.rm = TRUE)-var)
    }
    if(input$Movement == "Target"){
      cost_count=cost_count+1
      var = normalise(table$`v2cldmovew`)*100
      table["cost"] =  table["cost"] + (max(var, na.rm = TRUE)-var)
    }


    # Poverty
    if(input$Poverty == "Avoid"){
      cost_count=cost_count+1
      var = normalise(table$`e_gdppc`)*100#table$`e_gdppc`#
      table["cost"] =  table["cost"]+ (max(var, na.rm = TRUE)-var)
    }
    if(input$Poverty == "Target"){
      cost_count=cost_count+1
      var = normalise(table$`e_gdppc`)*100#table$`e_gdppc`#
      table["cost"] =  table["cost"]+ (var)
    }

    # War
    if(input$War == "Avoid"){
      cost_count=cost_count+1
      dta = apply(table[c("e_miinteco", # international armed conflict
                          "e_miinterc",
                          "e_civil_war")],1,sum, na.rm=TRUE)
      var = normalise(dta)
      table["cost"] =  table["cost"]+ (var)
    }
    if(input$War == "Target"){
      cost_count=cost_count+1
      dta = apply(table[c("e_miinteco", # international armed conflict
                          "e_miinterc",
                          "e_civil_war")],1,sum, na.rm=TRUE)
      var = normalise(dta)
      table["cost"] =  table["cost"]+ (max(var, na.rm = TRUE)-var)
    }
    if (cost_count==0){
      cost_count = 1
      table["cost"] = 1
    }
    if (benefit_count==0){
      benefit_count = 1
      table["benefit"] = 1
    }
    table["benefit"] = table["benefit"]/ benefit_count
    table["cost"] = table["cost"]/ cost_count
    table["toplot"] = table["benefit"] / (table["cost"]+0.001)

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



