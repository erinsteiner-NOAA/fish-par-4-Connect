# TITLE: Participation Survey Data Shiny App 2017 and 2020
# AUTHOR: C. Lewis-Smith
# DATE: Oct. 2021

# Prepare environment ----

# Load libraries

library(shiny)
library(DT)
library(here)
library(sf)
library(terra)
library(dplyr)
library(readr)
library(stringr)
library(readxl)
library(leaflet)
library(ggplot2)
library(htmltools)
library(forestmangr)
library(manipulateWidget)

# Load, rename, and clean survey data  ----

county17sf <- read_sf(here("outputs", "county17sf.shp"))
county17df <- read_csv(here("outputs", "county17df.csv")) %>% 
  as.data.frame() %>% 
  rename(`Median Household Income` = `Median Income 2017`) %>% 
  rename(`Urban-rural Continuum` = `Rural-urban Continuum`) 

county20sf <- read_sf(here("outputs", "county20sf.shp"))
county20df <- read_csv(here("outputs", "county20df.csv")) %>% 
  as.data.frame() %>% 
  rename(`Median Household Income` = `Median Income 2017`) %>% 
  rename(`Urban-rural Continuum` = `Rural-urban Continuum`)

# Round college variable
county17df$`Percent College Grad` <- round(county17df$`Percent College Grad`, 2)
county20df$`Percent College Grad` <- round(county20df$`Percent College Grad`, 2)

county17 = county17sf %>% 
  merge(county17df)  # Missing field created even with null rownames in d

county17 <- st_transform(county17, crs = 4326)

county20 = county20sf %>% 
  merge(county20df) 

county20 <- st_transform(county20, crs = 4326)

# Make lists for selection tool ----

question.names17 = county17df %>% 
  select(contains("Q")) 

question.names.non = county17df %>% 
  select(!contains(c("Q", "STATE", "...", "COUNTY", "NAME")))

question.names20 = county20df %>% 
  select(contains("Q")) 

# Create Base Maps 
county_imap <- leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% ###base group
  setView(lng = -115.252667, lat = 41.7850, zoom = 4)

county_imap2 <- leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% ###base group
  setView(lng = -115.252667, lat = 41.7850, zoom = 4)


# Create Map 3 Base Map
# Have to add Q1 as default to work around bug

palQ1 <- colorBin("YlOrRd", domain = county20$`Q1: Age`, bins = 6)

county_imap3 <- leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% ###base group
  setView(lng = -115.252667, lat = 41.7850, zoom = 4) %>% 
  addPolygons(data = county20,
              stroke = F,
              smoothFactor = .2,
              fillOpacity = .7,
              label = ~NAME, 
              color = ~palQ1(`Q1: Age`),
              highlight = highlightOptions(weight = 2, 
                                           color = "red",
                                           fillOpacity = 0.7,
                                           bringToFront = F),
              dashArray = "3",
              layerId = county20$NAME,
              group = "Q1") %>% 
  leaflet::addLegend(
    data = county20, ##overlay group 2 legend
    pal = palQ1, values = ~`Q1: Age`,
    opacity = .7,
    title = "Weighted Mean",
    group = "Q1", position = "bottomright",
    className = "info legend aggregate risk" ###not showing up
  )

palIncome <- colorQuantile(palette = "YlOrRd", 
                         domain = county20$`Median Household Income` , n = 6 
)

county_imap4 <- leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% ###base group
  setView(lng = -115.252667, lat = 41.7850, zoom = 4) %>% 
  addPolygons(data = county20,
              stroke = F,
              smoothFactor = .2,
              fillOpacity = .7,
              label = ~NAME, 
              color = ~palIncome(`Median Household Income`),
              highlight = highlightOptions(weight = 2, 
                                           color = "red",
                                           fillOpacity = 0.7,
                                           bringToFront = F),
              dashArray = "3",
              group = "Income") %>% 
  clearControls() %>%
  leaflet::addLegend(
    data = county20, 
    pal = palIncome, values = ~`Median Household Income`,
    opacity = .7,
    title = "Income",
    group = "Income", position = "bottomright",
    className = "info legend aggregate risk" 
  ) 
 

# UI ----

ui = fluidPage(
  titlePanel(
    fluidRow(
      column(3, img(height = 75, width = 375, src = "noaalogo.png")),
      column(2, offset = 7, img(height = 75, width = 175, src = "noaalogoside.png"))
    )
  ),
 navbarPage("West Coast Fisheries Participation Survey",
            tabPanel("Visualize Responses",
            
 tabsetPanel( id = "tabs",           
  
  # Tab for 2017 survey responses ----
  tabPanel("2017 Survey", value = "A",
  fluidRow(
    column(5, 
           tabsetPanel(
             tabPanel("Page 1", id="dummy", img(height = 600,
                                                width = 500,
                                                src = "2017p1.png")), 
             tabPanel("Page 2", id="dummy", img(height = 600,
                                                width = 500,
                                                src = "2017p2.png")), 
             tabPanel("Page 3", id="dummy", img(height = 600,
                                                width = 500,
                                                src = "2017p3.png")),
             tabPanel("Page 4", id="dummy", img(height = 600,
                                                width = 500,
                                                src = "2017p4.png"))
           )
    ),
    column(width = 3, offset = .5,
           leafletOutput(outputId = "map1"),
           selectInput(inputId = "select",
                       label = "Select Survey Question",
                       multiple = FALSE,
                       colnames(question.names17))
    ),
    column(width = 3,
           leafletOutput(outputId = "map2"),
           selectInput(inputId = "selectnon17",
                       label = "Select County Variable",
                       multiple = FALSE,
                       colnames(question.names.non))
           )
        )
  ),
  
  # Tab for 2020 responses ----
  
  tabPanel("2020 Survey", value = "B",
           fluidRow(
             column(5, 
                    tabsetPanel(
                      tabPanel("Page 1", id="dummy", img(height = 675,
                                                         width = 525,
                                                         src = "2020p1.png")), 
                      tabPanel("Page 2", id="dummy", img(height = 675,
                                                         width = 525,
                                                         src = "2020p2.png")),
                      tabPanel("Page 3", id="dummy", img(height = 675,
                                                         width = 525,
                                                         src = "2020p3.png")),
                      tabPanel("Page 4", id="dummy", img(height = 675,
                                                         width = 525,
                                                         src = "2020p4.png"))
                    )
             ),
             column(width = 3, offset = .5, 
                    leafletOutput(outputId = "map3"),
                    selectInput(inputId = "select2",
                                label = "Select Survey Question",
                                multiple = FALSE,
                                colnames(question.names20))
             ),
             column(3, 
                    leafletOutput(outputId = "map4"),
                    selectInput(inputId = "selectnon20",
                                label = "Select County Variable",
                                multiple = FALSE,
                                colnames(question.names.non))
           )
           )
      ) 
  )
            ),
 tabPanel("About",
          column(4, 
                 h5("In 2017 and 2020 we conducted mail surveys to collect information about West Coast fishermen, 
                    why they choose to participate in commercial fishing, and the benefits they derive from fishing, 
                    including non-monetary benefits. We also wanted to understand how individuals and communities are 
                    affected when opportunities and profitability in particular fisheries change. We sent the survey 
                    to all vessel owners who had commercial fishery landings in Washington, Oregon or California in 
                    the years preceding the surveys. We received over 1450 responses to our survey both years, over 
                    a 50% response rate."),
                 h5("There are two additional applications for viewing the ", a("2017", href = "https://www.fisheries.noaa.gov/data-tools/west-coast-fisheries-participation-survey-result-tool-2017"),
                 " and ", a("2020", href = "https://www.fisheries.noaa.gov/data-tools/west-coast-fisheries-participation-survey-result-tool-2020"), " results. The surveys were largely similar but a 
                 few questions differ between years. Results can be viewed 
                 for all respondents or by state of residence of the respondents. The frequency tables as well as the charts can 
                 be downloaded. Survey design and data collection was produced in partnership with ",
                 a("Washington Sea Grant", href = "https://wsg.washington.edu/"))
                 
          ),
          column(5, 
                  id="responses", img(height = 575,
                                      width = 800,
                                      src = "sidebyside.png")) 
          
 ),
 tabPanel("Sources",
          column(6, 
                 h4("Additional Data Sources:"),
                 h5("United States Department of Agriculture. (2019). Education completion rates and Urban-rural Continuum [Data file].", 
                    a("Retrieved from https://data.ers.usda.gov/reports.aspx?ID=17829", 
                      href="https://data.ers.usda.gov/reports.aspx?ID=17829")),
                 h5("United States Census Bureau. (2019). County Population Totals: 2010-2019 [Data file].", 
                    a("Retrieved from https://www.census.gov/data/tables/time-series/demo/popest/2010s-counties-total.html#par_textimage_242301767", 
                      href="https://www.census.gov/data/tables/time-series/demo/popest/2010s-counties-total.html#par_textimage_242301767")),
                 h5("United States Department of Commerce. (2017). County Median Household Income [Data file].", 
                    a("Retrieved from https://www.bea.gov/data/income-saving/personal-income-county-metro-and-other-areas", 
                    href="https://www.bea.gov/data/income-saving/personal-income-county-metro-and-other-areas")),
                 h5("United States Department of Housing and Urban Development. (2020). HUD-USPS ZIP Crosswalk Files [Data file].", 
                    a("Retrieved from https://www.huduser.gov/portal/datasets/usps_crosswalk.html#data", 
                      href="https://www.huduser.gov/portal/datasets/usps_crosswalk.html#data"))
          )
 
    )
)

)

# Server ----        
server = function(input, output, session) {

  # Create server base environment ----  
  
  # Create default reactive filters
  data_filter <- reactive({
    county17 %>% 
      select(as.symbol(input$select), NAME, geometry)
  })
  
  data_filter3 <- reactive({
    county17 %>% 
      select(as.symbol(input$selectnon17), NAME, geometry)
  })
  
  data_filter2 <- reactive({
    county20 %>% 
      select(as.symbol(input$select2), NAME, geometry)
  })
  
  data_filter4 <- reactive({
    county20 %>% 
      select(as.symbol(input$selectnon20), NAME, geometry)
  })
  
  # Create default pals
  pal <- reactive ({
    colorNumeric("viridis", data_filter(), domain = NULL)
  })
  
  pal2 <- reactive ({
    colorNumeric("viridis", data_filter2(), domain = NULL)
  })
  
  pal3 <- reactive ({
    colorNumeric("viridis", data_filter3(), domain = NULL)
  })
  
  pal4 <- reactive ({
    colorNumeric("viridis", data_filter4(), domain = NULL)
  })
  
  output$map1 <-renderLeaflet({
    county_imap
  })
  
  output$map2 <-renderLeaflet({
    county_imap2
  })
  
  output$map3 <-renderLeaflet({
    county_imap3
  })
  
  output$map4 <-renderLeaflet({
    county_imap4 
  })
 
  
  observe({
    leafletProxy("map1", data = data_filter()) %>% 
      clearShapes() %>%
      addPolygons(
        fillColor = ~pal()
      )
  })
  
  observe({
    leafletProxy("map2", data = data_filter3()) %>% 
      clearShapes() %>%
      addPolygons(
        fillColor = ~pal3()
      )
  })

  observe({
    leafletProxy("map3", data = data_filter2()) %>% 
      clearShapes() %>%
      addPolygons(
        fillColor = ~pal2()
      )
  })
  
  observe({
    leafletProxy("map4", data = data_filter4()) %>% 
      clearShapes() %>%
      addPolygons(
        fillColor = ~pal4()
      )
  })
  
  # Reactive 2017 maps ---- 
  
  # Make map1 interactive proxy
  
  observeEvent({input$select},{
    hour_column <- paste0(input$select)
    data = data_filter()[hour_column]
    pal <- colorBin("YlOrRd", domain = as.numeric(data[[hour_column]]), bins = 6)
    leafletProxy("map1", data=data)%>%
      clearShapes()%>%
      addPolygons( 
        fillColor = pal(as.numeric(data[[hour_column]])),  
        weight = 0.0,
        opacity = 1,
        color = "white",
        label = county17$NAME,
        layerId = county17$NAME,
        highlight = highlightOptions(weight = 2, 
                                     color = "red",
                                     fillOpacity = 0.7,
                                     bringToFront = F),
        dashArray = "3",
        fillOpacity = 0.7 
      )   %>% clearControls() %>%
      addLegend(pal = pal, values =as.numeric(data[[hour_column]]), opacity = 0.7, title = "Weighted Averages", position = "bottomright")
  })
 
  # Make map2 proxy with county variables
   observeEvent (input$selectnon17 , {
     
     pal3 <- colorQuantile(palette = "YlOrRd", 
                           domain = county17$`Median Household Income` , n = 6 
     )
     
     pal4 <- colorQuantile(palette = "YlOrRd", 
                           domain = county17$`Population Estimate` , n = 6 
     )
     
     pal5 <- colorNumeric(palette = "YlOrRd", 
                          domain = county17$`Urban-rural Continuum` , n = 7 
     )
     
     pal6 <- colorQuantile(palette = "YlOrRd", county17$`Percent College Grad`, n = 6
     )
 
    if(input$selectnon17 == "Median Household Income"){
      leafletProxy("map2", data = county17) %>%
        clearShapes() %>%
        addPolygons(stroke = F,
                    smoothFactor = .2,
                    fillOpacity = .7,
                    label = ~NAME, 
                    color = ~pal3(`Median Household Income`),
                    highlight = highlightOptions(weight = 2, 
                                                 color = "red",
                                                 fillOpacity = 0.7,
                                                 bringToFront = F),
                    dashArray = "3",
                    group = "Income") %>% 
        clearControls() %>%
        leaflet::addLegend(
          data = county17, ##overlay group 2 legend
          pal = pal3, values = ~`Median Household Income`,
          opacity = .7,
          title = "Income",
          group = "Income", position = "bottomright",
          className = "info legend aggregate risk" ###not showing up
        ) 
    }else{
      if(input$selectnon17 == "Population Estimate"){
      leafletProxy("map2", data = county17) %>%
        clearShapes() %>%
        addPolygons(data = county17, ##overlay group 3
                    fillColor = ~pal4(`Population Estimate`),
                    color = "transparent",
                    fillOpacity = .7,
                    label = ~NAME,
                    highlight = highlightOptions(weight = 2, 
                                                 color = "red",
                                                 fillOpacity = 0.7,
                                                 bringToFront = F),
                    dashArray = "3",
                    group = "Population"
        ) %>%
        clearControls() %>%
        leaflet::addLegend(
          data = county17, ##overlay group 3 legend
          pal = pal4, 
          opacity = .7,
          na.label = "NA",
          labels = NAME,
          values = ~`Population Estimate`,
          title = "Population",
          group = "Population", position = "bottomright",
          className = "info legend")  
      }else{
        if(input$selectnon17 == "Urban-rural Continuum"){
          leafletProxy("map2", data = county17) %>%
            clearShapes() %>%
            addPolygons(data = county17, ##overlay group 3
                        fillColor = ~pal5(`Urban-rural Continuum`),
                        color = "transparent",
                        fillOpacity = .7,
                        label = ~NAME,
                        highlight = highlightOptions(weight = 2, 
                                                     color = "red",
                                                     fillOpacity = 0.7,
                                                     bringToFront = F),
                        dashArray = "3",
                        group = "Urban-rural"
            ) %>%
            clearControls() %>%
            leaflet::addLegend(
              data = county17, ##overlay group 3 legend
              pal = pal5, 
              opacity = .7,
              na.label = "NA",
              labels = NAME,
              values = ~`Urban-rural Continuum`,
              title = "Urban-rural",
              group = "Urban-rural", position = "bottomright",
              className = "info legend"
            )   
          
        }else{
        if (input$selectnon17 == "Percent College Grad"){
          leafletProxy("map2", data = county17) %>%
            clearShapes() %>%
            addPolygons(data = county17, ##overlay group 3
                        fillColor = ~pal6(`Percent College Grad`),
                        color = "transparent",
                        fillOpacity = .7,
                        label = ~NAME,
                        highlight = highlightOptions(weight = 2, 
                                                     color = "red",
                                                     fillOpacity = 0.7,
                                                     bringToFront = F),
                        dashArray = "3",
                        group = "College Education"
            ) %>%
            clearControls() %>%
            leaflet::addLegend(
              data = county17, ##overlay group 3 legend
              pal = pal6, 
              opacity = .7,
              na.label = "NA",
              labels = NAME,
              values = ~`Percent College Grad`,
              title = "College Education",
              group = "College Education", position = "bottomright",
              className = "info legend"
            )    
        }
        
      }
      
    }
    
    }
  })

   # Reactive 2020 maps ---- 
   
   # Make map3 interactive proxy 
   observeEvent({input$select2},{
     hour_column <- paste0(input$select2)
     data = data_filter2()[hour_column]
     pal <- colorBin("YlOrRd", domain = as.numeric(data[[hour_column]]), bins = 6)
     leafletProxy("map3", data=data)%>%
       clearShapes()%>%
       addPolygons( 
         fillColor = pal(as.numeric(data[[hour_column]])),  
         weight = 0.0,
         opacity = 1,
         color = "white",
         label = county20$NAME,
         layerId = county20$NAME,
         highlight = highlightOptions(weight = 2, 
                                      color = "red",
                                      fillOpacity = 0.7,
                                      bringToFront = F),
         dashArray = "3",
         fillOpacity = 0.7 
       )   %>% clearControls() %>%
       addLegend(pal = pal, values =as.numeric(data[[hour_column]]), opacity = 0.7, title = "Weighted Averages", position = "bottomright")
   }) 

   # Make map4 proxy with county variables  
   observeEvent (input$selectnon20 , {
     
     pal3.20 <- colorQuantile(palette = "YlOrRd", 
                              domain = county20$`Median Household Income` , n = 6 
     )
     
     pal4.20 <- colorQuantile(palette = "YlOrRd", 
                              domain = county20$`Population Estimate` , n = 6 
     )
     
     pal5.20 <- colorNumeric(palette = "YlOrRd", 
                             domain = county20$`Urban-rural Continuum` , n = 7 
     )
     
     pal6.20 <- colorQuantile(palette = "YlOrRd", county20$`Percent College Grad`, n = 6
                              
     )
     
     if(input$selectnon20 == "Median Household Income"){
       leafletProxy("map4", data = county20) %>%
         clearShapes() %>%
         addPolygons(stroke = F,
                     smoothFactor = .2,
                     fillOpacity = .7,
                     label = ~NAME, 
                     color = ~pal3.20(`Median Household Income`),
                     highlight = highlightOptions(weight = 2, 
                                                  color = "red",
                                                  fillOpacity = 0.7,
                                                  bringToFront = F),
                     dashArray = "3",
                     group = "Income") %>% 
         clearControls() %>%
         leaflet::addLegend(
           data = county20, 
           pal = pal3.20, values = ~`Median Household Income`,
           opacity = .7,
           title = "Income",
           group = "Income", position = "bottomright",
           className = "info legend aggregate risk" 
         ) 
     }else{
       if(input$selectnon20 == "Population Estimate"){
         leafletProxy("map4", data = county20) %>%
           clearShapes() %>%
           addPolygons(data = county20, ##overlay group 3
                       fillColor = ~pal4.20(`Population Estimate`),
                       color = "transparent",
                       fillOpacity = .7,
                       label = ~NAME,
                       highlight = highlightOptions(weight = 2, 
                                                    color = "red",
                                                    fillOpacity = 0.7,
                                                    bringToFront = F),
                       dashArray = "3",
                       group = "Population"
           ) %>%
           clearControls() %>%
           leaflet::addLegend(
             data = county20, 
             pal = pal4.20, 
             opacity = .7,
             na.label = "NA",
             labels = NAME,
             values = ~`Population Estimate`,
             title = "Population",
             group = "Population", position = "bottomright",
             className = "info legend")  
       }else{
         if(input$selectnon20 == "Urban-rural Continuum"){
           leafletProxy("map4", data = county20) %>%
             clearShapes() %>%
             addPolygons(data = county20, 
                         fillColor = ~pal5.20(`Urban-rural Continuum`),
                         color = "transparent",
                         fillOpacity = .7,
                         label = ~NAME,
                         highlight = highlightOptions(weight = 2, 
                                                      color = "red",
                                                      fillOpacity = 0.7,
                                                      bringToFront = F),
                         dashArray = "3",
                         group = "Urban-rural"
             ) %>%
             clearControls() %>%
             leaflet::addLegend(
               data = county20, 
               pal = pal5.20, 
               opacity = .7,
               na.label = "NA",
               labels = NAME,
               values = ~`Urban-rural Continuum`,
               title = "Urban-rural",
               group = "Urban-rural", position = "bottomright",
               className = "info legend"
             )   
           
         }else{
           if (input$selectnon20 == "Percent College Grad"){
             leafletProxy("map4", data = county20) %>%
               clearShapes() %>%
               addPolygons(data = county20, 
                           fillColor = ~pal6.20(`Percent College Grad`),
                           color = "transparent",
                           fillOpacity = .7,
                           label = ~NAME,
                           highlight = highlightOptions(weight = 2, 
                                                        color = "red",
                                                        fillOpacity = 0.7,
                                                        bringToFront = F),
                           dashArray = "3",
                           group = "College Education"
               ) %>%
               clearControls() %>%
               leaflet::addLegend(
                 data = county20, 
                 pal = pal6.20, 
                 opacity = .7,
                 na.label = "NA",
                 labels = NAME,
                 values = ~`Percent College Grad`,
                 title = "College Education",
                 group = "College Education", position = "bottomright",
                 className = "info legend"
               )    
           }
           
         }
         
       }
       
     }
   })
  
   # Create map mirroring ---- 
 
# Observer to respond to zoom / pan of map1 and apply to map2 
  observe({ 
    coords <- input$map1_bounds
    
    if (!is.null(coords)) {
      tproxy <- leafletProxy("map2") %>% 
        fitBounds(coords$west,
                  coords$south,
                  coords$east,
                  coords$north)
    }
  })
 
# Observer to respond to zoom / pan of map2 and apply to map1 
  observe({ 
    coords <- input$map2_bounds
    
    if (!is.null(coords)) {
      tproxy <- leafletProxy("map1") %>% 
        fitBounds(coords$west,
                  coords$south,
                  coords$east,
                  coords$north)
    }
  })
 
# Observer to respond to zoom / pan of map3 and apply to map4 
  observe({ 
    coords <- input$map3_bounds
    
    if (!is.null(coords)) {
      tproxy <- leafletProxy("map4") %>% 
        fitBounds(coords$west,
                  coords$south,
                  coords$east,
                  coords$north)
    }
  })
 
# Observer to respond to zoom / pan of map4 and apply to map3 
  observe({ 
    coords <- input$map4_bounds
    
    if (!is.null(coords)) {
      tproxy <- leafletProxy("map3") %>% 
        fitBounds(coords$west,
                  coords$south,
                  coords$east,
                  coords$north)
    }
  })

  # Create click pop up tool ----   
  # Click tool 2017 
  observe(
    { click = input$map1_shape_click
    selecter = as.character(input$select)
    sub = county17[county17$NAME==input$map1_shape_click$id, c("NAME", "Median Household Income", 
                                                               "Population Estimate", "Urban-rural Continuum",
                                                               "Percent College Grad")]
    nm1 = paste(county17df[county17df$NAME == input$map1_shape_click$id, c((paste(input$select)))])
    nm = sub$NAME
    selecter2 = str_extract(selecter, "[^ ]+") 
    income = format(sub$`Median Household Income`, big.mark = ",")
    popul = format(sub$`Population Estimate`, big.mark = ",")
    rural = sub$`Urban-rural Continuum`
    college = ((sub$`Percent College Grad`)*100)
    popformat = paste(nm, "County", "<br>",  "Weighted Mean", selecter2, nm1,"<br>", "Median income:",
                      income, "<br>", "Population:", popul, "<br>", "Urban to rural:", rural, "<br>",
                      "College degree %:", college, "<br>",
                      sep = " ")
    if(is.null(click))
      return()
    else
      leafletProxy("map1") %>% 
      clearMarkers() %>% 
      addPopups(lng = click$lng, lat = click$lat, popup= popformat, 
                options = popupOptions(closeOnClick = T))
    }
  ) 

# Click tool 2020  
  observe(
    { click = input$map3_shape_click
    selecter = as.character(input$select2)
    sub = county20[county20$NAME==input$map3_shape_click$id, c("NAME", "Median Household Income", 
                                                               "Population Estimate", "Urban-rural Continuum",
                                                               "Percent College Grad")]
    nm1 = paste(county20df[county20df$NAME == input$map3_shape_click$id, c((paste(input$select)))])
    nm = sub$NAME
    selecter2 = str_extract(selecter, "[^ ]+") 
    income = format(sub$`Median Household Income`, big.mark = ",")
    popul = format(sub$`Population Estimate`, big.mark = ",")
    rural = sub$`Urban-rural Continuum`
    college = ((sub$`Percent College Grad`)*100)

    popformat = paste(nm, "County", "<br>",  "Weighted Mean", selecter2, nm1,"<br>", "Median income:",
                      income, "<br>", "Population:", popul, "<br>", "Urban to rural:", rural, "<br>",
                      "College degree %:", college, "<br>",
                      sep = " ")
    if(is.null(click))
      return()
    else
      leafletProxy("map3") %>% 
      clearMarkers() %>% 
      addPopups(lng = click$lng, lat = click$lat, popup= popformat, 
                options = popupOptions(closeOnClick = T))
    }
  ) 
  
  
}

shinyApp(ui, server) 

# Source for Education and Urban-rural
# https://data.ers.usda.gov/reports.aspx?ID=17829 

# Source for income Data
# https://www.bea.gov/data/income-saving/personal-income-county-metro-and-other-areas

# Source Population Data
# https://www.census.gov/data/tables/time-series/demo/popest/2010s-counties-total.html#par_textimage_242301767
# Source: U.S. Census Bureau, Population Division

