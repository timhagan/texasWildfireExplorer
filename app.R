library(DescTools)
library(data.table)
library(tidycensus)
library(sf)
library(dplyr)
library(ggplot2)
library(ggridges)
library(hrbrthemes)
library(scales)
library(viridis)
library(plotly)
library(jsonlite)
library(tidycensus)
library(tigris)
library(jsonlite)
library(Hmisc)
library(caret)
library(leaflet)
library(ggsci)

wildfires_sf_ord_date <- readRDS("wildfire_data.rds")
state_info <- readRDS("state_data.rds")

# Choices for drop-downs
ui <- navbarPage("Texas Wildfires", id="nav",
                 
                 tabPanel("Interactive map",
                          div(class="outer",
                              
                              tags$head(
                                  # Include custom CSS
                                  includeCSS("styles.css"),
                                  includeScript("gomap.js")
                              ),
                              
                              # If not using custom CSS, set height of leafletOutput to a number instead of percent
                              leafletOutput("map", width="100%", height="100%"),
                              
                              # Shiny versions prior to 0.11 should use class = "modal" instead.
                              absolutePanel(id = "controls", class = "panel panel-default", 
                                            fixed = TRUE, draggable = TRUE, 
                                            top = "5%", left = "1%", right = "auto", bottom = "auto",
                                            width = 300, height = 500,
                                            
                                            h2("Controls"),
                                            
                                            radioButtons("countyFill", "County Polygon Fill", 
                                                         choices = c("Median Income" = "income_level", 
                                                                     "None" = "lightgrey"),
                                                         selected = "lightgrey"),

                                            radioButtons("fireColor", "Bubble Color", 
                                                         choices = c("Fire Cause", 
                                                                     "None" = "lightgrey"),
                                                         selected = "Fire Cause"),
                                            
                                            selectInput("incomePal", label = "Palette choice for County Median Income",
                                                        choices = c("magma","inferno", "plasma", "viridis",
                                                                    "cividis", "rocket", "mako", "turbo"),
                                                        selected = "viridis"),
                                            
                                            selectInput("causePal", label = "Palette choice for Fire Cause",
                                                        choices = c("magma","inferno", "plasma", "viridis",
                                                                    "cividis", "rocket", "mako", "turbo"),
                                                        selected = "turbo"),
                                            
                                            numericInput("binwidth", "Bin width for Frequency Polygon", 30)
                              ),

                               
                              absolutePanel(id = "sunny", class = "panel panel-default", 
                                            fixed = TRUE, draggable = TRUE, 
                                            top = "auto", left = "auto", right = "1%", bottom = "50%",
                                            width = 350, height = 350, 
                                            
                                            h2("Wildfires by County"),
                                            plotlyOutput("sunburst", height = 275)
                              ),
                              
                              
                              absolutePanel(id = "preds", class = "panel panel-default", 
                                            fixed = TRUE, draggable = TRUE, 
                                            top = "auto", left = "auto", right = "1%", bottom = "15%",
                                            width = 350, height = 350, 
                                            
                                            h2("Fire Size Predictions"),
                                            plotOutput("ridges", height = 275)
                              ),
                              
                              ### Frequency polygon
                              absolutePanel(id = "freqpanel", class = "panel panel-default", 
                                            fixed = TRUE, draggable = TRUE, 
                                            top = "auto", left = "1%", right = "auto", bottom = 75,
                                            width = "auto", height = 275,
                                            
                                            plotlyOutput("freqpoly", width = 700, height = 250)
                              ),
                              
                              absolutePanel(id = "time_sel", class = "panel panel-default", 
                                            fixed = TRUE, draggable = F, 
                                            top = "auto", left = "10%", right = "10%", bottom = "1%",
                                            width = "auto", height = 50,

                                            sliderInput("timeperiod", NULL, step = 1, width = '100%',
                                                        min = as.Date("2015-01-01", "%Y-%m-%d")-1,
                                                        max = Sys.Date()+1, 
                                                        value = c(as.Date("2022-01-01", "%Y-%m-%d"),
                                                                  Sys.Date()),
                                                        timeFormat = "%Y-%m-%d", 
                                                        dragRange = T)
                              ),
                              
                              tags$div(id="cite",
                                       'Visualization created for ', tags$em('P7: Data Analytics and Visualizations'), ' by Tim Hagan, Charles Chou, and Eric.'
                              )
                          )
                 )
)

server <- function(input, output) {
    
    causepal <- reactive(colorFactor(viridis_pal(option = input$causePal)(length(levels(wildfires_sf_ord_date$`Fire Cause`))), 
                            levels = levels(wildfires_sf_ord_date$`Fire Cause`)))
    incomepal <- reactive(colorFactor(viridis_pal(option = input$incomePal)(length(levels(state_info$income_level))), 
                             state_info$income_level))
    
    #### MAP ####
    output$map <- renderLeaflet({
        leaflet() %>%
            addTiles() %>%
            addPolygons(data = state_info, 
                        popup = ~`County Name`, 
                        layerId = ~`County Name`
                        ,fill = T
                        ,color = ~incomepal()(get(input$countyFill))
                        ) %>%
            addCircles(data = wildfires_sf_ord_date[between(wildfires_sf_ord_date$`Fire Discovery Date`,
                                                            input$timeperiod[1],
                                                            input$timeperiod[2]),], 
                       layerId = ~UniqueFireIdentifier,
                       radius = ~`Daily Acres`/(2*pi),
                       fill = T, 
                       color = ~causepal()(get(input$fireColor)), 
                       fillOpacity = .5, popup = ~FireInfo)
    })
    
    # A reactive expression that returns the set of zips that are
    # in bounds right now
    firesInBounds <- reactive({
        if (is.null(input$map_bounds))
            return(wildfires_sf_ord_date[FALSE,])
        bounds <- input$map_bounds
        latRng <- range(bounds$north, bounds$south)
        lngRng <- range(bounds$east, bounds$west)
        
        subset(wildfires_sf_ord_date,
               Y >= latRng[1] & Y <= latRng[2] &
                   X >= lngRng[1] & X <= lngRng[2] &
                   `Fire Discovery Date` <= input$timeperiod[2] &
                   `Fire Discovery Date` >= input$timeperiod[1])
    }) 
    
    #### Frequency Polygon ####
    cause_pal1 <- reactive(viridis_pal(option = input$causePal)(length(levels(wildfires_sf_ord_date$`Fire Cause`))))
    cause_pal <- reactive(c(
        "Natural" = cause_pal1()[2],
        "Unknown" = cause_pal1()[3],
        "Human" = cause_pal1()[1]
    ))
    
    output$freqpoly <- renderPlotly({
        ggplotly(ggplot(firesInBounds(), aes(x = `Fire Discovery Date`, 
                                             after_stat(density), 
                                             color = `Fire Cause`)) + 
                     geom_freqpoly(binwidth = input$binwidth) +
                     scale_color_manual(values = cause_pal()) +
                     labs(fill = "Fire Cause", x = "Discovery Date", y = "Density of Wildfires"))
    })
    
    #### SUNBURST ####
    num_fires_in_county <- reactive({
        setNames(aggregate(firesInBounds()$NAME.x, by = list(firesInBounds()$NAME.x), FUN = length),
                 c("NAME.x", "CountOfFires"))
    })
    

    state_info1 <- reactive({
        merge(state_info, num_fires_in_county(), by.x = "County Name", by.y = "NAME.x")
    })
    
    dt2 <- reactive({
        data.table(state_info1())[,.(parents = income_level,
                                     labels = NAME.y,
                                     values = CountOfFires)]
    })
    
    income_level_amounts <- reactive({
        aggregate(firesInBounds()$NAME.x, 
                  by = list(firesInBounds()$income_level),
                  FUN = length)
    })

    dt1 <- reactive({
        data.table(income_level_amounts())[,.(parents = "",
                                            labels = Group.1,
                                            values = x)]
    })
    
    
    color_pal1 <- reactive(viridis_pal(option = input$incomePal)(length(levels(state_info$income_level))))
    
    color_scheme <- reactive(c(
        "Bot 25%" = color_pal1()[1],
        "25%-50%" = color_pal1()[2],
        "50-75%" =  color_pal1()[3],
        "Top 25%" =  color_pal1()[4]
    ))
    
    sunburst_data <- reactive({
        dt_a <- rbind(dt1(), dt2())
        dt_a$colors <- ifelse(recode(dt_a$labels, !!!color_scheme(), .default = "NA") != "NA",
                              recode(dt_a$labels, !!!color_scheme(), .default = "NA"),
                              ifelse(recode(dt_a$parents, !!!color_scheme(), .default = "NA") != "NA",
                                     recode(dt_a$parents, !!!color_scheme(), .default = "NA"),
                                     "black"))
        return(dt_a)
    })

    colors_sunburst1 <- reactive({
        setNames(recode(dt1()$labels, !!!color_scheme(), .default = "black"),
                 dt1()$labels) 
    })
    
    colors_sunburst2 <- reactive({
        setNames(recode(dt2()$parents, !!!color_scheme(), .default = "black"),
                 dt2()$parents) 
    })

    output$sunburst <- renderPlotly({
        plot_ly(sunburst_data(),
            type= "sunburst",
            labels= ~labels,
            parents = ~parents,
            values = ~values, 
            marker = list(colors = c(colors_sunburst1(), colors_sunburst2()))
        ) %>% layout(title="Count by Median Income")
    })
    

    # observe the marker click info and print to console when it is changed.
    observeEvent(input$map_shape_click, {
        if(grepl("20",input$map_shape_click$id, fixed = T)){
            clickedPoint <- input$map_shape_click
            wildfires_sf_ord_date$highlighted <- ifelse(wildfires_sf_ord_date$rf.log.predictions.bin==
                                                            wildfires_sf_ord_date[clickedPoint$id==wildfires_sf_ord_date$UniqueFireIdentifier,]$rf.log.predictions.bin,
                                                        T,F)
            output$ridges <- renderPlot({
                ggplot(wildfires_sf_ord_date[!wildfires_sf_ord_date$trainingData,], 
                       aes(x = DailyAcres_log, y = rf.log.predictions.bin, 
                           fill = highlighted)) +
                    geom_density_ridges_gradient(scale = 2, rel_min_height = 0.02) +
                    scale_fill_manual(values = c("lightgrey", "darkred")) +
                    scale_y_discrete(labels = c("Lowest", "Low", "Low-Mid", "Mid", "Mid-High", "High", "Highest")) +
                    labs(title = NULL, subtitle = "Predicted vs Actual On Out of Time Test Data") +
                    xlab("Log of Actual Fire Size") + ylab("Fire Size Prediction Bins") +
                    theme(
                        legend.position="none",
                        panel.spacing = unit(0.1, "lines"),
                        strip.text.x = element_text(size = 8)
                    )
            })
        } else {
            wildfires_sf_ord_date$highlighted <- F
            output$ridges <- renderPlot({
                ggplot(wildfires_sf_ord_date[!wildfires_sf_ord_date$trainingData,], 
                       aes(x = DailyAcres_log, y = rf.log.predictions.bin, 
                           fill = highlighted)) +
                    geom_density_ridges_gradient(scale = 2, rel_min_height = 0.02) +
                    scale_fill_manual(values = c("lightgrey", "darkred")) +
                    scale_y_discrete(labels = c("Lowest", "Low", "Low-Mid", "Mid", "Mid-High", "High", "Highest")) +
                    labs(title = NULL, subtitle = "Predicted vs Actual On Out of Time Test Data") +
                    xlab("Log of Actual Fire Size") + ylab("Fire Size Prediction Bins") +
                    theme(
                        legend.position="none",
                        panel.spacing = unit(0.1, "lines"),
                        strip.text.x = element_text(size = 8)
                    )
            })
        }
    }, 
    ignoreInit = T, ignoreNULL = T)
    
    observeEvent(input$map_click,{
        wildfires_sf_ord_date$highlighted <- F
        output$ridges <- renderPlot({
            ggplot(wildfires_sf_ord_date[!wildfires_sf_ord_date$trainingData,], 
                   aes(x = DailyAcres_log, y = rf.log.predictions.bin, 
                       fill = highlighted)) +
                geom_density_ridges_gradient(scale = 2, rel_min_height = 0.02) +
                scale_fill_manual(values = c("lightgrey", "darkred")) +
                scale_y_discrete(labels = c("Lowest", "Low", "Low-Mid", "Mid", "Mid-High", "High", "Highest")) +
                labs(title = NULL, subtitle = "Predicted vs Actual On Out of Time Test Data") +
                xlab("Log of Actual Fire Size") + ylab("Fire Size Prediction Bins") +
                theme(
                    legend.position="none",
                    panel.spacing = unit(0.1, "lines"),
                    strip.text.x = element_text(size = 8)
                )
        })
    })
    
    output$ridges <- renderPlot({
        ggplot(wildfires_sf_ord_date[!wildfires_sf_ord_date$trainingData,], 
               aes(x = DailyAcres_log, y = rf.log.predictions.bin, 
                   fill = highlighted)) +
            geom_density_ridges_gradient(scale = 2, rel_min_height = 0.02) +
            scale_fill_manual(values = c("lightgrey", "darkred")) +
            scale_y_discrete(labels = c("Lowest", "Low", "Low-Mid", "Mid", "Mid-High", "High", "Highest")) +
            labs(title = NULL, subtitle = "Predicted vs Actual On Out of Time Test Data") +
            xlab("Log of Actual Fire Size") + ylab("Fire Size Prediction Bins") +
            theme(
                legend.position="none",
                panel.spacing = unit(0.1, "lines"),
                strip.text.x = element_text(size = 8)
            )
    })
}

shinyApp(ui, server)

#################
#################
#################

