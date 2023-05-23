# Helper packages
#' @import dplyr
#' @import RODBC
#' @import splus2R

# Packages containing shiny functions
#' @import shiny
#' @import shinyBS
#' @import shinythemes
#' @import shinyvalidate

# Packages containing spatial functions
#' @import CCAMLRGIS
#' @import leaflet
#' @import leaflet.extras
#' @import raster
#' @import rgeos
#' @import sf

# Packages containing modelling functions
#' @import gam
#' @import MASS
#' @import mgcv
#' @import pwr

# Packages for visualisation of data
#' @import data.table
#' @import effects
#' @import GGally
#' @import ggplot2
#' @import lattice


#' @title Randomly anonymise a vector
#'
#' This function randomly anonymises a vector, changing the values to letter codes.
#' @param x The vector to be anonymised.
#' @param anon.level How many letters should be used in the anonymised codes.
#' @param return.lookup Should the function return a lookup table to find the original values?
#' @returns A vector the same length as the original, with values replaced by a randomly generated code. Identical values correspond to identical codes. If return.lookup = T, a table is also returned that allows the original values to be extracted from the anonymised list.
#' @note If the number of unique values in x is very large (exceeds 26^anon.level) the app will return an error. The function may also take a long time to run if the number of unique values in x is very high.
#' @examples
#' ## Anonymise the ShipNames in the data
#' anonymise(catcheffort.sample$ShipName)
#'
#' ## Keep a lookup table for later checking
#' ship.anon <- anonymise(catcheffort.sample$ShipName, return.lookup = T)
#' ship.anon$anon.list # List of anonymised values
#' ship.anon$anon.lookup # Lookup table
#' @export

anonymise <- function(x, anon.level = 4, return.lookup = F){
  y <- unique(x)
  x.anon <- x
  lookup.x <- array()
  lookup.code <- array()
  place <- 1

  if(length(LETTERS)^anon.level < length(y)){
    stop('The anon.level is not sufficiently high to anonymise the data. This may be caused by a large number of unique values in the data. Increase the anon.level, or check your data has the expected number of unique values.')
  }

  for(i in y){
    y.anon <- paste(sample(LETTERS, size = anon.level), collapse = "")

    while(y.anon %in% x.anon){
      y.anon <- paste(sample(LETTERS, size = anon.level), collapse = "")
    }

    lookup.x[place] <- i
    lookup.code[place] <- y.anon
    place <- place + 1

    x.anon[which(x.anon == i)] <- y.anon

  }

  if(return.lookup == T){
    lookup.table <- data.frame(original = lookup.x, code = lookup.code)
    return(list(anon.list = x.anon, anon.lookup = lookup.table))
  } else {
    return(x.anon)
  }


}

#' Return missing column names
#'
#' The check.column.names function helps data checking by returning a list of column names that are missing
#' @param input.frame Data frame to check.
#' @param column.target List of column names that should be in the target data frame.
#' @return List of columns that are missing in the input frame.
#' @export

check.column.names <- function(input.frame, column.target){
  place <- 1
  missing.columns <- array()
  for(i in column.target){
    if(i %in% colnames(input.frame)){
      next
    } else {
      missing.columns[place] <- i
      place <- place + 1
    }
  }

  return(missing.columns)
}

#' Build the simulator ui
#'
#' The simulator.ui function returns the ui object for the app, assuming the user has returned length data. It could be used to customise the tool for individual purposes.
#'
#' @details As the function is built into the surveySimulator() call, some aspects of the function rely on things specified in that function. If you want to edit this for your own purposes, ensure you have checked all of the inputs!
#' @return A shiny ui object.
#' @export

simulator.ui <- function(masterdata){
  ui <- navbarPage(title = "Survey Simulation Tool",
                   theme = shinytheme("flatly"),
                   # The first tab is for setting up the simulation
                   # Two sub tabs provide a) data set up (including data exploration plots and information)
                   # And b) setting up the stations for the simulation
                   # Prior to doing any of this work, the user will have already gone through a data upload process
                   tabPanel(title = "Simulation",
                            tabsetPanel(id = "SimulationSetUp",
                                        tabPanel(title = "Data Set-up",
                                                 #Build the sidebar using the three reactive UI outputs
                                                 sidebarPanel(
                                                   bsCollapse(id = "dataSetUp", multiple = T, open = c("Data restrictions", "Data Exploration Options", "Model Terms"),
                                                              bsCollapsePanel(title = "Data restrictions",
                                                                              checkboxInput(inputId = "anon", label = "Anonymous data?"),
                                                                              #uiOutput("sidebarY"),
                                                                              sliderInput(inputId = "yearres", label = "Year Selection",
                                                                                          min = min(masterdata$Season), max = max(masterdata$Season),
                                                                                          value = c(min(masterdata$Season), max(masterdata$Season)), step = 1, sep = ""),
                                                                              uiOutput("sidebarG"),
                                                                              uiOutput("sidebarV")),
                                                              bsCollapsePanel(title = "Data Exploration Options",
                                                                              uiOutput("datasummaryUI"),
                                                                              uiOutput("pairsUI")),
                                                              bsCollapsePanel(title = "Model Terms",
                                                                              uiOutput("sidebarM"),
                                                                              uiOutput("sidebarFactor"),
                                                                              uiOutput("sidebarNumeric"))
                                                   )
                                                 ),
                                                 mainPanel(
                                                   uiOutput("summaryUI")
                                                 )
                                        ),
                                        tabPanel(title = "Station Set-up",
                                                 sidebarPanel(
                                                   bsCollapse(id = "stationSetUp", multiple = T,
                                                              bsCollapsePanel(title = "Station Details",
                                                                              numericInput(inputId = "buffer", label = "Radius of selection area", min = 10^-4, max = Inf, value = 10),
                                                                              selectInput(inputId = "unit", label = "Unit of radius", choices = c("Nautical Mile", "Kilometre"), selected = "Nautical Mile")
                                                              ),
                                                              bsCollapsePanel(title = "Polygon upload",
                                                                              fileInput(inputId = "stationPoly", label = "Upload Polygons")
                                                              ),
                                                              bsCollapsePanel(title = "Station Generation",
                                                                              numericInput(inputId = "stationDistance", label = "Minimum distance between stations", min = 10^-4, max = Inf, value = 0),
                                                                              numericInput(inputId = "stations", label = "Number of random stations", min = 10, max = 200, value = 30),
                                                                              actionButton(inputId = "random", "Random stations"),
                                                                              br(),
                                                                              fileInput(inputId = "upload", label = "Upload Station Locations", accept = ".csv"),
                                                                              downloadButton(outputId = "downloadpoints", label = "Download Station Locations")
                                                              )
                                                   ),
                                                   actionButton(inputId = "bootstrap", label = "Run the surveys")
                                                 ),
                                                 mainPanel(
                                                   leafletOutput(outputId = "surveymap")
                                                 ))
                            )
                   ),
                   # The second tab provides all the results, divided into three sub sections
                   tabPanel(title = "Results",
                            tabsetPanel(
                              tabPanel(title = "Model results",
                                       sidebarPanel(
                                         selectInput(inputId = "GLMplot", label = "Variable view",
                                                     choices = c("Season", "Month", "Nationality", "Depth Class", "Cetaceans", "Area", "Gear"),
                                                     selected = "Season"),
                                         selectInput(inputId = "modelselect", label = "Model run",
                                                     choices = c(1),
                                                     multiple = F),
                                         uiOutput("modelChooser"),
                                         checkboxInput(inputId = "fullmodel", value = F, label = "View all data model?"),
                                         verbatimTextOutput(outputId = "factorlv")
                                       ),
                                       mainPanel(
                                         plotOutput(outputId = "plotted"),
                                         plotOutput(outputId = "cpueplot"),
                                         verbatimTextOutput(outputId = "GLMsummary")
                                       )),
                              tabPanel(title = "Power Analysis",
                                       sidebarPanel(
                                         selectInput(inputId = "pwrselect", label = "Model run",
                                                     choices = c(1),
                                                     multiple = F),
                                         numericInput(inputId = "pwrReduce", label = "CPUE multiplier", value = 1, min = 10^-4, max = Inf, step = 10^-2),
                                         numericInput(inputId = "pSig", label = "Significance level", value = 0.01, min = 10^-4, max = 1, step = 0.001)
                                       ),
                                       mainPanel(
                                         plotOutput(outputId = "powerPlot"),
                                         verbatimTextOutput(outputId = "powerOut")
                                       )),
                              tabPanel(title = "Length results",
                                       sidebarPanel(
                                         selectInput(inputId = "y", label = "Plot view",
                                                     choices = c("Season", "Month", "Nationality", "Gear"),
                                                     selected = "Gear"),
                                         selectInput(inputId = "lengthselect", label = "Model run", choices = c(1), multiple = F),
                                         downloadButton("DownloadCPUE", "Download Plot")
                                       ),
                                       mainPanel(
                                         plotOutput(outputId = "lengthplot")
                                       )))
                   )
  )
  return(ui)
}


#' Build the simulator ui (no length)
#'
#' The simulator.ui.nolength function returns the ui object for the app, assuming the user has not returned length data. It could be used to customise the tool for individual purposes.
#'
#' @details As the function is built into the surveySimulator() call, some aspects of the function rely on things specified in that function. If you want to edit this for your own purposes, ensure you have checked all of the inputs!
#' @return A shiny ui object.
#' @export

simulator.ui.nolength <- function(masterdata){
  ui <- navbarPage(title = "Survey Simulation Tool",
                   theme = shinytheme("flatly"),
                   # The first tab is for setting up the simulation
                   # Two sub tabs provide a) data set up (including data exploration plots and information)
                   # And b) setting up the stations for the simulation
                   # Prior to doing any of this work, the user will have already gone through a data upload process
                   tabPanel(title = "Simulation",
                            tabsetPanel(id = "SimulationSetUp",
                                        tabPanel(title = "Data Set-up",
                                                 #Build the sidebar using the three reactive UI outputs
                                                 sidebarPanel(
                                                   bsCollapse(id = "dataSetUp", multiple = T, open = c("Data restrictions", "Data Exploration Options", "Model Terms"),
                                                              bsCollapsePanel(title = "Data restrictions",
                                                                              checkboxInput(inputId = "anon", label = "Anonymous data?"),
                                                                              #uiOutput("sidebarY"),
                                                                              sliderInput(inputId = "yearres", label = "Year Selection",
                                                                                          min = min(masterdata$Season), max = max(masterdata$Season),
                                                                                          value = c(min(masterdata$Season), max(masterdata$Season)), step = 1, sep = ""),
                                                                              uiOutput("sidebarG"),
                                                                              uiOutput("sidebarV")),
                                                              bsCollapsePanel(title = "Data Exploration Options",
                                                                              uiOutput("datasummaryUI"),
                                                                              uiOutput("pairsUI")),
                                                              bsCollapsePanel(title = "Model Terms",
                                                                              uiOutput("sidebarM"),
                                                                              uiOutput("sidebarFactor"),
                                                                              uiOutput("sidebarNumeric"))
                                                   )
                                                 ),
                                                 mainPanel(
                                                   uiOutput("summaryUI")
                                                 )
                                        ),
                                        tabPanel(title = "Station Set-up",
                                                 sidebarPanel(
                                                   bsCollapse(id = "stationSetUp", multiple = T,
                                                              bsCollapsePanel(title = "Station Details",
                                                                              numericInput(inputId = "buffer", label = "Radius of selection area", min = 10^-4, max = Inf, value = 10),
                                                                              selectInput(inputId = "unit", label = "Unit of radius", choices = c("Nautical Mile", "Kilometre"), selected = "Nautical Mile")
                                                              ),
                                                              bsCollapsePanel(title = "Polygon upload",
                                                                              fileInput(inputId = "stationPoly", label = "Upload Polygons")
                                                              ),
                                                              bsCollapsePanel(title = "Station Generation",
                                                                              numericInput(inputId = "stationDistance", label = "Minimum distance between stations", min = 10^-4, max = Inf, value = 0),
                                                                              numericInput(inputId = "stations", label = "Number of random stations", min = 10, max = 200, value = 30),
                                                                              actionButton(inputId = "random", "Random stations"),
                                                                              br(),
                                                                              fileInput(inputId = "upload", label = "Upload Station Locations", accept = ".csv"),
                                                                              downloadButton(outputId = "downloadpoints", label = "Download Station Locations")
                                                              )
                                                   ),
                                                   actionButton(inputId = "bootstrap", label = "Run the surveys")
                                                 ),
                                                 mainPanel(
                                                   leafletOutput(outputId = "surveymap")
                                                 ))
                            )
                   ),
                   # The second tab provides all the results, divided into three sub sections
                   tabPanel(title = "Results",
                            tabsetPanel(
                              tabPanel(title = "Model results",
                                       sidebarPanel(
                                         selectInput(inputId = "GLMplot", label = "Variable view",
                                                     choices = c("Season", "Month", "Nationality", "Depth Class", "Cetaceans", "Area", "Gear"),
                                                     selected = "Season"),
                                         selectInput(inputId = "modelselect", label = "Model run",
                                                     choices = c(1),
                                                     multiple = F),
                                         uiOutput("modelChooser"),
                                         checkboxInput(inputId = "fullmodel", value = F, label = "View all data model?"),
                                         downloadButton("DownloadGLM", "Download Plots"),
                                         verbatimTextOutput(outputId = "factorlv")
                                       ),
                                       mainPanel(
                                         plotOutput(outputId = "plotted"),
                                         plotOutput(outputId = "cpueplot"),
                                         verbatimTextOutput(outputId = "GLMsummary")
                                       )),
                              tabPanel(title = "Power Analysis",
                                       sidebarPanel(
                                         selectInput(inputId = "pwrselect", label = "Model run",
                                                     choices = c(1),
                                                     multiple = F),
                                         numericInput(inputId = "pwrReduce", label = "CPUE multiplier", value = 1, min = 10^-4, max = Inf, step = 10^-2),
                                         numericInput(inputId = "pSig", label = "Significance level", value = 0.01, min = 10^-4, max = 1, step = 0.001)
                                       ),
                                       mainPanel(
                                         plotOutput(outputId = "powerPlot"),
                                         verbatimTextOutput(outputId = "powerOut")
                                       )))
                   )
  )
  return(ui)
}


#' Build the simulator server
#'
#' The simulator.server.nolength function returns the server object for the app, assuming the user has not returned length data. It could be used to customise the tool for individual purposes.
#'
#' @details As the function is built into the surveySimulator() call, some aspects of the function rely on things specified in that function. If you want to edit this for your own purposes, ensure you have checked all of the inputs!
#' @return A shiny server object (a function).
#' @export

simulator.server.nolength <- function(){
  server <- function(input, output, session){

    #### Initial parameters ####
    user.polygon <<- 0
    masterdata$cpue <- masterdata$Catch/masterdata$Effort
    remove.masters <- which(colnames(masterdata) %in% c("Catch", "Effort"))
    masterdata <- masterdata[,-c(remove.masters)]

    #### Sidebar set up ####

    ## Store anon data
    anon.pre.vessel <- masterdata$ShipName
    anon.pre.nationality <- masterdata$Nationality

    ## Reactively set up anon data
    anon <- reactive({
      i <- input$anon
      if(i == F){
        masterdata$ShipName <- anon.pre.vessel
        masterdata$Nationality <- anon.pre.nationality
      } else {
        masterdata$ShipName <- anonymise(masterdata$ShipName, 4)
        masterdata$Nationality <- anonymise(masterdata$Nationality, 4)
      }

      return(masterdata)
    })

    ## Set up reactive gear types, to drop/add levels based on year settings
    gearReact <- reactive({
      gears <- unique(anon()$Gear[which(anon()$Season >= input$yearres[1] & anon()$Season <= input$yearres[2])])
      return(sort(gears))
    })

    ## Set up reactive vessels, to drop/add levels based on gear settings
    vesselReact <- reactive({
      setdata <- anon()[which(anon()$Season >= input$yearres[1] & anon()$Season <= input$yearres[2]),]
      vessels <- unique(setdata$ShipName[which(setdata$Gear %in% gearReact())])
      return(sort(vessels))
    })

    modeltermsReact <- reactive({
      colnames.temp <- colnames(masterdata)
      colnames.return <- colnames.temp[which(colnames.temp != "ID" & colnames.temp != "Species" & colnames.temp != "Latitude" & colnames.temp != "Longitude" & colnames.temp != "cpue")]

      return(colnames.return)
    })

    output$modelChooser <- renderUI({
      selectInput(inputId = "modelChoice", label = "Model terms",
                  choices = modeltermsReact(),
                  multiple = T, selected = modeltermsReact())
    })

    modelclassReact <- reactive({

    })


    factorReact <- reactive({
      total.list <- modeltermsReact()

      #total.list <- total.list[which(!(total.list %in% input$numericlist))]

      return(selectInput(inputId = "factorlist", label = "variables to use as factors:", choices = total.list, selected = total.list, multiple = T))
    })

    output$sidebarFactor <- renderUI({
      factorReact()
    })

    output$sidebarNumeric <- renderUI({
      numericReact()
    })

    output$sidebarM <- renderUI({
      checkModels <-
        list(h3("Model terms"),
             tags$div(align = 'left',
                      class = 'col',
                      checkboxGroupInput(inputId = 'modeltermSelect',
                                         label = NULL,
                                         choices = modeltermsReact(),
                                         selected = modeltermsReact(),
                                         inline = F)))
    })

    modelSetup <- reactive({
      temp <- input$modeltermSelect
      return(temp)
    })

    ## Vessel sidebar options
    output$sidebarV <- renderUI({

      checkVessel <-
        list(h3("Vessel Selection"),
             tags$div(align = 'left',
                      class = 'multicol',
                      checkboxGroupInput(inputId  = 'vesselselect',
                                         label = NULL,
                                         choices  = vesselReact(),
                                         selected = vesselReact(),
                                         inline   = FALSE)))
    })

    ## Gear sidebar options
    output$sidebarG <- renderUI({


      checkGear <-
        list(h3("Gear Selection"),
             tags$div(align = 'left',
                      class = 'multicol',
                      checkboxGroupInput(inputId  = 'gearselect',
                                         label = NULL,
                                         choices  = gearReact(),
                                         selected = gearReact(),
                                         inline   = FALSE)))




    })

    ## Data summary reactive
    output$datasummaryUI <- renderUI({
      selectInput(inputId = "summarytype", label = "Plot view", multiple = F,
                  selected = "Pairs", choices = c("Pairs", names(inputdata())))
    })

    pairsUIreact <- reactive({

      if(input$summarytype == "Pairs"){
        return(
          tagList(
            selectInput(inputId = "pairplot",
                        label = "Columns to include in pairs plot",
                        choices = names(inputdata()), multiple = T, selected = names(inputdata())[3:6])
          )
        )
      } else {
        return(
          tagList(
            br()
          )
        )
      }


    })

    output$pairsUI <- renderUI({

      pairsUIreact()

    })



    #### Reactive data set up ####


    ## Take the catch/effort data and filter it based on user inputs
    inputdata <- reactive({
      temp.data <- anon() %>%
        filter(., ShipName %in% input$vesselselect) %>%
        filter(., Gear%in% input$gearselect)

      for(i in 1:ncol(temp.data)){
        if(colnames(temp.data)[i] %in% input$factorlist){
          temp.data[,i] <- as.factor(temp.data[,i])
        }
      }

      return(temp.data)
    })

    ## Create a spatial copy of the effort data for station distribution
    r.data <- reactive({
      effort.t <- masterdata[which(masterdata$ID %in% inputdata()$ID),]
      r.data.t <- data.frame(effort.t$Latitude, effort.t$Longitude)

      colnames(r.data.t) <- c("y", "x")
      coordinates(r.data.t) <- ~x + y

      return(remove.duplicates(r.data.t))
    })




    #### Station generation ####

    ## Random station generation is based on data distribution
    # Change to incorporate CCAMLRGIS
    observeEvent(input$random, {

      # Build a polygon that surrounds the data completely
      # polygon <- r.data() %>%
      #   st_as_sf(coords = c("X", "Y"), crs = st_crs(SmallBathy)) %>%
      #   summarise(geometry = st_union(geometry)) %>%
      #   st_convex_hull() %>%
      #   st_transform(crs = st_crs(SmallBathy))

      # Or, just a user uploaded polygon(s)

      if(user.polygon == 1){
        polygon <- user.polygon.upload
      }

      # Randomly generate points within the polygon(s)

      ## CCAMLRGIS method
      ## Needs fixing - MKERR
      ## points <<- create_Stations(Poly = polygon,N = input$stations, dist = input$stationDistance, Bathy = SmallBathy, Depths = input$stationDepth)

      ## sf and rgeos method



      withProgress({

        incProgress(0.1)
        orig.buffer <- rgeos::gBuffer(spgeom = r.data(), byid = T, width = 0.1)
        incProgress(0.1)
        union.buffer <- rgeos::gUnaryUnion(orig.buffer)

        polygon <- st_as_sf(union.buffer)

        station.points <<- matrix()

        buffer <- input$stationDistance

        while(nrow(station.points) < input$stations){
          incProgress(amount = 0.8/input$stations)
          temp.row <- sf::st_sample(polygon, size = 1) %>%
            sf::st_coordinates() %>% as.data.frame()

          if(ncol(station.points) == 1){
            station.points <<- temp.row
          } else {

            station.points.dist <- spDistsN1(as.matrix(station.points),
                                             as.numeric(temp.row), longlat = T)

            while(min(station.points.dist) < buffer){
              temp.row <- sf::st_sample(polygon, size = 1) %>%
                sf::st_coordinates() %>% as.data.frame()

              station.points.dist <- spDistsN1(as.matrix(station.points),
                                               as.numeric(temp.row), longlat = T)
            }

            station.points <<- rbind(station.points, temp.row)

          }


        }
      }, message = "Generating stations")


      output$surveymap <- renderLeaflet({
        leaflet() %>%
          addTiles() %>%
          #addProviderTiles(provider = "Esri.WorldPhysical") %>%
          addCircles(lng = station.points[,1], lat = station.points[,2]) %>%
          fitBounds( lng1 = min(masterdata$Longitude)
                     , lat1 = min(masterdata$Latitude)
                     , lng2 = max(masterdata$Longitude)
                     , lat2 = max(masterdata$Latitude) ) %>%
          addDrawToolbar(
            targetGroup='User layer',
            polylineOptions=FALSE,
            markerOptions = TRUE,
            circleOptions = TRUE, editOptions = editToolbarOptions())
      })

      randomStations <<- 1

    })

    ## Uploaded stations are inserted onto the map and stored
    # This will overwrite random stations (!!!)
    observeEvent(input$upload, {
      file <- input$upload
      point.read <- read.csv(file = file$datapath)

      station.points <<- data.frame(x = point.read$X, y = point.read$Y)

      output$surveymap <- renderLeaflet({
        leaflet() %>%
          addTiles() %>%
          #addProviderTiles(provider = "Esri.WorldImagery") %>%
          addCircles(lng = station.points[,1], lat = station.points[,2]) %>%
          fitBounds( lng1 = min(masterdata$Longitude)
                     , lat1 = min(masterdata$Latitude)
                     , lng2 = max(masterdata$Longitude)
                     , lat2 = max(masterdata$Latitude) ) %>%
          addDrawToolbar(
            targetGroup='User layer',
            polylineOptions=FALSE,
            markerOptions = TRUE,
            circleOptions = TRUE, editOptions = editToolbarOptions())
      })

    })

    #### Running the simulation ####
    observeEvent(input$bootstrap, {


      sub.data <<- inputdata()

      updateChoices <- input$modeltermSelect[which(input$modeltermSelect != "cpue")]

      updateSelectInput(inputId = "modelChoice", label = "Model terms",
                        choices = updateChoices, selected = updateChoices)
      n <- 10##############
      updateSelectInput(inputId = "modelselect", choices = c(1:n))
      updateSelectInput(inputId = "lengthselect", choices = c(1:n))
      updateSelectInput(inputId = "cpuechoice", choices = c(1:n))

      # set up loop to run each simulation
      withProgress(message = "Running surveys", expr = {
        for(i in 1:n){


          incProgress(amount = 1/n)
          trial_data <- sub.data

          trial_data$rowid <- as.numeric(1:nrow(trial_data))

          # for each year, randomly retrieve one data point per station
          draw <- array()
          place <- 1
          place2 <- 1

          for(j in unique(trial_data$Season)){

            # get a subset of potential points
            filter.data <- trial_data[which(trial_data$Season == j),]

            # loop to get random draw per station
            for(k in 1:nrow(station.points)){

              dist1 <- spDistsN1(cbind(filter.data$Longitude, filter.data$Latitude),
                                 c(station.points[k,1],
                                   station.points[k,2]), longlat = T)

              dist2 <- spDistsN1(cbind(filter.data$Longitude, filter.data$Latitude),
                                 c(station.points[k,1],
                                   station.points[k,2]), longlat = T)

              if(input$unit == "Nautical Mile"){
                buff <- input$buffer * 1.852
              } else {
                buff <- input$buffer
              }


              buffed1 <- which(dist1 <= buff)
              buffed2 <- which(dist2 <= buff)

              bufferzone <- unique(c(buffed1, buffed2))


              station.data <- filter.data[bufferzone,]


              if(nrow(station.data) > 1){
                for(x in 1:2){
                  draw[place] <- sample(station.data$rowid, size = 1, replace = T)
                  place <- place + 1
                }
              }

            }

          }



          # set up the simulated data
          simul_data <- trial_data[which(trial_data$rowid %in% draw),]

          for(i in 1:ncol(simul_data)){
            if(colnames(simul_data)[i] %in% input$factorlist){
              simul_data[,i] <- as.factor(simul_data[,i])
            }
          }



          #colnames(simul_data)[c(3,4,5,6,15,12,14)] <- c ("gear","season", "month", "nationality", "depth.class", "cetaceans", "area.n")

          simul_data$Season <- factor(simul_data$Season)
          simul_data$Gear <- factor(simul_data$Gear)
          #simul_data$Month <- factor(simul_data$Month)
          simul_data$Nationality <- factor(simul_data$Nationality)
          #simul_data$depth.class <- factor(simul_data$depth.class)
          #simul_data$cetaceans <- factor(simul_data$cetaceans)
          #simul_data$area.n <- factor(simul_data$area.n)

          if(i == 1){
            simul_ID <- list(unique(simul_data$ID))
          } else {
            simul_ID <- list(simul_ID, list(unique(simul_data$ID)))
          }

          simul_data <<- simul_data


          # weird duplicate bug fix
          df <- table(simul_data$Season)
          for(i1 in 1:length(df)){
            if(df[i1] > dim(station.points)[1]){
              j1 <- df[i1] - dim(station.points)[1]
              k1 <- sample(1:(j1+1), j1)

              removedf <- simul_data[which(simul_data$season == names(df)[i1]),]
              removedr <- removedf$rowid[k1]

              simul_data <- simul_data[which(!(simul_data$rowid %in% removedr)),]
            }
          }

          for(ikk in modelSetup()){

            if(ikk == modelSetup()[1]){
              model.base <- ikk
            } else {
              model.base <- paste(model.base, ikk, sep = "+")
            }

          }

          model.base <- paste("cpue", "~", model.base, sep = "")

          #simul_model <- glm(as.formula(model.base), family=gaussian(link="sqrt"),
          #                   na.action=na.omit, data = simul_data, subset=cpue>0, maxit = 100)

          simul_model <- glm(as.formula(model.base), family=gaussian(link="sqrt"),
                             na.action=na.omit, data = simul_data, maxit = 100)

          if(i == 1){
            modelruns <- list(simul_model)
            #predictruns <- list(No3)
          } else {
            modelruns[[i]] <- simul_model
            #predictruns[[i]] <- No3
          }

        }

        modelruns <<- modelruns
        #predictruns <<- predictruns
      })

    })


    #### Generating models ####
    model.terms <- reactive({

      for(i in (input$modelChoice)){
        if(!(i %in% c("Catch", "Effort"))){
          if(i == input$modelChoice[1]){
            model.t <<- i
          } else {
            model.t <<- paste(model.t, i, sep = "+")
          }
        }
      }


      return(paste("cpue", " ~ ", model.t))
    })

    GLM_run <- reactive({
      tryme <- input$bootstrap
      run_data <- modelruns[[GLM_choice()]]$data

      for(i in 1:ncol(run_data)){
        if(colnames(run_data)[i] %in% input$factorlist){
          run_data[,i] <- as.factor(run_data[,i])
        }
      }

      #m <- glm(as.formula(model.terms()), family=gaussian(link="sqrt"),
      #         na.action=na.omit, data = run_data, subset=cpue>0, maxit = 100)

      m <- glm(as.formula(model.terms()), family=gaussian(link="sqrt"),
               na.action=na.omit, data = run_data, maxit = 100)

      return(m)
    })

    predict_run<- reactive({

      place <- GLM_choice()


      for(i in 1:length(modelruns)){



        simul_data <- modelruns[[i]]$data

        for(i6 in 1:ncol(simul_data)){
          if(colnames(simul_data)[i6] %in% input$factorlist){
            simul_data[,i6] <- as.factor(simul_data[,i6])
          }
        }

        m <- glm(as.formula(model.terms()), family=gaussian(link="sqrt"),
                 na.action=na.omit, data = simul_data, subset=cpue>0, maxit = 100)

        if(is.null(simul_data)){
          next
        }

        nseasons<-length(levels(simul_data$Season))
        seasons<-levels(simul_data$Season)
        #seasons<-c("2003","2004","2005","2006","2007","2008","2009","2010","2011")
        #nseasons<-length(seasons)

        ## WITHOUT WHALES
        # build predict frame
        predict.matrix.build <- data.frame(n = nseasons)
        for(i5 in input$modelChoice){
          place <- which(input$modelChoice == i5)

          if(i5 == "Season"){
            predict.matrix.build <- cbind(predict.matrix.build, as.factor(seasons))
          } else {
            predict.matrix.build <- cbind(predict.matrix.build, rep(simul_data[1,i5],nseasons))
          }

        }

        predict.matrix.build <- predict.matrix.build[,-c(which(colnames(predict.matrix.build) == "n"))]

        colnames(predict.matrix.build) <- input$modelChoice
        GLM483.predict.frame <- predict.matrix.build


        for(i4 in 1:ncol(GLM483.predict.frame)){
          GLM483.predict.frame[,i4] <- as.factor(GLM483.predict.frame[,i4])
        }

        GLM.pred.rev.3 <- predict(m, newdata=GLM483.predict.frame, type="response", se.fit=T)



        No3 <- data.frame(season=seasons)
        No3$estimate <- GLM.pred.rev.3$fit#*positive.proportion[c((length(positive.proportion)-8):(length(positive.proportion)))]
        No3$se <- GLM.pred.rev.3$se.fit
        No3$lower95 <- (GLM.pred.rev.3$fit-2*GLM.pred.rev.3$se.fit) #*positive.proportion[c((length(positive.proportion)-8):(length(positive.proportion)))]
        No3$upper95 <- (GLM.pred.rev.3$fit+2*GLM.pred.rev.3$se.fit) #*positive.proportion[c((length(positive.proportion)-8):(length(positive.proportion)))]
        No3$index <- No3$estimate*1000
        No3$cv    <- No3$se/No3$estimate

        op1 <- No3[,c(1,6)]
        op2 <- No3[,c(1,7)]
        op2$season <- paste0("cv_",op2$season)

        if(i == 1){
          predictruns1 <<- data.frame(No3, group = i)
        } else {
          predictruns1 <<- rbind(predictruns1, data.frame(No3, group = i))
        }

      }
      return(predictruns1)

    })



    #### Output global reactive statements ####


    GLM_choice <- reactive({
      i <- as.numeric(input$modelselect)
      return(i)
    })

    PWR_choice <- reactive({
      i <- as.numeric(input$pwrselect)
      return(i)
    })

    choiceID <- reactive({
      i <- as.numeric(input$lengthselect)

      return(simul_ID[[i]])
    })



    #### Outputs ####

    ## Reactive data summary plotting

    pairsChoice <- reactive({
      columnChoice <- input$pairplot
      return(which(names(inputdata()) %in% columnChoice))
    })

    histChoice <- reactive({
      return(which(names(inputdata()) == input$summarytype))
    })

    # Build reactive ui placement based on plot choice
    summaryplot <- reactive({
      if(input$summarytype == "Pairs"){
        return(
          tagList(
            plotOutput(outputId = "datasummaryPairs")
          )
        )
      } else {
        return(
          tagList(
            plotOutput(outputId = "datasummary")
          )
        )
      }
    })

    output$summaryUI <- renderUI({
      summaryplot()
    })

    # Here are the plots
    output$datasummary <- renderPlot({
      qplot(x = inputdata()[,histChoice()]) +
        theme_bw() + xlab(input$summarytype)
    })

    output$datasummaryPairs <- renderPlot({
      ggpairs(inputdata(), columns = pairsChoice(), cardinality_threshold = 20, bins = 30) + theme_bw()
    }, height = 1000)

    ## Survey map
    output$surveymap <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        #addProviderTiles(provider = "Esri.WorldPhysical") %>%
        fitBounds( lng1 = min(masterdata$Longitude)
                   , lat1 = min(masterdata$Latitude)
                   , lng2 = max(masterdata$Longitude)
                   , lat2 = max(masterdata$Latitude) ) %>%
        addDrawToolbar(
          targetGroup='User layer',
          polylineOptions=FALSE,
          markerOptions = TRUE,
          circleOptions = TRUE, editOptions = editToolbarOptions())
    })



    ## GLM effects plot

    GLM_run <- reactive({
      run_data <- modelruns[[GLM_choice()]]$data

      for(i in 1:ncol(run_data)){
        if(colnames(run_data)[i] %in% input$factorlist){
          run_data[,i] <- as.factor(run_data[,i])
        }
      }

      #m <- glm(as.formula(model.terms()), family=gaussian(link="sqrt"),
      #         na.action=na.omit, data = run_data, subset=cpue>0, maxit = 100)

      m <- glm(as.formula(model.terms()), family=gaussian(link="sqrt"),
               na.action=na.omit, data = run_data, maxit = 100)

      return(m)
    })

    full_model <- reactive({
      full_data <- inputdata()

      for(i in 1:ncol(full_data)){
        if(colnames(full_data)[i] %in% input$factorlist){
          full_data[,i] <- as.factor(full_data[,i])
        }
      }


      full.glm <<- glm(formula = as.formula(model.terms()), family=gaussian(link="sqrt"),
                       na.action=na.omit, data = full_data, subset=cpue>0, maxit = 100)

      return(full.glm)
    })

    full_predict <- reactive({

      m <- full_model()
      full_data <- inputdata()

      for(i in 1:ncol(full_data)){
        if(colnames(full_data)[i] %in% input$factorlist){
          full_data[,i] <- as.factor(full_data[,i])
        }
      }

      nseasons<-length(levels(full_data$Season))
      seasons<-levels(full_data$Season)
      predict.matrix.build <- data.frame(n = nseasons)
      for(i in input$modelChoice){
        place <- which(input$modelChoice == i)

        if(i == "Season"){
          predict.matrix.build <- cbind(predict.matrix.build, as.factor(seasons))
        } else {
          predict.matrix.build <- cbind(predict.matrix.build, rep(full_data[1,i],nseasons))
        }

      }

      predict.matrix.build <- predict.matrix.build[,-c(which(colnames(predict.matrix.build) == "n"))]

      colnames(predict.matrix.build) <- input$modelChoice
      GLM483.predict.frame <- predict.matrix.build


      for(i in 1:ncol(GLM483.predict.frame)){
        GLM483.predict.frame[,i] <- as.factor(GLM483.predict.frame[,i])
      }



      GLM.pred.rev.3 <- predict(m, newdata=GLM483.predict.frame, type="response", se.fit=T)



      No3 <- data.frame(season=seasons)
      No3$estimate <- GLM.pred.rev.3$fit#*positive.proportion[c((length(positive.proportion)-8):(length(positive.proportion)))]
      No3$se <- GLM.pred.rev.3$se.fit
      No3$lower95 <- (GLM.pred.rev.3$fit-2*GLM.pred.rev.3$se.fit) #*positive.proportion[c((length(positive.proportion)-8):(length(positive.proportion)))]
      No3$upper95 <- (GLM.pred.rev.3$fit+2*GLM.pred.rev.3$se.fit) #*positive.proportion[c((length(positive.proportion)-8):(length(positive.proportion)))]
      No3$index <- No3$estimate*1000
      No3$cv    <- No3$se/No3$estimate

      return(No3)

    })

    GLM_plottype <- reactive({
      if(input$GLMplot == "Depth Class"){
        "depth.class"
      } else if(input$GLMplot == "Area"){
        "area.n"
      } else {
        (input$GLMplot)
      }

    })

    plot_react <- reactive({

      if(input$fullmodel == T){

        x <- plot.Gam(full_model(),se=TRUE, terms = GLM_plottype())

        y <- plot.Gam(GLM_run(),se=TRUE, terms = GLM_plottype())

        place <- 0
        xar <- array()
        yar <- array()
        sear <- array()
        model <- array()
        rug <- array()

        for(j in unique(x$preplot[[1]]$x)){

          place <- place + 1

          xar[place] <- j
          yar[place] <- mean(x$preplot[[1]]$y[which(x$preplot[[1]]$x == j)])
          sear[place] <- mean(x$preplot[[1]]$se.y[which(x$preplot[[1]]$x == j)])
          model[place] <- "full"

        }

        for(j in unique(y$preplot[[1]]$x)){

          place <- place + 1

          xar[place] <- j
          yar[place] <- mean(y$preplot[[1]]$y[which(y$preplot[[1]]$x == j)])
          sear[place] <- mean(y$preplot[[1]]$se.y[which(y$preplot[[1]]$x == j)])
          model[place] <- "current"


        }

        rug <- c(table(x$preplot$season$x), table(y$preplot$season$x))

        yup <- yar + sear

        ydown <- yar - sear

        test <<- data.frame(xar = xar, yar = yar, yup = yup, ydown = ydown, model = model, sear = sear)

        test.plot <- ggplot(data = test) + theme_bw() +
          geom_point(aes(x = xar, y = yar), alpha = 0.5) +
          geom_pointrange(aes(x = xar, y = yar, ymin = ydown, ymax = yup, xmin = xar, xmax = xar, col = model, size = model), alpha = 0.5) +
          scale_size_manual(values = c(1, 0.5)) + scale_colour_manual(values = c("red", "black")) +
          xlab(paste(GLM_plottype())) + ylab(paste("Partial effect for ", GLM_plottype(), sep = ""))

        return(test.plot)

      } else {


        y <- plot.Gam(GLM_run(),se=TRUE, terms = GLM_plottype())

        place <- 0
        xar <- array()
        yar <- array()
        sear <- array()
        model <- array()
        rug <- array()


        for(j in unique(y$preplot[[1]]$x)){

          place <- place + 1

          xar[place] <- j
          yar[place] <- mean(y$preplot[[1]]$y[which(y$preplot[[1]]$x == j)])
          sear[place] <- mean(y$preplot[[1]]$se.y[which(y$preplot[[1]]$x == j)])
          model[place] <- "current"


        }

        rug <- c(table(y$preplot[[1]]$x))

        yup <- yar + sear

        ydown <- yar - sear

        test <<- data.frame(xar = xar, yar = yar, yup = yup, ydown = ydown, model = model, sear = sear)

        test.plot <- ggplot(data = test) + theme_bw() +
          geom_point(aes(x = xar, y = yar)) +
          geom_pointrange(aes(x = xar, y = yar, ymin = ydown, ymax = yup, xmin = xar, xmax = xar, col = model), show.legend = F) +
          scale_size_manual(values = c(1)) + scale_colour_manual(values = c("black")) +
          xlab(paste(GLM_plottype())) + ylab(paste("Partial effect for ", GLM_plottype(), sep = ""))

        return(test.plot)

      }

    })

    output$plotted <- renderPlot({
      plot_react()
    })

    ## GLM summary
    output$GLMsummary <- renderPrint({
      summary(GLM_run())
    })

    predict_graphing <- reactive({
      df <- predict_run()
      df$group <- as.factor(df$group)

      df$alpha <- 0.1
      df$size <- 0.15
      df$filled <- NA

      df$alpha[which(df$group == GLM_choice())] <- 0.4
      df$size[which(df$group == GLM_choice())] <- 1
      df$filled[which(df$group == GLM_choice())] <- GLM_choice()

      return(df)

    })

    ## CPUE plot
    cpueplot.type <- reactive({
      if(input$fullmodel == T){

        ggplot(subset(predict_graphing(), season != "2003")) +
          geom_line(aes(x=as.numeric(as.character(season)), y=estimate, group = group, col = group, size = size), show.legend = F) + scale_size_continuous(range = c(0.15, 1)) +
          geom_ribbon(aes(x=as.numeric(as.character(season)), ymax=upper95, ymin = lower95, group = group, fill = filled, alpha = alpha), linetype=2, show.legend = F) +
          scale_y_continuous(limits=c(0,0.5)) +labs(x="Season", y="Standardised CPUE") + theme_bw() + scale_alpha_continuous(range = c(0.1, 0.4)) +
          geom_ribbon(aes(x=as.numeric(as.character(season)), ymax=upper95, ymin = lower95), data = subset(full_predict(), season != "2003"), alpha = 0.15, fill = "black", show.legend = F) +
          geom_line(aes(x=as.numeric(as.character(season)), y=estimate), size = 1.25, col = "black", data = subset(full_predict(), season != "2003"))


      } else {

        ggplot(subset(predict_graphing(), season != "2003")) +
          geom_line(aes(x=as.numeric(as.character(season)), y=estimate, group = group, col = group, size = size), show.legend = F) + scale_size_continuous(range = c(0.15, 1)) +
          geom_ribbon(aes(x=as.numeric(as.character(season)), ymax=upper95, ymin = lower95, group = group, fill = filled, alpha = alpha), linetype=2, show.legend = F) +
          scale_y_continuous(limits=c(0,0.5)) +labs(x="Season", y="Standardised CPUE") + theme_bw() + scale_alpha_continuous(range = c(0.1, 0.4))

      }
    })

    output$cpueplot <- renderPlot({
      cpueplot.type()
    })

    ## Power Analysis
    powerInput <- reactive({
      powerChoice <- input$pwrReduce
      return(powerChoice)
    })

    output$powerPlot <- renderPlot({
      ggplot() +
        geom_density(aes(x = modelruns[[PWR_choice()]]$data$cpue), fill = "red", alpha = 0.5) +
        geom_density(aes(x = modelruns[[PWR_choice()]]$data$cpue * powerInput()), fill = "blue", alpha = 0.5) +
        theme_bw() + xlab("log(CPUE)")
    })

    powerSample <- reactive({
      nrow(modelruns[[PWR_choice()]]$data)

      return(nrow(modelruns[[PWR_choice()]]$data))
    })

    powerD <- reactive({
      mean.dff <- abs(mean(log(modelruns[[PWR_choice()]]$data$cpue), na.rm = T) - mean(log(modelruns[[PWR_choice()]]$data$cpue) * powerInput(), na.rm = T))
      sd.pool <- sd(c(log(modelruns[[PWR_choice()]]$data$cpue), log(modelruns[[PWR_choice()]]$data$cpue) * powerInput()), na.rm = T)

      return(mean.dff/sd.pool)
    })

    powerSig <- reactive({
      siglevel <- input$pSig

      return(siglevel)
    })

    output$powerOut <- renderPrint({
      pwr.t.test(
        n = powerSample(),
        d = powerD(),
        sig.level = powerSig(),power = NULL)
    })
  }

  return(server)
}



#' Build the simulator server
#'
#' The simulator.server function returns the server object for the app, assuming the user has returned length data. It could be used to customise the tool for individual purposes.
#'
#' @details As the function is built into the surveySimulator() call, some aspects of the function rely on things specified in that function. If you want to edit this for your own purposes, ensure you have checked all of the inputs!
#' @return A shiny server object (a function).
#' @export

simulator.server <- function(){
  server <- function(input, output, session){

    #### Initial parameters ####
    user.polygon <<- 0
    masterdata$cpue <- masterdata$Catch/masterdata$Effort
    remove.masters <- which(colnames(masterdata) %in% c("Catch", "Effort"))
    masterdata <- masterdata[,-c(remove.masters)]

    #### Sidebar set up ####

    ## Store anon data
    anon.pre.vessel <- masterdata$ShipName
    anon.pre.nationality <- masterdata$Nationality

    ## Reactively set up anon data
    anon <- reactive({
      i <- input$anon
      if(i == F){
        masterdata$ShipName <- anon.pre.vessel
        masterdata$Nationality <- anon.pre.nationality
      } else {
        masterdata$ShipName <- anonymise(masterdata$ShipName, 4)
        masterdata$Nationality <- anonymise(masterdata$Nationality, 4)
      }

      return(masterdata)
    })

    ## Set up reactive gear types, to drop/add levels based on year settings
    gearReact <- reactive({
      gears <- unique(anon()$Gear[which(anon()$Season >= input$yearres[1] & anon()$Season <= input$yearres[2])])
      return(sort(gears))
    })

    ## Set up reactive vessels, to drop/add levels based on gear settings
    vesselReact <- reactive({
      setdata <- anon()[which(anon()$Season >= input$yearres[1] & anon()$Season <= input$yearres[2]),]
      vessels <- unique(setdata$ShipName[which(setdata$Gear %in% gearReact())])
      return(sort(vessels))
    })

    modeltermsReact <- reactive({
      colnames.temp <- colnames(masterdata)
      colnames.return <- colnames.temp[which(colnames.temp != "ID" & colnames.temp != "Species" & colnames.temp != "Latitude" & colnames.temp != "Longitude" & colnames.temp != "cpue")]

      return(colnames.return)
    })

    output$modelChooser <- renderUI({
      selectInput(inputId = "modelChoice", label = "Model terms",
                  choices = modeltermsReact(),
                  multiple = T, selected = modeltermsReact())
    })

    modelclassReact <- reactive({

    })


    factorReact <- reactive({
      total.list <- modeltermsReact()

      #total.list <- total.list[which(!(total.list %in% input$numericlist))]

      return(selectInput(inputId = "factorlist", label = "variables to use as factors:", choices = total.list, selected = total.list, multiple = T))
    })

    output$sidebarFactor <- renderUI({
      factorReact()
    })

    output$sidebarNumeric <- renderUI({
      numericReact()
    })

    output$sidebarM <- renderUI({
      checkModels <-
        list(h3("Model terms"),
             tags$div(align = 'left',
                      class = 'col',
                      checkboxGroupInput(inputId = 'modeltermSelect',
                                         label = NULL,
                                         choices = modeltermsReact(),
                                         selected = modeltermsReact(),
                                         inline = F)))
    })

    modelSetup <- reactive({
      temp <- input$modeltermSelect
      return(temp)
    })

    ## Vessel sidebar options
    output$sidebarV <- renderUI({

      checkVessel <-
        list(h3("Vessel Selection"),
             tags$div(align = 'left',
                      class = 'multicol',
                      checkboxGroupInput(inputId  = 'vesselselect',
                                         label = NULL,
                                         choices  = vesselReact(),
                                         selected = vesselReact(),
                                         inline   = FALSE)))
    })

    ## Gear sidebar options
    output$sidebarG <- renderUI({


      checkGear <-
        list(h3("Gear Selection"),
             tags$div(align = 'left',
                      class = 'multicol',
                      checkboxGroupInput(inputId  = 'gearselect',
                                         label = NULL,
                                         choices  = gearReact(),
                                         selected = gearReact(),
                                         inline   = FALSE)))




    })

    ## Data summary reactive
    output$datasummaryUI <- renderUI({
      selectInput(inputId = "summarytype", label = "Plot view", multiple = F,
                  selected = "Pairs", choices = c("Pairs", names(inputdata())))
    })

    pairsUIreact <- reactive({

      if(input$summarytype == "Pairs"){
        return(
          tagList(
            selectInput(inputId = "pairplot",
                        label = "Columns to include in pairs plot",
                        choices = names(inputdata()), multiple = T, selected = names(inputdata())[3:6])
          )
        )
      } else {
        return(
          tagList(
            br()
          )
        )
      }


    })

    output$pairsUI <- renderUI({

      pairsUIreact()

    })



    #### Reactive data set up ####


    ## Take the catch/effort data and filter it based on user inputs
    inputdata <- reactive({
      temp.data <- anon() %>%
        filter(., ShipName %in% input$vesselselect) %>%
        filter(., Gear%in% input$gearselect)

      for(i in 1:ncol(temp.data)){
        if(colnames(temp.data)[i] %in% input$factorlist){
          temp.data[,i] <- as.factor(temp.data[,i])
        }
      }

      return(temp.data)
    })

    ## Take the length data and filter it based on user inputs
    inputlength <- reactive({
      temp.data <- Tlength %>%
        filter(., ShipName %in% input$vesselselect) %>%
        filter(., Gear %in% input$gearselect)

      return(Tlength)
    })

    ## Create a spatial copy of the effort data for station distribution
    r.data <- reactive({
      effort.t <- masterdata[which(masterdata$ID %in% inputdata()$ID),]
      r.data.t <- data.frame(effort.t$Latitude, effort.t$Longitude)

      colnames(r.data.t) <- c("y", "x")
      coordinates(r.data.t) <- ~x + y

      return(remove.duplicates(r.data.t))
    })

    r.length <- reactive({
      length.t <- data.frame(Tlength$Latitude, Tlength$Longitude)

      colnames(length.t) <- c("y", "x")

      length.t <- length.t[which(!is.na(length.t$y)),]

      coordinates(length.t) <- ~x + y

      return(remove.duplicates(length.t))
    })


    #### Station generation ####

    ## Random station generation is based on data distribution
    # Change to incorporate CCAMLRGIS
    observeEvent(input$random, {

      # Build a polygon that surrounds the data completely
      # polygon <- r.data() %>%
      #   st_as_sf(coords = c("X", "Y"), crs = st_crs(SmallBathy)) %>%
      #   summarise(geometry = st_union(geometry)) %>%
      #   st_convex_hull() %>%
      #   st_transform(crs = st_crs(SmallBathy))

      # Or, just a user uploaded polygon(s)

      if(user.polygon == 1){
        polygon <- user.polygon.upload
      }

      # Randomly generate points within the polygon(s)

      ## CCAMLRGIS method
      ## Needs fixing - MKERR
      ## points <<- create_Stations(Poly = polygon,N = input$stations, dist = input$stationDistance, Bathy = SmallBathy, Depths = input$stationDepth)

      ## sf and rgeos method



      withProgress({

        incProgress(0.1)
        orig.buffer <- rgeos::gBuffer(spgeom = r.data(), byid = T, width = 0.1)
        incProgress(0.1)
        union.buffer <- rgeos::gUnaryUnion(orig.buffer)

        polygon <- st_as_sf(union.buffer)

        station.points <<- matrix()

        buffer <- input$stationDistance

        while(nrow(station.points) < input$stations){
          incProgress(amount = 0.8/input$stations)
          temp.row <- sf::st_sample(polygon, size = 1) %>%
            sf::st_coordinates() %>% as.data.frame()

          if(ncol(station.points) == 1){
            station.points <<- temp.row
          } else {

            station.points.dist <- spDistsN1(as.matrix(station.points),
                                             as.numeric(temp.row), longlat = T)

            while(min(station.points.dist) < buffer){
              temp.row <- sf::st_sample(polygon, size = 1) %>%
                sf::st_coordinates() %>% as.data.frame()

              station.points.dist <- spDistsN1(as.matrix(station.points),
                                               as.numeric(temp.row), longlat = T)
            }

            station.points <<- rbind(station.points, temp.row)

          }


        }
      }, message = "Generating stations")


      output$surveymap <- renderLeaflet({
        leaflet() %>%
          addTiles() %>%
          #addProviderTiles(provider = "Esri.WorldPhysical") %>%
          addCircles(lng = station.points[,1], lat = station.points[,2]) %>%
          fitBounds( lng1 = min(masterdata$Longitude)
                     , lat1 = min(masterdata$Latitude)
                     , lng2 = max(masterdata$Longitude)
                     , lat2 = max(masterdata$Latitude) ) %>%
          addDrawToolbar(
            targetGroup='User layer',
            polylineOptions=FALSE,
            markerOptions = TRUE,
            circleOptions = TRUE, editOptions = editToolbarOptions())
      })

      randomStations <<- 1

    })

    ## Uploaded stations are inserted onto the map and stored
    # This will overwrite random stations (!!!)
    observeEvent(input$upload, {
      file <- input$upload
      point.read <- read.csv(file = file$datapath)

      station.points <<- data.frame(x = point.read$X, y = point.read$Y)

      output$surveymap <- renderLeaflet({
        leaflet() %>%
          addTiles() %>%
          #addProviderTiles(provider = "Esri.WorldImagery") %>%
          addCircles(lng = station.points[,1], lat = station.points[,2]) %>%
          fitBounds( lng1 = min(masterdata$Longitude)
                     , lat1 = min(masterdata$Latitude)
                     , lng2 = max(masterdata$Longitude)
                     , lat2 = max(masterdata$Latitude) ) %>%
          addDrawToolbar(
            targetGroup='User layer',
            polylineOptions=FALSE,
            markerOptions = TRUE,
            circleOptions = TRUE, editOptions = editToolbarOptions())
      })

    })

    #### Running the simulation ####
    observeEvent(input$bootstrap, {


      sub.data <<- inputdata()
      sub.length <<- inputlength()

      updateChoices <- input$modeltermSelect[which(input$modeltermSelect != "cpue")]

      updateSelectInput(inputId = "modelChoice", label = "Model terms",
                        choices = updateChoices, selected = updateChoices)
      n <- 10##############
      updateSelectInput(inputId = "modelselect", choices = c(1:n))
      updateSelectInput(inputId = "lengthselect", choices = c(1:n))
      updateSelectInput(inputId = "cpuechoice", choices = c(1:n))

      # set up loop to run each simulation
      withProgress(message = "Running surveys", expr = {
        for(i in 1:n){


          incProgress(amount = 1/n)
          trial_data <- sub.data

          trial_data$rowid <- as.numeric(1:nrow(trial_data))

          # for each year, randomly retrieve one data point per station
          draw <- array()
          place <- 1
          place2 <- 1

          for(j in unique(trial_data$Season)){

            # get a subset of potential points
            filter.data <- trial_data[which(trial_data$Season == j),]

            # loop to get random draw per station
            for(k in 1:nrow(station.points)){

              dist1 <- spDistsN1(cbind(filter.data$Longitude, filter.data$Latitude),
                                 c(station.points[k,1],
                                   station.points[k,2]), longlat = T)

              dist2 <- spDistsN1(cbind(filter.data$Longitude, filter.data$Latitude),
                                 c(station.points[k,1],
                                   station.points[k,2]), longlat = T)

              if(input$unit == "Nautical Mile"){
                buff <- input$buffer * 1.852
              } else {
                buff <- input$buffer
              }


              buffed1 <- which(dist1 <= buff)
              buffed2 <- which(dist2 <= buff)

              bufferzone <- unique(c(buffed1, buffed2))


              station.data <- filter.data[bufferzone,]


              if(nrow(station.data) > 1){
                for(x in 1:2){
                  draw[place] <- sample(station.data$rowid, size = 1, replace = T)
                  place <- place + 1
                }
              }

            }

          }



          # set up the simulated data
          simul_data <- trial_data[which(trial_data$rowid %in% draw),]

          for(i in 1:ncol(simul_data)){
            if(colnames(simul_data)[i] %in% input$factorlist){
              simul_data[,i] <- as.factor(simul_data[,i])
            }
          }



          #colnames(simul_data)[c(3,4,5,6,15,12,14)] <- c ("gear","season", "month", "nationality", "depth.class", "cetaceans", "area.n")

          simul_data$Season <- factor(simul_data$Season)
          simul_data$Gear <- factor(simul_data$Gear)
          #simul_data$Month <- factor(simul_data$Month)
          simul_data$Nationality <- factor(simul_data$Nationality)
          #simul_data$depth.class <- factor(simul_data$depth.class)
          #simul_data$cetaceans <- factor(simul_data$cetaceans)
          #simul_data$area.n <- factor(simul_data$area.n)

          if(i == 1){
            simul_ID <- list(unique(simul_data$ID))
          } else {
            simul_ID <- list(simul_ID, list(unique(simul_data$ID)))
          }

          simul_data <<- simul_data


          # weird duplicate bug fix
          df <- table(simul_data$Season)
          for(i1 in 1:length(df)){
            if(df[i1] > dim(station.points)[1]){
              j1 <- df[i1] - dim(station.points)[1]
              k1 <- sample(1:(j1+1), j1)

              removedf <- simul_data[which(simul_data$season == names(df)[i1]),]
              removedr <- removedf$rowid[k1]

              simul_data <- simul_data[which(!(simul_data$rowid %in% removedr)),]
            }
          }

          for(ikk in modelSetup()){

            if(ikk == modelSetup()[1]){
              model.base <- ikk
            } else {
              model.base <- paste(model.base, ikk, sep = "+")
            }

          }

          model.base <- paste("cpue", "~", model.base, sep = "")

          #simul_model <- glm(as.formula(model.base), family=gaussian(link="sqrt"),
          #                   na.action=na.omit, data = simul_data, subset=cpue>0, maxit = 100)

          simul_model <- glm(as.formula(model.base), family=gaussian(link="sqrt"),
                             na.action=na.omit, data = simul_data, maxit = 100)

          if(i == 1){
            modelruns <- list(simul_model)
            #predictruns <- list(No3)
          } else {
            modelruns[[i]] <- simul_model
            #predictruns[[i]] <- No3
          }


          # prep length data
          trial_length <<- sub.length
          trial_length$rowid <- as.numeric(1:nrow(trial_length))

          # loop to get length data
          draw <- array()

          for(j in unique(trial_length$Season)){



            # get a subset of potential points
            lfilter.data <- trial_length[which(trial_length$Season == j),]


            # loop to get random draw per station

            for(k in 1:nrow(station.points)){

              dist1 <- spDistsN1(cbind(lfilter.data$Longitude, lfilter.data$Latitude),
                                 c(station.points[k,1],
                                   station.points[k,2]), longlat = T)

              dist2 <- spDistsN1(cbind(lfilter.data$Longitude, lfilter.data$Latitude),
                                 c(station.points[k,1],
                                   station.points[k,2]), longlat = T)

              if(input$unit == "Nautical Mile"){
                buff <- input$buffer * 1.852
              } else {
                buff <- input$buffer
              }


              buffed1 <- which(dist1 <= buff)
              buffed2 <- which(dist2 <= buff)

              bufferzone <- unique(c(buffed1, buffed2))


              station.data <<- lfilter.data[bufferzone,]


              if(nrow(station.data) > 1){
                for(x in 1:2){
                  draw[place2] <- sample(station.data$rowid, size = 1, replace = T)
                  place2 <- place2 + 1
                }
              }

            }


            #colnames(trial_length)[c(8,9,10,11)] <- c ("gear","season", "month", "nationality")

            #trial_length$season <- factor(trial_length$season)
            #trial_length$gear <- factor(trial_length$gear)
            #trial_length$month <- factor(trial_length$month)
            #trial_length$nationality <- factor(trial_length$nationality)


            # if(i == 1){
            #   simul_length <- trial_length[which(trial_length$rowid %in% draw),]
            #   simul_length <- list(simul_length)
            # } else {
            #   simul_length[[i]] <- trial_length[which(trial_length$rowid %in% draw),]
            # }

            simul_length <<- trial_length[which(trial_length$rowid %in% draw),]



          }
        }

        modelruns <<- modelruns
        #predictruns <<- predictruns
      })

    })


    #### Generating models ####
    model.terms <- reactive({

      for(i in (input$modelChoice)){
        if(!(i %in% c("Catch", "Effort"))){
          if(i == input$modelChoice[1]){
          model.t <<- i
        } else {
          model.t <<- paste(model.t, i, sep = "+")
        }
      }
        }


      return(paste("cpue", " ~ ", model.t))
    })

    GLM_run <- reactive({
      tryme <- input$bootstrap
      run_data <- modelruns[[GLM_choice()]]$data

      for(i in 1:ncol(run_data)){
        if(colnames(run_data)[i] %in% input$factorlist){
          run_data[,i] <- as.factor(run_data[,i])
        }
      }

      #m <- glm(as.formula(model.terms()), family=gaussian(link="sqrt"),
      #         na.action=na.omit, data = run_data, subset=cpue>0, maxit = 100)

      m <- glm(as.formula(model.terms()), family=gaussian(link="sqrt"),
               na.action=na.omit, data = run_data, subset=cpue>0, maxit = 100)

      return(m)
    })

    predict_run<- reactive({

      place <- GLM_choice()


      for(i in 1:length(modelruns)){



        simul_data <- modelruns[[i]]$data

        for(i6 in 1:ncol(simul_data)){
          if(colnames(simul_data)[i6] %in% input$factorlist){
            simul_data[,i6] <- as.factor(simul_data[,i6])
          }
        }

        m <- glm(as.formula(model.terms()), family=gaussian(link="sqrt"),
                 na.action=na.omit, data = simul_data, subset=cpue>0, maxit = 100)

        if(is.null(simul_data)){
          next
        }

        nseasons<-length(levels(simul_data$Season))
        seasons<-levels(simul_data$Season)
        #seasons<-c("2003","2004","2005","2006","2007","2008","2009","2010","2011")
        #nseasons<-length(seasons)

        ## WITHOUT WHALES
        # build predict frame
        predict.matrix.build <- data.frame(n = nseasons)
        for(i5 in input$modelChoice){
          place <- which(input$modelChoice == i5)

          if(i5 == "Season"){
            predict.matrix.build <- cbind(predict.matrix.build, as.factor(seasons))
          } else {
            predict.matrix.build <- cbind(predict.matrix.build, rep(simul_data[1,i5],nseasons))
          }

        }

        predict.matrix.build <- predict.matrix.build[,-c(which(colnames(predict.matrix.build) == "n"))]

        colnames(predict.matrix.build) <- input$modelChoice
        GLM483.predict.frame <- predict.matrix.build


        for(i4 in 1:ncol(GLM483.predict.frame)){
          GLM483.predict.frame[,i4] <- as.factor(GLM483.predict.frame[,i4])
        }

        GLM.pred.rev.3 <- predict(m, newdata=GLM483.predict.frame, type="response", se.fit=T)



        No3 <- data.frame(season=seasons)
        No3$estimate <- GLM.pred.rev.3$fit#*positive.proportion[c((length(positive.proportion)-8):(length(positive.proportion)))]
        No3$se <- GLM.pred.rev.3$se.fit
        No3$lower95 <- (GLM.pred.rev.3$fit-2*GLM.pred.rev.3$se.fit) #*positive.proportion[c((length(positive.proportion)-8):(length(positive.proportion)))]
        No3$upper95 <- (GLM.pred.rev.3$fit+2*GLM.pred.rev.3$se.fit) #*positive.proportion[c((length(positive.proportion)-8):(length(positive.proportion)))]
        No3$index <- No3$estimate*1000
        No3$cv    <- No3$se/No3$estimate

        op1 <- No3[,c(1,6)]
        op2 <- No3[,c(1,7)]
        op2$season <- paste0("cv_",op2$season)

        if(i == 1){
          predictruns1 <<- data.frame(No3, group = i)
        } else {
          predictruns1 <<- rbind(predictruns1, data.frame(No3, group = i))
        }

      }
      return(predictruns1)

    })



    #### Output global reactive statements ####


    GLM_choice <- reactive({
      i <- as.numeric(input$modelselect)
      return(i)
    })

    PWR_choice <- reactive({
      i <- as.numeric(input$pwrselect)
      return(i)
    })

    choiceID <- reactive({
      i <- as.numeric(input$lengthselect)

      return(simul_ID[[i]])
    })

    length.data <- reactive({
      Tlength.ret <- simul_length[[choiceID()]]

      return(Tlength.ret)
    })


    #### Outputs ####

    ## Reactive data summary plotting

    pairsChoice <- reactive({
      columnChoice <- input$pairplot
      return(which(names(inputdata()) %in% columnChoice))
    })

    histChoice <- reactive({
      return(which(names(inputdata()) == input$summarytype))
    })

    # Build reactive ui placement based on plot choice
    summaryplot <- reactive({
      if(input$summarytype == "Pairs"){
        return(
          tagList(
            plotOutput(outputId = "datasummaryPairs")
          )
        )
      } else {
        return(
          tagList(
            plotOutput(outputId = "datasummary")
          )
        )
      }
    })

    output$summaryUI <- renderUI({
      summaryplot()
    })

    # Here are the plots
    output$datasummary <- renderPlot({
      qplot(x = inputdata()[,histChoice()]) +
        theme_bw() + xlab(input$summarytype)
    })

    output$datasummaryPairs <- renderPlot({
      ggpairs(inputdata(), columns = pairsChoice(), cardinality_threshold = 20, bins = 30) + theme_bw()
    }, height = 1000)

    ## Survey map
    output$surveymap <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        #addProviderTiles(provider = "Esri.WorldPhysical") %>%
        fitBounds( lng1 = min(masterdata$Longitude)
                   , lat1 = min(masterdata$Latitude)
                   , lng2 = max(masterdata$Longitude)
                   , lat2 = max(masterdata$Latitude) ) %>%
        addDrawToolbar(
          targetGroup='User layer',
          polylineOptions=FALSE,
          markerOptions = TRUE,
          circleOptions = TRUE, editOptions = editToolbarOptions())
    })



    ## GLM effects plot

    GLM_run <- reactive({
      run_data <- modelruns[[GLM_choice()]]$data

      for(i in 1:ncol(run_data)){
        if(colnames(run_data)[i] %in% input$factorlist){
          run_data[,i] <- as.factor(run_data[,i])
        }
      }

      #m <- glm(as.formula(model.terms()), family=gaussian(link="sqrt"),
      #         na.action=na.omit, data = run_data, subset=cpue>0, maxit = 100)

      m <- glm(as.formula(model.terms()), family=gaussian(link="sqrt"),
               na.action=na.omit, data = run_data, maxit = 100)

      return(m)
    })

    full_model <- reactive({
      full_data <- inputdata()

      for(i in 1:ncol(full_data)){
        if(colnames(full_data)[i] %in% input$factorlist){
          full_data[,i] <- as.factor(full_data[,i])
        }
      }


      full.glm <<- glm(formula = as.formula(model.terms()), family=gaussian(link="sqrt"),
                       na.action=na.omit, data = full_data, subset=cpue>0, maxit = 100)

      return(full.glm)
    })

    full_predict <- reactive({

      m <- full_model()
      full_data <- inputdata()

      for(i in 1:ncol(full_data)){
        if(colnames(full_data)[i] %in% input$factorlist){
          full_data[,i] <- as.factor(full_data[,i])
        }
      }

      nseasons<-length(levels(full_data$Season))
      seasons<-levels(full_data$Season)
      predict.matrix.build <- data.frame(n = nseasons)
      for(i in input$modelChoice){
        place <- which(input$modelChoice == i)

        if(i == "Season"){
          predict.matrix.build <- cbind(predict.matrix.build, as.factor(seasons))
        } else {
          predict.matrix.build <- cbind(predict.matrix.build, rep(full_data[1,i],nseasons))
        }

      }

      predict.matrix.build <- predict.matrix.build[,-c(which(colnames(predict.matrix.build) == "n"))]

      colnames(predict.matrix.build) <- input$modelChoice
      GLM483.predict.frame <- predict.matrix.build


      for(i in 1:ncol(GLM483.predict.frame)){
        GLM483.predict.frame[,i] <- as.factor(GLM483.predict.frame[,i])
      }



      GLM.pred.rev.3 <- predict(m, newdata=GLM483.predict.frame, type="response", se.fit=T)



      No3 <- data.frame(season=seasons)
      No3$estimate <- GLM.pred.rev.3$fit#*positive.proportion[c((length(positive.proportion)-8):(length(positive.proportion)))]
      No3$se <- GLM.pred.rev.3$se.fit
      No3$lower95 <- (GLM.pred.rev.3$fit-2*GLM.pred.rev.3$se.fit) #*positive.proportion[c((length(positive.proportion)-8):(length(positive.proportion)))]
      No3$upper95 <- (GLM.pred.rev.3$fit+2*GLM.pred.rev.3$se.fit) #*positive.proportion[c((length(positive.proportion)-8):(length(positive.proportion)))]
      No3$index <- No3$estimate*1000
      No3$cv    <- No3$se/No3$estimate

      return(No3)

    })

    GLM_plottype <- reactive({
      if(input$GLMplot == "Depth Class"){
        "depth.class"
      } else if(input$GLMplot == "Area"){
        "area.n"
      } else {
        (input$GLMplot)
      }

    })

    plot_react <- reactive({

      if(input$fullmodel == T){

        x <- plot.Gam(full_model(),se=TRUE, terms = GLM_plottype())

        y <- plot.Gam(GLM_run(),se=TRUE, terms = GLM_plottype())

        place <- 0
        xar <- array()
        yar <- array()
        sear <- array()
        model <- array()
        rug <- array()

        for(j in unique(x$preplot[[1]]$x)){

          place <- place + 1

          xar[place] <- j
          yar[place] <- mean(x$preplot[[1]]$y[which(x$preplot[[1]]$x == j)])
          sear[place] <- mean(x$preplot[[1]]$se.y[which(x$preplot[[1]]$x == j)])
          model[place] <- "full"

        }

        for(j in unique(y$preplot[[1]]$x)){

          place <- place + 1

          xar[place] <- j
          yar[place] <- mean(y$preplot[[1]]$y[which(y$preplot[[1]]$x == j)])
          sear[place] <- mean(y$preplot[[1]]$se.y[which(y$preplot[[1]]$x == j)])
          model[place] <- "current"


        }

        rug <- c(table(x$preplot$season$x), table(y$preplot$season$x))

        yup <- yar + sear

        ydown <- yar - sear

        test <<- data.frame(xar = xar, yar = yar, yup = yup, ydown = ydown, model = model, sear = sear)

        test.plot <- ggplot(data = test) + theme_bw() +
          geom_point(aes(x = xar, y = yar), alpha = 0.5) +
          geom_pointrange(aes(x = xar, y = yar, ymin = ydown, ymax = yup, xmin = xar, xmax = xar, col = model, size = model), alpha = 0.5) +
          scale_size_manual(values = c(1, 0.5)) + scale_colour_manual(values = c("red", "black")) +
          xlab(paste(GLM_plottype())) + ylab(paste("Partial effect for ", GLM_plottype(), sep = ""))

        return(test.plot)

      } else {


        y <- plot.Gam(GLM_run(),se=TRUE, terms = GLM_plottype())

        place <- 0
        xar <- array()
        yar <- array()
        sear <- array()
        model <- array()
        rug <- array()


        for(j in unique(y$preplot[[1]]$x)){

          place <- place + 1

          xar[place] <- j
          yar[place] <- mean(y$preplot[[1]]$y[which(y$preplot[[1]]$x == j)])
          sear[place] <- mean(y$preplot[[1]]$se.y[which(y$preplot[[1]]$x == j)])
          model[place] <- "current"


        }

        rug <- c(table(y$preplot[[1]]$x))

        yup <- yar + sear

        ydown <- yar - sear

        test <<- data.frame(xar = xar, yar = yar, yup = yup, ydown = ydown, model = model, sear = sear)

        test.plot <- ggplot(data = test) + theme_bw() +
          geom_point(aes(x = xar, y = yar)) +
          geom_pointrange(aes(x = xar, y = yar, ymin = ydown, ymax = yup, xmin = xar, xmax = xar, col = model), show.legend = F) +
          scale_size_manual(values = c(1)) + scale_colour_manual(values = c("black")) +
          xlab(paste(GLM_plottype())) + ylab(paste("Partial effect for ", GLM_plottype(), sep = ""))

        return(test.plot)

      }

    })

    output$plotted <- renderPlot({
      plot_react()
    })

    ## GLM summary
    output$GLMsummary <- renderPrint({
      summary(GLM_run())
    })

    ## Length facet grid
    output$lengthplot <- renderPlot({
      ggplot(mapping = aes_string("LENGTH_CM"), data = length.data()) +
        theme_bw() + geom_freqpoly() + facet_grid(rows = vars(season), cols = vars(gear))
    })

    predict_graphing <- reactive({
      df <- predict_run()
      df$group <- as.factor(df$group)

      df$alpha <- 0.1
      df$size <- 0.15
      df$filled <- NA

      df$alpha[which(df$group == GLM_choice())] <- 0.4
      df$size[which(df$group == GLM_choice())] <- 1
      df$filled[which(df$group == GLM_choice())] <- GLM_choice()

      return(df)

    })

    ## CPUE plot
    cpueplot.type <- reactive({
      if(input$fullmodel == T){

        ggplot(subset(predict_graphing(), season != "2003")) +
          geom_line(aes(x=as.numeric(as.character(season)), y=estimate, group = group, col = group, size = size), show.legend = F) + scale_size_continuous(range = c(0.15, 1)) +
          geom_ribbon(aes(x=as.numeric(as.character(season)), ymax=upper95, ymin = lower95, group = group, fill = filled, alpha = alpha), linetype=2, show.legend = F) +
          scale_y_continuous(limits=c(0,0.5)) +labs(x="Season", y="Standardised CPUE") + theme_bw() + scale_alpha_continuous(range = c(0.1, 0.4)) +
          geom_ribbon(aes(x=as.numeric(as.character(season)), ymax=upper95, ymin = lower95), data = subset(full_predict(), season != "2003"), alpha = 0.15, fill = "black", show.legend = F) +
          geom_line(aes(x=as.numeric(as.character(season)), y=estimate), size = 1.25, col = "black", data = subset(full_predict(), season != "2003"))


      } else {

        ggplot(subset(predict_graphing(), season != "2003")) +
          geom_line(aes(x=as.numeric(as.character(season)), y=estimate, group = group, col = group, size = size), show.legend = F) + scale_size_continuous(range = c(0.15, 1)) +
          geom_ribbon(aes(x=as.numeric(as.character(season)), ymax=upper95, ymin = lower95, group = group, fill = filled, alpha = alpha), linetype=2, show.legend = F) +
          scale_y_continuous(limits=c(0,0.5)) +labs(x="Season", y="Standardised CPUE") + theme_bw() + scale_alpha_continuous(range = c(0.1, 0.4))

      }
    })

    output$cpueplot <- renderPlot({
      cpueplot.type()
    })

    ## Power Analysis
    powerInput <- reactive({
      powerChoice <- input$pwrReduce
      return(powerChoice)
    })

    output$powerPlot <- renderPlot({
      ggplot() +
        geom_density(aes(x = modelruns[[PWR_choice()]]$data$cpue), fill = "red", alpha = 0.5) +
        geom_density(aes(x = modelruns[[PWR_choice()]]$data$cpue * powerInput()), fill = "blue", alpha = 0.5) +
        theme_bw() + xlab("log(CPUE)")
    })

    powerSample <- reactive({
      nrow(modelruns[[PWR_choice()]]$data)

      return(nrow(modelruns[[PWR_choice()]]$data))
    })

    powerD <- reactive({
      mean.dff <- abs(mean(log(modelruns[[PWR_choice()]]$data$cpue), na.rm = T) - mean(log(modelruns[[PWR_choice()]]$data$cpue) * powerInput(), na.rm = T))
      sd.pool <- sd(c(log(modelruns[[PWR_choice()]]$data$cpue), log(modelruns[[PWR_choice()]]$data$cpue) * powerInput()), na.rm = T)

      return(mean.dff/sd.pool)
    })

    powerSig <- reactive({
      siglevel <- input$pSig

      return(siglevel)
    })

    output$powerOut <- renderPrint({
      pwr.t.test(
        n = powerSample(),
        d = powerD(),
        sig.level = powerSig(),power = NULL)
    })
  }

  return(server)
}




#' Run the Survey Simulation Tool
#'
#' The function to run the tool, which will open in a new window through RShiny.
#' @param catcheffort Data frame containing combined catch and effort data
#' @param length Optional data frame containing length data for the same geographical area as the catch and effort data
#' @details Catcheffort has to be a data frame containing columns for Species, Catch, Effort (eg hooks), Gear, Nationality, ShipName, Latitude, Longitude and Season as a minimum. Running data through the simulatorData() function allows checking and name changing of columns to ensure it reaches minimum standards.
#' Length data is optional, and allows viewing of simulated length results in the app.
#' @return Running an RShiny app, no data is returned to the R session by default.
#' @seealso
#' simulatorData()
#' @examples
#' ## Run data using the included (randomly generated) data.
#' # Not specifying data runs the app in test mode
#' # Note that this data is randomly generated from mathematical distributions and is centered around longitude 0, latitude 0. It has no basis in any real CCAMLR dataset.
#'
#' surveySimulator()
#' @export

surveySimulator <- function(catcheffort = NA, length = NA){
  # Return error message if data isn't included
  if(is.na(catcheffort)){
    data(exampleCatchEffort)
    masterdata <- catcheffort.sample
    warning("Catch effort data has not been specified by the user, so the app is running in test mode. This uses a test dataset that is randomly generated and does not include any real data from CCAMLR. If you included length data it has been ignored.")
  } else {
    masterdata <<- catcheffort
  }

  # Warn the user about no length data, and notify the consequence
  if(is.na(length)){
    length.use <- 0 # Set value for the function to know what app to run
    warning("Length data is not uploaded, so this part of the app will be supressed.")
  } else {
    if(!is.na(catcheffort)){
      length.use <- 1 # Set value for the function to know what app to run
    } else {
      length.use <- 0 # Set value for the function to know what app to run
    }

  }

  # Check data has been run through the simulatorData function, or is just the correct format
  if(is.na(check.column.names(input.frame = catcheffort, column.target = c()))){
    message("Data check complete - the catch-effort data you included is fine! The app will now start.")
  } else {
    stop("Data check complete - your input data doesn't have the correct format for the app to interpret. Sorry! You can check the minimum column requirements in the manual, or run your data through the simulatorData function.")
  }


  Tlength <<- length

  #masterdata$cpue <- masterdata$Catch/masterdata$Hooks

  # Set ui and server based on length value
  if(length.use == 1){
    ui.run <- simulator.ui(masterdata)
    server.run <- simulator.server()
  } else {
    ui.run <- simulator.ui.nolength(masterdata)
    server.run <- simulator.server.nolength()
  }

  # Run the app
  shinyApp(ui.run, server.run)
}

