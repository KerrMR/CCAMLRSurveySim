# The simulator.data function opens a shinyApp which allows checking of a data.frame

simulatorData <- function(){
  #### Data upload splash ####

  ## column targets
  #target.catch <- c("SPECIES_CODE", "CAUGHT_KG_TOTAL")
  #target.effort <- c("SHIP_NAME", "LONGLINE_CODE", "FishingLatitude", "FishingLongitude", "SeasonAbbreviation", "HOOK_COUNT")
  #target.length <- c("LENGTH_CM", "FishingLatitude", "FishingLongitude", "LONGLINE_CODE", "SeasonAbbreviation")



  ### Data upload splash screen

  ## Step One: How would you like to upload data?
  # Two buttons to select full RData file vs Individual datasets
  observeEvent(input$proceed, {
    removeModal()
    showModal(
      modalDialog(
        title = "How would you like to upload data?",
        footer = NULL,
        actionButton(inputId = "onedata", label = "Upload RData"),
        actionButton(inputId = "sepdata", label = "Upload data as files")
      )
    )
  })



  ## Step Two: Set up the upload screen
  # Set ui based on choice

  # If only one data file
  observeEvent(input$onedata, {
    removeModal()
    showModal(
      modalDialog(
        title = "Upload Rdata file",
        footer = NULL,
        fileInput(inputId = "onedataFILE", "Upload RData file", multiple = F, accept = c(".RDS", ".RData", ".csv")),
        actionButton(inputId = "goFILEONE", label = "Ready")
      )
    )
  })

  # If there are multiple data files
  # Optional catch/effort combined file
  observeEvent(input$sepdata, {
    removeModal()
    showModal(
      modalDialog(
        title = "Upload data files",
        footer = NULL,
        fileInput(inputId = "effortFILE", label = "Upload effort data", multiple = F),
        fileInput(inputId = "catchFILE", label = "Upload catch data", multiple = F),
        checkboxInput(inputId = "combinedCE", label = "Combined catch/effort", value = T),
        fileInput(inputId = "lengthFILE", label = "Upload length data", multiple = F),
        actionButton(inputId = "goFILE", label = "Ready")
      )
    )
  })


  ## Set up uploaded data
  # For one data file
  observeEvent(input$goFILEONE, {
    file <- input$onedataFILE

    ext <- tools::file_ext(file$datapath)
    req(file)
    validate(need(ext %in% c(".RData", ".RDS", ".csv"), "Please upload a file in the correct format. Read the data upload guide for more information."))

    load(file = file$datapath, envir = "")

    ## Check the right pieces are there and check for columns
    # If the file is a csv, check for columns
    # Otherwise, warn the user and do not continue
    if(exists(catch) & exists(effort) & exists(length) & ext != ".csv"){

      upload.catch <- catch
      upload.effort <- effort
      upload.length <- length

      ## Make sure the right columns are there

      # check each data upload
      missing.catch <- array()
      missing.effort <- array()
      missing.length <- array()

      place <- 1
      for(i in target.length){
        if(i %in% colnames(upload.length)){
          next
        } else {
          missing.length[place] <- i
          place <- place + 1
        }
      }

      place <- 1
      for(i in target.effort){
        if(i %in% colnames(upload.effort)){
          next
        } else {
          missing.effort[place] <- i
          place <- place + 1
        }
      }

      place <- 1
      for(i in target.catch){
        if(i %in% colnames(upload.catch)){
          next
        } else {
          missing.catch[place] <- i
          place <- place + 1
        }
      }



      if(is.na(missing.catch[1]) & is.na(missing.effort[1]) & is.na(missing.length[1])){
        # Data is fine, please proceed
        removeModal()
      } else {

        if(!is.na(missing.catch[1])){
          missC <- length(missing.catch)

          for(i in 1:missC){
            j <- selectInput(inputId = paste("catch", i, sep = ""), label = missing.catch[i], choices = colnames(upload.catch), multiple = F)

            if(i == 1){
              catchUI <- tagList(j)
              inC <- paste("catch", i, sep = "")
            } else {
              catchUI <- tagList(catchUI, j)
              inC <- c(inC, paste("catch", i, sep = ""))
            }
          }

        } else {
          catchUI <- NA
        }

        if(!is.na(missing.effort[1])){
          missE <- length(missing.effort)

          for(i in 1:missE){
            j <- selectInput(inputId = paste("effort", i, sep = ""), label = missing.effort[i], choices = colnames(upload.effort), multiple = F)

            if(i == 1){
              effortUI <- tagList(j)
              inE <- paste("effort", i, sep = "")
            } else {
              effortUI <- tagList(effortUI, j)
              inE <- c(inE, paste("effort", i, sep = ""))
            }
          }

        } else {
          effortUI <- NA
        }

        if(!is.na(missing.length[1])){
          missL <- length(missing.length)

          for(i in 1:missL){
            j <- selectInput(inputId = paste("length", i, sep = ""), label = missing.length[i], choices = colnames(upload.length), multiple = F)

            if(i == 1){
              lengthUI <- tagList(j)
              inL <- paste("length", i, sep = "")
            } else {
              lengthUI <- tagList(lengthUI, j)
              inL <- c(inL, paste("length", i, sep = ""))
            }
          }

        } else {
          lengthUI <- NA
        }

        columnUI <- tagList(catchUI, effortUI, lengthUI)
        columnUI <- tagList(columnUI[which(!is.na(columnUI))])

        columnFIX <- list(missing.catch, missing.effort, missing.length)
        columnFIXIN <- list(inC, inE, inL)

        removeModal()
        showModal(
          modalDialog(
            title = "Please identify the missing columns, they could not be automatically detected",
            columnUI,
            actionButton(inputId = "columnUIDONE", label = "Done!"))
        )
      }

      removeModal()
    } else if(ext == ".csv"){
      jf
    } else {
      showModal(
        modalDialog(
          title = NULL,
          h4("The uploaded file does not contain catch, effort and length data frames. If you created the file, please ensure it follows the protocol in the user guide - it requires 'catch', 'effort' and 'length' data frames.")
        )
      )
    }
  })

  # For multiple
  observeEvent(input$goFILE, {
    file <- input$lengthFILE

    ext <- tools::file_ext(file$datapath)
    req(file)
    validate(need(ext %in% c(".csv"), "Please upload a csv file. Read the data upload guide for more information."))

    upload.length <- read.csv(file = file$datapath, envir = "")

    file <- input$catchFILE

    ext <- tools::file_ext(file$datapath)
    req(file)
    validate(need(ext %in% c(".csv"), "Please upload a csv file. Read the data upload guide for more information."))

    upload.catch <- read.csv(file = file$datapath, envir = "")

    file <- input$effortFILE

    ext <- tools::file_ext(file$datapath)
    req(file)
    validate(need(ext %in% c(".csv"), "Please upload a csv file. Read the data upload guide for more information."))

    upload.effort <- read.csv(file = file$datapath, envir = "")

    ## Check columns

  })



  observeEvent(
    input$columnUIDONE,
    {
      for(i in columnFIX){
        for(j in 1:length(columnFIX[i])){
          if(!is.na(columnFIX[[i]][j])){
            replacement <- columnFIXIN
            replacement <- input[replacement]

            replaced <- columnFIX[[i]][j]

            if(i == 1){
              k <- which(colnames(upload.catch) == replaced)
              colnames(upload.catch)[k] <- replacement
            } else if(i == 2){
              k <- which(colnames(upload.effort) == replaced)
              colnames(upload.effort)[k] <- replacement
            } else if(i == 3){
              k <- which(colnames(upload.length) == replaced)
              colnames(upload.length)[k] <- replacement
            }

          }
        }
      }

      upload.length <<- upload.length
      upload.catch <<- upload.catch
      upload.effort <<- upload.effort

      removeModal()
    }
  )


}
