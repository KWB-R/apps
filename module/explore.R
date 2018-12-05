# ui_explore -------------------------------------------------------------------
ui_explore <- function(...) 
{
  fluidPage(
    
    includeCSS("www/metricsgraphics_user.css"),
    
    title = "Hantush",
    sidebarLayout(
      sidebarPanel(         
        
        h3("Model configuration"),
        uiOutput('ui_time'),        
        
        #h4("Infiltrationsbecken"),
        uiOutput('ui_basinLength'),
        uiOutput('ui_basinWidth'),
        uiOutput('ui_infiltrationRate'),
        
        #h4("Grundwasserleiter"),
        uiOutput('ui_horizConductivity'),
        uiOutput('ui_iniHead'),
        uiOutput('ui_specificYield'),
        
        actionButton(
          "reset_input", 
          "Reset parameters"
        ),
        
        checkboxInput("details", "Details", value = FALSE),
        
        conditionalPanel(
          
          condition = "input.details",
          
          h5("Base configuration"),
          
          h6("Units"),
          
          textInput(
            inputId = "unitLength",
            label = "Length unit", 
            value = "meter"
          ),
          
          textInput(
            inputId = "unitTime",  
            label = "Time unit", 
            value = "day"
          ),
          
          #textInput(
          #  inputId = "plotheight",  
          #  label = "Hoehe der Grafik in Pixel", 
          #  value = "400"
          #),
          
          uiOutput('ui_numberTimeSteps'),


          h6("Coordinates"),
          
          uiOutput('ui_xDistances'),
          uiOutput('xSpacing'),
          
          h6("Plot"),
          
          uiOutput('yMax'), 
          h5("Download"),
          downloadButton(outputId = "downloadExploreResults", 
                         label = "Download results"),
          downloadButton(outputId = "downloadExploreReport", 
                         label = "Download report"),
          radioButtons('format', 'Report format', c('PDF', 'HTML', 'Word'),
                       inline = TRUE)
        )
      ),
      mainPanel(
        h3("Model simulation"), 
        #plotOutput("mounding"),
        metricsgraphicsOutput(
          'mounding1',           
          width = "100%", 
          height = #if (is.null(input$plotheight)) 
            "500px"
          #else 
          # paste0(input$plotheight, "px")          
        ),
        #showOutput('mounding2')
        tableOutput("managerresult")
      )
    )
  )
}

# server_explore ---------------------------------------------------------------
server_explore <-  function(...) 
{  
 
  ### Set "d" as default value even if ""
  timeUnit <- reactive(
    if (input$unitTime == "") "day" else sprintf(" %s", input$unitTime)
  )

  ### Set "m" as default value even if ""
  lengthUnit <- reactive(
    if (input$unitLength=="") "meter" else sprintf(" %s", input$unitLength)
  )
  
  lengthPerTime <- reactive(
    paste0(lengthUnit(),"/",timeUnit())
  )
  
  resetPostfix <- reactive(
    (input$reset_input %% 10000) + 1
  )
  
  output$ui_time <- renderUI({
    div(
      id = sprintf("time_%s",resetPostfix()),
      sliderInput(
        inputId = "time",
        label = "Time since start of infiltration",
        min = 1,
        max = 100,
        value = 1,
        animate = TRUE,
        post = timeUnit()
      )
    )
  })
  
  #### Only length dependent parameters
  output$ui_basinLength <- renderUI({
    div(
      id = sprintf("basinLength_%s",resetPostfix()),
      sliderInput(
        inputId = "basinLength",
        label = "Basin length",
        min = 5,
        max = 100,
        value = 50,
        post = lengthUnit())
    )
  })
  
  output$ui_basinWidth <- renderUI({
    div(
      id = sprintf("basinWidth_%s",resetPostfix()),
      sliderInput(
        inputId = "basinWidth",
        label = "Basin width",
        min = 5,
        max = 100,
        value = 50,
        post = lengthUnit())
    )
  })
  
  output$ui_infiltrationRate <- renderUI({ 
    div(
      id = sprintf("infiltrationRate_%s",resetPostfix()),
      sliderInput(
        inputId = "infiltrationRate",
        label = "Basin infiltration rate",
        min = 0.1,
        max = 10,
        value = 4.3,
        step = 0.1,
        post = lengthPerTime()
      )
    )
  })
  
  output$ui_horizConductivity <- renderUI({
    div(
      id = sprintf("horizConductivity_%s",resetPostfix()),
      sliderInput(
        inputId = "horizConductivity", 
        #label = "Hydraulische Leitfähigkeit",
        label = "Hydraulic aquifer conductivity",
        min= 1, 
        max = 1000,
        value = 43.2,
        step = 0.1,
        post = lengthPerTime()
      )
    )
  })
  
  output$ui_iniHead <- renderUI({
    div(
      id = sprintf("iniHead_%s", resetPostfix()),  
      sliderInput(
        inputId = "iniHead", 
        #label = "Anfängliche gesättigte Mächtigkeit",
        label = "Initial saturated aquifer thickness",
        min = 1,
        max = 100,
        value = 40,
        step = 1,
        post = lengthUnit()
      )
    )
  })
  
  output$ui_specificYield <- renderUI ({
    div(
      id = sprintf("specificYield_%s", resetPostfix()),
      sliderInput(
        inputId = "specificYield",
        label = "Specific yield",
        min = 0.01,
        max = 0.5,
        value = 0.2,
        step = 0.01
      )
    )
  })
  
  output$ui_numberTimeSteps <- renderUI ({
    div(
      id = sprintf("numberTimeSteps_%s", resetPostfix()),
      sliderInput(
        inputId = "numberTimeSteps",
        label = "Number of time steps",
        min = 1,
        max = 500,
        value = 5,
        step = 1
      )
    )
  })
  
  output$ui_xDistances <- renderUI({
    div(
      id = sprintf("xDistances_%s", resetPostfix()),
      sliderInput(
        inputId = "xDistances",
        label ="x min/max distances",
        min = -1000,
        max = 1000, 
        value = c(-100,200),
        post = lengthUnit()
      )
    )
  })
  
  output$xSpacing <- renderUI({
    div(
      id = sprintf("xSpacing_%s", resetPostfix()),
      sliderInput(
        inputId = "xSpacing",
        label ="x spacing",
        min = 1,
        max = 50, 
        value = 10,
        post = lengthUnit()
      )
    )
  })
  
  output$yMax <- renderUI({
    div(
      id = sprintf("yMax_%s", resetPostfix()),
      sliderInput(
        inputId = "yMax",
        label = "Set maximum value for y-axis",
        min = 0,
        max = 100,
        step = 0.25,
        value = 5.5,
        post = lengthUnit()
      )
    )
  })
    
  baseProps  <-  reactive(  {
    if(!is.null(input$time) & 
         !is.null(input$basinWidth) & 
         !is.null(input$basinLength) & 
         !is.null(input$infiltrationRate) & 
         !is.null(input$horizConductivity) &  
         !is.null(input$iniHead) &  
         !is.null(input$specificYield) & 
         !is.null(input$numberTimeSteps)
       ) {
      #print("baseProps: ",as.character(isolate(baseProps())))
      x <- list()
      
      withProgress(
        message = "1. Update model configuration...", 
        value = 0, 
        expr = {
          x <- baseProperties(
            time = input$time,
            basinWidth = input$basinWidth/2, 
            basinLength = input$basinLength/2 ,
            infiltrationRate = input$infiltrationRate, 
            horizConductivity = input$horizConductivity,
            iniHead = input$iniHead,
            specificYield = input$specificYield,
            numberTimeSteps = input$numberTimeSteps
          )
          return(x)
        }
      )
    } else {
      return(NULL)
    }
  })  
  
  getManagerResult <- reactive({
    #inputs: time, basinWidth, basinLength, infiltrationRate 
    #horizConductivity, iniHead, specificYield, numberTimeSteps
    area <- input$basinWidth*input$basinLength
    modelresult <- runHantush()$dat
    wetcondition <- modelresult$Pegelanstieg > 2 & modelresult$x > 100
    floodcondition <- modelresult$Pegelanstieg > 4
    floodedStretch <- if (any(floodcondition)) 
      diff(range(modelresult$x[floodcondition]))
    else
      0
    infiltration <- area * input$infiltrationRate
    #modelresult[wetcondition, ]
    score <- infiltration * input$time 
      -area 
      -(floodedStretch ^ 3) 
      -(sum(wetcondition) ^ 2)
    
    data.frame(
      "Area" = sprintf("%d m2", area),
      "Infiltration rate" = sprintf("%0.0f m3/d", infiltration),
      "Duration" = sprintf("%d days", input$time),
      "Infiltration volume" = sprintf("%0.0f m3", infiltration * input$time),
      #"Kosten Beckenbau" = 1000,
      "Wet surface" = sprintf("%0.0f m", floodedStretch),
      "Wet cellars" = sum(wetcondition)
      #, "Punkte" = sprintf("%0.0f", floor(score/1000))
    )
  })
  
  runHantush  <-  reactive({ 
  if (!is.null(baseProps()) & !is.null(input$xDistances) & !is.null(input$xSpacing)) {
    
    #isolate(baseProps())
    #isolate(input$xDistances)
    #isolate(input$xSpacing)
    
    res <- list()
    
    withProgress(
      message = '2. Start simulation...', 
      detail = "this may take a few seconds", 
      value = 0.1, 
      expr= {
        res <- hantushDistancesBaseProps(
          x = seq(min(input$xDistances), max(input$xDistances), input$xSpacing), 
          baseProps =  baseProps()
        )
        #names(res$dat)[names(res$dat)=="WLincrease"] <- "Pegelanstieg"
        return(res)
      }
    )
    
  } else {
    return(NULL)
  }}
  )

  blength <- reactive(
    if (!is.null(input$basinLength)) {
      abs(as.numeric(input$basinLength))/2
    }
  )
  
  output$mounding1 <- renderMetricsgraphics({
    
    if(!is.null(runHantush()) & !is.null(blength) & !is.null(input$yMax)) {
      
      #print(paste("output$mounding1: ", isolate(runHantush())))     
      yMax <- input$yMax
      
      # User needs to explicitly set a value different from 0 to fix yMax, otherwise automatised update
      if (yMax == 0) {
        yMax <- NULL 
      }
      
      runHantush()$dat %>%
        mjs_plot(
          x = x,           
          y = WLincrease
        ) %>%
        mjs_line(area = TRUE) %>%
        mjs_add_baseline(y_value = 4, "Ground surface")  %>%
        mjs_add_baseline(y_value = 2, "Cellar")  %>%
        #mjs_add_baseline(y_value = 1, "Roots")  %>%
        mjs_add_marker(0, "Basin")  %>%
        mjs_add_marker(-blength() , "[")  %>%
        mjs_add_marker( blength() , "]")  %>%
        #mjs_add_marker(-200 , "Forest")  %>%
        mjs_add_marker( 100 , "Settlement")  %>%
        mjs_labs(
          x_label = sprintf("Distance from basin center (%s )", lengthUnit()),
          y_label = sprintf("Groundwater level increase (%s )", lengthUnit())
        ) %>%
        mjs_axis_y(min_y = 0, max_y = yMax)
    }
  })
  
  output$managerresult <- renderTable(
    getManagerResult(),
    include.rownames = FALSE
  )
  
  
  output$downloadExploreResults <-  downloadHandler(filename = function() {paste0("exploreResult_", Sys.Date(), ".csv")},
                                                    content = function(file) { 
                                                      result <- runHantush()
                                                      write.csv(result$dat, file=file, row.names=FALSE)}
  )
  
  
  output$downloadExploreReport <- downloadHandler(
    filename = function() {
      paste('exploreReport', sep = '.', switch(
        input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
      ))
    },
    
    content = function(file) {
      src <- normalizePath('reports/explore.Rmd')
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'explore.Rmd')
      
      library(rmarkdown)
      out <- render('explore.Rmd', switch(
        input$format,
        PDF = pdf_document(number_sections = TRUE,toc = TRUE,fig_caption = TRUE), 
        HTML = html_document(toc = TRUE, number_sections = TRUE), 
        Word = word_document(fig_caption = TRUE)
      ))
      file.rename(out, file)
    }
  )

}
