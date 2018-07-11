# ui_explore -------------------------------------------------------------------
ui_explore <- function(...) 
{
  fluidPage(
    
    includeCSS("www/metricsgraphics_user.css"),
    
    title = "Hantush",
    sidebarLayout(
      sidebarPanel(         
        
        h3("Modellkonfiguration"),
        uiOutput('ui_time'),        
        
        #h4("Infiltrationsbecken"),
        uiOutput('ui_basinLength'),
        uiOutput('ui_basinWidth'),
        uiOutput('ui_infiltrationRate'),
        
        #h4("Grundwasserleiter"),
        uiOutput('ui_horizConductivity'),
        
        actionButton(
          "reset_input", 
          "Parameter zurücksetzen"
        ),
        
        checkboxInput("details", "Details", value = FALSE),
        
        conditionalPanel(
          
          condition = "input.details",
          
          h5("Basiskonfiguration"),
          
          h6("Einheiten"),
          
          textInput(
            inputId = "unitLength",
            label = "Längeneinheit", 
            value = "Meter"
          ),
          
          textInput(
            inputId = "unitTime",  
            label = "Zeiteinheit", 
            value = "Tag"
          ),
          
          #textInput(
          #  inputId = "plotheight",  
          #  label = "Hoehe der Grafik in Pixel", 
          #  value = "400"
          #),
          
          uiOutput('ui_numberTimeSteps'),
          
          uiOutput('ui_iniHead'),
          uiOutput('ui_specificYield'),

          h6("Coordinates"),
          
          uiOutput('ui_xDistances'),
          uiOutput('xSpacing'),
          
          h6("Plot"),
          
          uiOutput('yMax')          
        )
      ),
      mainPanel(
        h3("Modellsimulation"), 
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
    if (input$unitTime == "") "Tag" else sprintf(" %s", input$unitTime)
  )

  ### Set "m" as default value even if ""
  lengthUnit <- reactive(
    if (input$unitLength=="") "Meter" else sprintf(" %s", input$unitLength)
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
        label = "Zeit seit Infiltrationsbeginn",
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
        label = "Länge des Beckens",
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
        label = "Breite des Beckens",
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
        label = "Infiltrationrate",
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
        label = "Leitfähigkeit des Grundwasserleiters",
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
        label = "Anfangsmächtigkeit des Grundwasserleiters",
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
        label = "Effektive Porosität des Grundwasserleiters",
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
        label = "Anzahl Zeitschritte",
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
        message = "1. Aktualisiere Modellkonfiguration...", 
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
      "Fläche" = sprintf("%d m2", area),
      "Infiltration" = sprintf("%0.0f m3/d", infiltration),
      "Dauer" = sprintf("%d Tage", input$time),
      "Anreicherung" = sprintf("%0.0f m3", infiltration * input$time),
      #"Kosten Beckenbau" = 1000,
      "Überflutete Strecke" = sprintf("%0.0f m", floodedStretch),
      "Nasse Keller" = sum(wetcondition)
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
      message = '2. Starte Berechnung...', 
      detail = "einen Moment Geduld bitte", 
      value = 0.1, 
      expr= {
        res <- hantushDistancesBaseProps(
          x = seq(min(input$xDistances), max(input$xDistances), input$xSpacing), 
          baseProps =  baseProps()
        )
        names(res$dat)[names(res$dat)=="WLincrease"] <- "Pegelanstieg"
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
          y = Pegelanstieg # WLincrease
        ) %>%
        mjs_line(area = TRUE) %>%
        mjs_add_baseline(y_value = 4, "Erdoberfläche")  %>%
        mjs_add_baseline(y_value = 2, "Keller")  %>%
        #mjs_add_baseline(y_value = 1, "Wurzeln")  %>%
        mjs_add_marker(0, "Becken")  %>%
        mjs_add_marker(-blength() , "[")  %>%
        mjs_add_marker( blength() , "]")  %>%
        #mjs_add_marker(-200 , "Wald")  %>%
        mjs_add_marker( 100 , "Siedlung")  %>%
        mjs_labs(
          x_label = sprintf("Entfernung vom Beckenmittelpunkt (%s )", lengthUnit()),
          y_label = sprintf("Anstieg des Grundwasserstandes (%s )", lengthUnit())
        ) %>%
        mjs_axis_y(min_y = 0, max_y = yMax)
    }
  })
  
  output$managerresult <- renderTable(
    getManagerResult(),
    include.rownames = FALSE
  )
}
