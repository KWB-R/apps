ui_wtaqWeb <- function() {
  fluidPage(title = "WTAQ for dummies", responsive = TRUE,
                       sidebarLayout(
                         sidebarPanel( h5("Model parameterisation"), 
                                       fileInput("loadWTAQconfig", label = "Upload WTAQ configuration",multiple = FALSE),
                                       h6("Aquifer"),
                                       selectInput(inputId = "aqtype",
                                                   label = "Aquifer type", 
                                                   choices = c("WATER TABLE", "CONFINED"), 
                                                   multiple = FALSE),
                                       sliderInput(inputId = "bb", min = 5,max = 100,step = 1,
                                                    label = "Initial saturated aquifer thickness (m)",
                                                    value = 40),
                                       numericInput(inputId = "hkr", min= 0.00001, max = 0.1,step = 0.00001,
                                                    label = "Horizontal aquifer conductivity (m/s)",
                                                    value = 0.0007),
                                       numericInput(inputId = "hkz",min = 0.0000001,max = 0.1,step = 0.0000001,
                                                    label = "Vertical aquifer conductivity (m/s)",
                                                    value = 0.0007),
                                       sliderInput(inputId = "ss",min = 0.000001,max = 0.001,step = 0.000001,
                                                    label = "Specific storage (1/m)",
                                                    value = 0.0001),
                                       sliderInput(inputId = "sy",min = 0,max = 0.3,step = 0.01,
                                                    label = "Specific yield",
                                                    value = 0.2),
                                       
                                       h6("Production well"), 
                                       sliderInput(inputId = "qq",min = 1,max = 1000,step = 1,
                                                    label = "Production rate (m3/h)",
                                                    value = 250),
                                       sliderInput(inputId = "rw",min = 0.1, max = 3,step = 0.1,
                                                    label = "Well radius (m)",
                                                    value = 0.5),
                                       numericInput(inputId = "zpd",
                                                    label = "Top of filter screen below initial water table (m)",
                                                    value = 0),
                                       numericInput(inputId = "zpl",
                                                    label = "Bottom of filter screen below initial water table (m)",
                                                    value = 40),
                                       sliderInput(inputId = "sw",min = 0, max = 20,step = 0.05,
                                                    label = "Well skin ",
                                                    value = 0),
                                       actionButton(inputId = "run",
                                                    label =  "Run WTAQ"),
                                       downloadButton(outputId = "downloadWTAQconfig", 
                                                      label = "Download WTAQ configuration")
                         ),
                          mainPanel("WTAQ results", 
                                    plotOutput("drawdown"),
                                    h6("WTAQ configuration:"),
                                    #tableOutput("contents"),
                                    verbatimTextOutput("configuration")
                          )
                        )
        )
}


generalConfiguration <- wtConfigureGeneral(format = "DIMENSIONAL")
### title of the project (max. length 70 characters)
#title=myTitle

aquiferConfiguration <- wtConfigureAquifer(    
  aqtype = "WATER TABLE", # aquifer type
  bb = 40,                # saturated aquifer thickness
  hkr = 0.007,            # horizontal hydraulic conductivity
  hkz = 0.007,          # vertical hydraulic conductivity
  ss = 1E-05,             # specific storage
  sy = 0.2               # specific yield 
)

drainageConfiguration <- wtConfigureDrainage(
  idra = 0 # = instantaneous drainage in unsaturated zone
)

timesConfiguration <- wtConfigureTimes(its=0, tlast = 1000000000, nlc = 8, nox = 9
)



pumpwellConfiguration <- wtConfigurePumpwell(pwname = "0",
                                             ### partially penetrating pumped well
                                             ipws = 0,
                                             ### finite diameter well
                                             ipwd = 0, 
                                             ### pumping rate of production well in (here: m?/s)
                                             qq = 250/3600, 
                                             ### radius of pumped well-screen (here: meter) 
                                             rw = 0.5, 
                                             ### top of filter screen below initial water table (here: meter)
                                             zpd = 20, 
                                             ### bottom of filter screen below initial water table (here: meter)
                                             zpl = 40, 
                                             ### well-bore skin parameter (dimensionless)
                                             sw = 0
                                             ### data.frame with times and measured drawdown data in pumping well
)
observationWells <- list()
myDistances <- c(1,2,5,10,25,50,100,200,300,400,600,1000,1500,2000,4000)

for (i in 1:length(myDistances))
{
  distance <- myDistances[i]
  observationWells[[i]] <- wtConfigureObservationWell(
    ### name of observation well
    obname = as.character(distance), 
    ### distance from pumping well (here: meters)
    r = distance, 
    ### partially penetrating observation well
    iows = 2, 
    ### delayed response
    idpr = 1, 
    ### top of filter screen below initial water table (here: meters)
    z1 = 0, 
    ### bottom of filter screen below initial water table (here: meters)
    z2 = 40, 
    ### inside radius of the observation well (here: meters)
    rp = 0.2
  )
}


wtaqConfiguration <<- wtConfigure(
  general = generalConfiguration,
  aquifer = aquiferConfiguration, 
  drainage = drainageConfiguration, 
  times = timesConfiguration, 
  solution = wtConfigureSolution(),
  pumpwell = pumpwellConfiguration,
  obswells = observationWells)



server_WtaqWeb <-  function() {


  
#  config  <-  
#     reactive(  { 
#     
#     inFile <- input$loadWTAQconfig
#     
#     if (is.null(inFile))
#     {
#     withProgress(message = "Setting default model parameterisation",
#                  detail = "This may take a few moments...", {
  

      
#      wtaqConfiguration   
#       setProgress(detail = "Done...")
      
#     }
#  else {
#       #withProgress(message = "Reading model parameterisation",
#       #              detail = "This may take a few moments...", {
#       wtReadInputFile(input$loadWTAQconfig, dbg=TRUE)
#       wtReadInputFile(inFile$datapath)
#       #setProgress(detail = "Done...")
#     })}
#   }
#   )

  modifyConfig  <-  reactive(  {
     if (input$run==0)
       return()
     else 
       newConf <- list()
      withProgress(message = "Updating model parameterisation...", value = 0, expr={newConf <- wtSetParameters(configuration = wtaqConfiguration, assignments = list(hkr=input$hkr, 
                                                                            hkz=input$hkz,                                                                     
                                                                            bb=input$bb,
                                                                            ss=input$ss,
                                                                            sy=input$sy,
                                                                            qq=input$qq/3600,
                                                                            rw=input$rw, 
                                                                            rc=input$rw,
                                                                            zpd=input$zpd, 
                                                                            zpl=input$zpl, 
                                                                            sw=input$sw))
 
    newConf$aquifer$aqtype <- input$aqtype    
    })
    newConf
  })
  



  output$drawdown <- renderPlot(   {  
    if (input$run==0)
      return()
    else 
  isolate({

    withProgress(message = 'Running WTAQ...', detail = "this takes ~ 4 seconds", value = 0.1, expr= {
    res <- wtRunConfiguration(configuration = modifyConfig())
    setProgress(0.8,message = "Plot results...",detail = "")
    wtPlotResult(res,plottype = "w")
    })
  })
})

output$configuration <- renderPrint({
  if (input$run==0)
    return(print("No WTAQ run performed yet!"))
  else 
    withProgress(message = "Printing model parameterisation...", value = 0.9, expr={
      print(modifyConfig())
      setProgress(1, detail = "Done!") 
    })
})


  
  output$downloadWTAQconfig <- downloadHandler(
    filename = function() {
      paste("wtaq_", Sys.Date(), ".inp", sep="")
    },
    content = function(file) {
      writeLines(wtInputFileLines(modifyConfig()), file)
    }, 
    contentType="text")
}
