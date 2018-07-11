uiAttr <- function(label = NULL,
                   min = NULL,
                   max = NULL,
                   value = NULL,
                   step = NULL,
                   animate = FALSE) {
  
  x <- list(label = label, 
            min = min,
            max = max,
            value = value,
            step = step,
            animate = FALSE)
  
}

initialConfig <- function (unitLength = uiAttr(label = "Length unit",
                                               value = "m"),
                           unitTime = uiAttr(label = "Time unit",
                                             value = "d"),
                           time = uiAttr(label = "Time",
                                         min = 0.1,
                                         max = 100,
                                         value = 10,
                                         step = 0.1,
                                         animate = TRUE),
                           basinLength = uiAttr(label = "Basin length",
                                                min = 5, 
                                                max = 100, 
                                                value = 10, 
                                                step = 1),
                           basinWidth = uiAttr(label = "Basin width",
                                               min = 5, 
                                               max = 100, 
                                               value = 10, 
                                               step = 1),
                           infiltrationRate = uiAttr(label = "Infiltration rate",
                                                     min = 0.1,
                                                     max = 10,
                                                     value = 1,
                                                     step = 0.1),
                           horizConductivity = uiAttr(label = "Horizontal aquifer conductivity",
                                                      max = 1000,
                                                      value = 10,
                                                      step = 0.1),
                           iniHead = uiAttr(label = "Initial saturated aquifer thickness",
                                            min = 1,
                                            max = 100,
                                            value = 10,
                                            step = 1),
                           xDistances = uiAttr(label ="x min/max distances",
                                               min = -1000,
                                               max = 1000, 
                                               value = c(-200,200)
                                               ),
                           xSpacing = uiAttr(label ="x spacing",
                                             min = 1,
                                             max = 50, 
                                             value = 10),
                           yMax = uiAttr(label = "max Y value (m)",
                           min = 0,
                           max = 100,
                           step = 0.25,
                           value = 0)
                           
                           
                           ) {
  x <- list(unitLength = unitLength, 
            unitTime = unitTime, 
            time = time,
            basinLength = basinLength, 
            basinWidth = basinWidth,
            infiltrationRate = infiltrationRate, 
            horizConductivity = horizConductivity,
            iniHead = iniHead,
            xDistances =  xDistances,
            xSpacing = xSpacing,
            yMax = yMax
            )
  return(x
}



  