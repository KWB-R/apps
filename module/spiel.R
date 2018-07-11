# ui_explore -------------------------------------------------------------------
ui_spiel <- function(...) 
{
  fluidPage(
    
    title = "Spiel",
    sidebarLayout(
      sidebarPanel(         
        sliderInput("beckenPosition",
                    label = "Beckenposition",
                    value = 0,
                    min = -300, 
                    max = 100, 
                    step = 0.5,
                    post = " Meter"),
        actionButton("go", "Anzeigen"),
        
        checkboxInput("spiel", "Details", value = FALSE),
        
        conditionalPanel(
          
          condition = "input.spiel",
          
          h6("Nachwuchsforscherdaten"),
          
          textInput(
            inputId = "managerName",
            label = "Name", 
            value = "Max Mustermann"
          ),
          
          textInput(
            inputId = "alter",
            label = "Alter", 
            value = "10"
          ),
          
          
          checkboxInput(
            inputId = "female",
            label = "weiblich"
          ),
          
          textInput(
            inputId = "managerMail",
            label = "E-Mail", 
            value = "max.mustermann@kwb.de"
          ),
          
          h6("Herunterladen"),
          
          downloadButton(
            outputId = "downloadSpielErgebnis", label = "Ergebnisse"
          ),
          
          downloadButton(
            outputId = "downloadProjektBericht", label = "Projektbericht"
          ),
          
          radioButtons(
            'format', 'Berichtsformat', c('PDF', 'HTML', 'Word'), inline = TRUE
          )          
        )),
      mainPanel(
        h3("Spielergebnis"),
        plotOutput("spielPlot")
      )
    ))
  
}

# server_explore ---------------------------------------------------------------
server_spiel <-  function(...) 
{  
  berichtName <<- reactive({
    prefix <- paste0(
      "Nachwuchswissenschaftler", ifelse(input$female, "in", "")
    )
    paste(prefix, input$managerName)
  })
  
  #   gameResult <- reactive({
  #   data.frame(Name = input$managerName, Email = input$managerMail, Weiblich = input$female)
  #   })
  gameResult <- eventReactive(input$go, {
    hanstushSpiel(gwa$dat, beckenPosition = input$beckenPosition, flurabstand = 13)
    
  })
  
  output$downloadSpielErgebnis <-  downloadHandler(
    filename = function() {
      paste0("spielErgebnis_", 
             gsub(pattern = " ", replacement = "_", input$managerName), 
             ".csv")
    },
    content = function(file) { 
      #spielErgebnis <- gameResult()
      spielErgebnis <- data.frame(
        Name = input$managerName,
        Alter = input$alter,
        Weiblich = input$female, 
        Email = input$managerMail,
        Beckenposition = input$beckenPosition)
      
      spielErgebnis <- cbind(spielErgebnis, as.data.frame(getFloodInCellar(gameResult =  gameResult(), 
                                                                           houseConfig = houseConfig )))
      
      write.csv(spielErgebnis, file=file, row.names=FALSE)
    }
  )
  
  
  output$downloadProjektBericht <- downloadHandler(
    filename = function() {
      bericht <- paste( 
        "Projektbericht_Keine_nassen_Keller_", 
        gsub(pattern = " ", replacement = "_", input$managerName)
      )
      paste(
        bericht, 
        sep = '.', 
        switch(input$format, PDF = 'pdf', HTML = 'html', Word = 'docx')
      )
    },
    
    content = function(file) {
      src <- normalizePath('reports/spiel.Rmd')
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'spiel.Rmd')
      
      
      out <- render(
        'spiel.Rmd', 
        switch(
          input$format,
          PDF = pdf_document(number_sections = TRUE,toc = FALSE,fig_caption = TRUE), 
          HTML = html_document(toc = FALSE, number_sections = TRUE), 
          Word = word_document(fig_caption = TRUE)
        )
      )
      file.rename(out, file)
    }
  )
  
  output$spielPlot <- renderPlot({
    plotGameResult(gameResult=gameResult(), houseConfig, xlim = c(-400, 300), 
                   basinLength = gwa$baseProps$basinLength
    )})
  
}
