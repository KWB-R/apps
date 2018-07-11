library(shiny)
library(shinythemes)
library(digest)
library(metricsgraphics)
library(leaflet)
library(kwb.hantush)
library(rmarkdown)


# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 9MB.
options(shiny.maxRequestSize = 9*1024^2)


# Global settings (for all sessions)
theme <<- shinytheme("readable")
#theme <<- "bootstrap.css"

# logo -------------------------------------------------------------------------
logo <<- function
(
  src="kwb.png", 
  target = "_blank", ### opens new tab/window 
  href="http://www.kompetenz-wasser.de", 
  align="middle", 
  label = "KWB_homepage",
  add_div = TRUE,
  ... ### add. arguments passed to img(), e.g. height=40  
) 
{  
  x <- a(
    target = target, 
    href = href,  
    onclick = sprintf("ga('send', 'event', 'click', 'link', '%s', 1)", label),
    img(src = src, align = align, ...)
  )
  
  if (add_div) {
    x <- div(x)
  }
  
  return(x)
}

# usgsLogo ---------------------------------------------------------------------
usgsLogo <- logo(
  src = "usgsReport.png", 
  href = "http://pubs.usgs.gov/sir/2010/5102/support/sir2010-5102.pdf",
  label = "USGS_report", 
  add_div = FALSE
)

# footer -----------------------------------------------------------------------
footer <- function 
(
  startCol = 9, 
  txt = "\u00A9 Kompetenzzentrum Wasser Berlin gGmbH 2015"
)
{
  footerTxt <- tags$footer(tags$h6(txt))
  x <- fixedRow(column(width = 12-startCol, footerTxt, offset = startCol))
  
  return(x)
}

# reference <- tabPanel("Reference", tags$div(class="header", checked=NA,
#                                             tags$a(target="_blank")tags$img(src="usgsReport.png", 
#                                         href="http://pubs.usgs.gov/sir/2010/5102/support/sir2010-5102.pdf")))

# reference --------------------------------------------------------------------
reference <- tabPanel("Reference", tags$div(usgsLogo))

# shinyServer ------------------------------------------------------------------
shinyServer(function(input, output, session) {

  # Local settings (for each session)  
  # Tools ----
  source("tools/login.R", local = TRUE)
  
  # Modules ----
  source("module/explore.R", local = TRUE)
  source("module/spiel.R", local = TRUE)
  source("module/hantushSpiel.R", local = TRUE)
  #source("module/highscore.R", local=TRUE)
  source("module/kwb.R", local = TRUE)
  # Data ----
  load("data/spiel.RData")
  
  # main page ----
  output$mainPage <- renderUI({
    
    #doLogin()
    
    if (loginData$LoggedIn == FALSE) {
      
      #doLogout()
      
      server_explore(input, output, session)
      server_spiel(input, output, session)
      #server_highscore(input, output, session)
      server_kwb(input, output)
      
      div(
        class = "",
        fluidPage(
          #fluidRow(column(12, column(4, br(), loginInfo()), br(), br(), logo(align = "right"))),
          navbarPage(
            title = "Grundwasseranreicherungsmanager",
            windowTitle = "Grundwasseranreicherungsmanager",
            tabPanel(
              "Erforschen", br(), 
              div(class = " ", ui_explore()), 
              id = "explore"
            ),
            tabPanel(
              "Spiel", br(), 
              div(class = " ", ui_spiel()), 
              id = "spiel"
            ),
#             tabPanel(
#               "Highscore", br(), 
#               div(class = " ", ui_highscore()), 
#               id = "highscore"
#             ),
            tabPanel(
              "Hintergrund", br(), 
              div(class = " ", reference), 
              id = "hintergrund"
            ),
            tabPanel(
              "KWB", br(), 
              div(class = " ", ui_kwb(output)),
              id = "kwb"
            ),
            #navbarMenu("More",
            #            reference,
            #            ui_kwb(output)),
            theme = theme,
            footer = footer()
          )
        )
      )
    } else {
      fluidPage(
        fluidRow(
          column(
            1, offset = 5, 
            br(), br(), br(), br(), 
            h5("Login"), 
            loginUI(), br()
          )
        ),
        header = tags$style(type = "text/css", "well { width: 100%; }"),
        theme = theme
      )
    }
  })
})
