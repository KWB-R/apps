library(shiny)
library(digest)
library(kwb.wtaq)

user <- unname(Sys.info()["user"])
if (user == "shiny") {
  
  # Set library locations
  .libPaths(c(
    "/home/mrustl/R/packages",
    "/usr/local/lib/R/site-library", 
    "/usr/lib/R/site-library",
    "/usr/lib/R/library")
  )
  
}

# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 9MB.
options(shiny.maxRequestSize = 9*1024^2)


shinyServer(function(input, output, session) {
  
  # Tools ----
  source("tools//login.R", local=TRUE)
  
  # Modules ----
  source("module//wtaqWeb.R", local=TRUE)
  
  # main page ----
  output$mainPage <- renderUI({
    doLogin()
    if (loginData$LoggedIn==FALSE) {
      doLogout()
      server_WtaqWeb()
      div(class="",
          fluidPage(fluidRow(column(12, column(4, br(), loginInfo()))),
                    navbarPage(title="Interactive WTAQ",
                               tabPanel("All-in-one", br(), div(class=" ", ui_wtaqWeb()), id="subGroup"))
          ))
    } else {
      fluidPage(fluidRow(column(1, offset=5, br(), br(), br(), br(), h5("Login"), loginUI())),
                header= tags$style(type="text/css", "well { width: 100%; }"))
    }
  })
})



