# Login information during session ----
loginData <- list()
loginData$LoggedIn <- FALSE


#  Demo User ----
# source for encryption: https://gist.github.com/ojessen/8656652


hash <- "3cb5dace8a8b3cd750b7d377e60711bde0160b4dd18d34205363e7420a0dd4ff"
salt <- "570836723.08363379795766.202733"
userTable <<- data.frame(user        = "wtaqWeb",
                         login       = hash,
                         nacl        = salt)



# Login user interface ----
loginUI <- function (){
  div(class= "",
      textInput("account", "Account"), 
      passwordInput("pwd", "Passwort"), br(), br(),
      actionButton("login", label = "Login")
  )
}


# Login info during session ----
loginInfo <- function() {
  fluidRow(
    column(3,
           "User: ", strong(loginData$Account)
    ),
    column(1, actionButton("logout", "Logout"))
  )
}


# Do login ----
doLogin <- reactive({
  if (!is.null(input$login)) {
    if (input$login > 0) {
      whichUser <- which(userTable$user == input$account)
      if(length(whichUser) > 0) {
        salt <- userTable$nacl[whichUser]
        hash <- digest(paste0(salt, input$pwd), algo="sha256")
        if(hash == userTable$login[whichUser]) {
          loginData$Account <<- input$account
          loginData$Session <<- "Session ID" # TODO
          loginData$LoginTime <<- Sys.time() # TODO
          loginData$LoggedIn <<- TRUE
        }
      }
    }
  }
})


# do logout ----
doLogout <- reactive({
  if (!is.null(input$logout)) {
    if (input$logout > 0) {
      isolate(
        loginData$LoggedIn <<- FALSE
      )
    }
  }
})