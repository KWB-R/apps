# Login information during session ----
loginData <<- list()
loginData$LoggedIn <- FALSE


#  Demo User ----
# source for encryption: https://gist.github.com/ojessen/8656652


hash <- c("33b62246704f53f5087a3c4f06c02b473ff9a96e68646211aefb0f1026f3f430", 
	      	"9859e8d6541f4f964b951f45c5066b80489b13c0a81ebf8a3be45f01ab60f96c")
salt <- c("200435201.404616694534942.042083",
		      "6287429.01608348391213896.218687")
userTable <<- data.frame(user        = c("hantushWeb", "christoph"),
                         fullName =  c("hantushWeb", "Christoph Sprenger"),
                         login       = hash,
                         nacl        = salt)



# Login user interface ----
loginUI <- function (){
  div(class= "",
      textInput("account", "Account"), 
      passwordInput("pwd", "Passwort"), 
      actionButton("login", label = "Login"),
      br(), br(), br(), br(), br(),
      logo()
   )
}


# Login info during session ----
loginInfo <- function() {
  fluidRow(
    column(6,
           "User: ", strong(loginData$Account),
           "Time: ", strong(loginData$LoginTime)
    ),
    column(1, actionButton("logout", "Logout", size="mini"))
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
          loginData$fullName <<- userTable$fullName[whichUser]
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