library(digest)
library(shiny)


ui <- basicPage(
  uiOutput("register"),
  actionButton("doRegister",label="Register"),
  verbatimTextOutput("resultRegister"),
  uiOutput("login"),
  actionButton("doLogin",label="Login"),
  verbatimTextOutput("receivedInput"),
  tableOutput("passwd")
)


server <- function(session, input, output) {
  output$register = renderUI(wellPanel(
    textInput("userNameRegister", label="User name",value=""),
    textInput("userFullNameRegister", label="user full name",value=""),
    textInput("loginRegister", label="Password",value="")
  ))
  output$resultRegister = renderText({
    if(input$doRegister>0){
      # I'm following the recommendations in
      # https://crackstation.net/hashing-security.htm
      salt = paste0(runif(1, max = 1e9),runif(1, max = 1e9))
      hash = digest::digest(paste0(salt,input$loginRegister), algo="sha256")
      # userTable as standin for the table in the database.
      # The aim is to avoid having to
      # login_clear obviously is for testing the behaviour,
      # and not supposed to be in a real app.
      if(!exists("userTable", envir=.GlobalEnv)){
        userTable <<- data.frame(user = input$userNameRegister,
                                 fullName = input$userFullNameRegister,
                                 login = hash,
                                 login_clear = input$loginRegister,
                                 nacl = salt)
      } else {
        userTable <<- rbind(userTable,
                            data.frame(user = input$userNameRegister,
                                       fullName = input$userFullNameRegister,
                                       login = hash,
                                       login_clear = input$loginRegister,
                                       nacl = salt))
      }
      paste("User name: ", input$userName, "registered.")
    }
  })
  output$login = renderUI(wellPanel(
    textInput("userName", label="User name",value=""),
    textInput("login", label="Password",value="")
  ))
  output$receivedInput = renderText({
    if(input$doLogin > 0) {
      success = FALSE
      whichUser = which(userTable$user == input$userName)
      if(length(whichUser)>0){
        salt = userTable$nacl[whichUser]
        hash = digest(paste0(salt,input$login), algo="sha256")
        if(hash == userTable$login[whichUser]) {
          success = TRUE
        }
      }
      if(success) {
        paste("Login for user ", input$userName, "successfull", hash, salt)
      } else
      {
        paste("Login for user ", input$userName, "failed", hash, salt)
      }
    }
  })
  output$passwd = renderTable({
    userTable
  })
}

runApp(list(ui = ui, server = server), port = 4054) 