# app.R

source("helpers.R", local = TRUE)
source("ui.R", local = TRUE)
source("server.R", local = TRUE)

shinyApp(ui, server)
