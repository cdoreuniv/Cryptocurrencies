source("librairies.R")
source("header.R")
source("sidebar.R")
source("body.R")
source("fonction.R")

ui = dashboardPage(
  header(),
  sidebar(),
  body()
)