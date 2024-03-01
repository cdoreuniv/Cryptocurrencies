sidebar = function(){
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem(
        "Présentation",
        tabName = "presentation",
        startExpanded = TRUE,
        menuSubItem("Présentation des cryptomonnaies", tabName = "pres1"),
        menuSubItem("Informations", tabName = "info")
      ),
      menuItem(
        "Analyse exploratoire des rentabilités",
        tabName = "analysexplo",
        startExpanded = TRUE,
       # menuSubItem("Présentation des rentabilités", tabName = "pres2"),
        menuSubItem("Statistiques descriptives", tabName = "statdesc")
      ),
      menuItem("Estimation des modèles de volatilité", tabName = "volatility")
      
    )
  )

}
