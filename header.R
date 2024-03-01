header = function(){
  dashboardHeader(
    # Titre
    title = "Rentabilités des cryptomonnaies",
    titleWidth = 300,
    # Menu tâches accomplies
    dropdownMenu(
      type = "tasks", 
      badgeStatus = "success",
      taskItem(value = 100, color = "green",
               "Documentation"
      ),
      taskItem(value = 100, color = "green",
               "Server deployment"
      ),
      taskItem(value = 100, color = "green",
               "Overall project"
      )
      
    )
  )
}