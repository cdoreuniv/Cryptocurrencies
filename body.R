library(shinydashboard)

body = function() {
  dashboardBody(
    tags$head(
      tags$script(src = "howler.js/dist/howler.min.js"),
      ),
    includeCSS("styles.css"),
    tabItems(
      tabItem(tabName = "pres1",
             # h2("Présentation des cryptomonnaies"),
              p("Le but de cette application est d'analyser et comparer trois différentes 
               cryptomonnaies au niveau de leurs rentabilités. Les cryptomonnaies sont des monnaies numériques conçues 
                pour fonctionner comme un moyen d'échange décentralisé. 
              Voici une présentation générale des cryptomonnaies : \n 
              L'utilisateur a le choix entre 3 cryptocurrencies :
                le Bitcoin, l'Ethereum et le BNB. 
                \n
                L'utilisateur choisit la date d'étude des séries. On considère que pour une meilleure analyse et en vue
                d'une prévision de la Value at Risk, il faut laisser un an à partir de la date de connexion 
                de l'utilisatuer. Il peut toutefois, dans la première page, représenter les variations
                des rentabilités de la cryptocurrencie choisie depuis sa création jusqu'à la veille de la 
                date de sa connexion. \n
                En ce qui concerne les deux autres pages, l'utilisateur doit laisser au moins un an 
                à compté de sa date de connexion. \n
                Les résultats des modèles mis en place en page 3 permettent de déterminer, sur une période donnée,
                le modèle GARCH qui est accepté pour une prévision. "),
             p("Pour améliorer cette application, avec du recul, il aurait fallut pouvoir calculer la VaR
               dans un nouvel onglet car c'est l'une des informations les plus importantes. J'aurais aussi voulu
               mettre en place une carte des taux de change entre des différentes cryptomonnaies et les monnaies 
               utilisées dans chaque pays. "),
               
              
            h3("Qu'est-ce qu'une cryptomonnaie ?"),
            tags$ul(
            tags$li("Décentralisation : Contrairement aux monnaies traditionnelles émises 
               par les gouvernements (fiat), les cryptomonnaies ne sont pas contrôlées 
               par une autorité centrale comme une banque centrale."),

            tags$li("Technologie de blockchain : Les cryptomonnaies reposent généralement 
            sur la technologie de la blockchain, qui est un registre public et 
            sécurisé de toutes les transactions effectuées. Cela rend les transactions 
            transparentes et immuables."),

            tags$li("Sécurité : La cryptographie est utilisée pour sécuriser les transactions 
            et contrôler la création de nouvelles unités. Cela rend les cryptomonnaies 
            difficiles à falsifier.")),

            h3("Utilisations des cryptomonnaies :"),
            tags$ul(
              tags$li("Investissement : De nombreuses personnes achètent des cryptomonnaies 
            dans l'espoir que leur valeur augmentera avec le temps."),

              tags$li("Transactions : Certaines cryptomonnaies sont utilisées pour des 
            transactions peer-to-peer, sans nécessiter de tiers de confiance comme les 
            banques."),

              tags$li("Contrats intelligents : Des plateformes comme Ethereum permettent la 
            création et l'exécution de contrats intelligents, qui sont des programmes 
            auto-exécutants avec des conditions prédéfinies."),

              tags$li("Tokenisation : Les entreprises utilisent parfois des cryptomonnaies pour 
            créer des tokens représentant des actifs physiques ou virtuels, facilitant 
            ainsi le commerce de ces actifs.")
            ),

            h3("Défis et considérations :"),
            tags$ul(
              tags$li("Volatilité : Les prix des cryptomonnaies peuvent être extrêmement 
              volatils, ce qui en fait un investissement risqué."),

              tags$li("Sécurité : Les portefeuilles de cryptomonnaies doivent être sécurisés 
              correctement pour éviter le vol de fonds."),

              tags$li("Régulation : Les gouvernements du monde entier commencent à réglementer 
              les cryptomonnaies, ce qui peut avoir un impact sur leur utilisation et 
              leur valeur.")
            )
),
      tabItem(
        h2("Informations et représentation des cryptocurrencies"),
        p("L'utilisateur doit choisir la cryptomonnaies d'intérêt afin d'obtenir des 
          informations la concernant. "),
        tabName = "info",
        fluidRow(
          #column(width = 6,
          box(
            width = 4,
            selectInput(
              "database",
              "Sélectionnez la base de données",
              choices = c(
                "Bitcoin"  =  "BTC",
                "Ethereum"  =  "ETH",
                "BNB"  =  "BNB"
              ))
              
            
          )
          #)
          ,
          #column(width = 9,
          box(width = 8,
              uiOutput("explication"),
              hr(),
              # Graphique ou toute autre sortie en fonction de la base de données sélectionnée
              plotlyOutput("coursactif"))
          # )
        )),
      
      tabItem(tabName = "analysexplo",
              h1("Analyse exploratoire des rentabilités")),
      
      tabItem(
        h2("Statistiques descriptives"),
        p("L'utilisateur doit sélectionner une période minimale de 5 ans pour avoir 
        suffisamment de données. Il est possible d'aller jusqu'à un an avant la date du jour.\n
          Le graphique représente les ditributions des rentabilités des séries brute et corrigée
          des points atypiques. Il est donc possible d'identifier les dates des points atypiques les
          plus remarquables pour les expliquer. Si aucune justification économique ne peut être apportée,
          préferez l'utilisation de la série corrigée."),
        tabName = "statdesc",
        fluidRow(
          sidebarPanel(
            selectInput(
              "nom_database2",
              label = h5("Sélectionner la base de données"),
              choices = c(
                "Bitcoin"  =  "BTC",
                "Ethereum"  =  "ETH",
                "BNB"  =  "BNB"
              )
            ),
            dateRangeInput(
              "dates",
              label = h5("Sélectionner une période"),
              start = "2018-02-02",
              end = "2023-02-01"
            ),
            textOutput("message")
          ),
          
          box(width = 8,
              plotlyOutput("discout1"))
        ),
        
        
        
        hr(),
        fluidRow(
          valueBoxOutput("moyenne", width = 2),
          valueBoxOutput("mediane", width = 2),
          valueBoxOutput("minimum", width = 2),
          valueBoxOutput("maximum", width = 2),
          valueBoxOutput("etype", width = 2),
          valueBoxOutput("skewness", width = 2)
        ),
        #hr(),
        fluidRow(
          valueBoxOutput("exkurto", width = 2),
          valueBoxOutput("v1", width = 2),
          valueBoxOutput("v2", width = 2),
          valueBoxOutput("jb", width = 2),
          valueBoxOutput("bp", width = 2),
          valueBoxOutput("q5", width = 2)
        )
        
        
        
      ),
      tabItem(
        tabName = "volatility",
          p("L'évaluation des modèles GARCH (Generalized Autoregressive Conditional Heteroskedasticity), 
          IGARCH (Integrated GARCH), et GJR-GARCH (Glosten-Jagannathan-Runkle GARCH) sur 
          les rendements d'un actif présente plusieurs intérêts dans l'analyse financière et 
          l'estimation de la volatilité future. Voici quelques raisons clés pour lesquelles ces 
          modèles sont largement utilisés :"),
            
            tags$ol(
              tags$li("Modélisation de la Volatilité"),
                tags$ul(
                tags$li("Volatilité Hétéroscédastique : Les marchés financiers présentent 
                souvent des caractéristiques de volatilité hétéroscédastique, ce qui 
                signifie que la volatilité des rendements peut varier au fil du temps. 
                Les modèles GARCH permettent de modéliser cette variation de la 
                volatilité."),
          
          tags$li("Détection des Clusters de Volatilité : Les modèles GARCH peuvent 
          identifier les périodes de volatilité élevée ou basse, ce qui est crucial 
          pour les investisseurs cherchant à comprendre les moments où les prix peuvent 
          être plus risqués ou plus stables.")),
          
          tags$li("Prévision de la Volatilité Future"),
          tags$ul(
            tags$li("Estimation de la Volatilité : Les modèles GARCH peuvent fournir des 
            estimations de la volatilité future, ce qui est utile pour la gestion des 
            risques et la prise de décision en matière d'investissement."),
            tags$li("Construction de Scénarios : En utilisant les prévisions de 
            volatilité, les investisseurs peuvent construire des scénarios de risque 
            pour évaluer l'impact potentiel de différentes conditions de marché sur 
            leurs portefeuilles.")),
          
          tags$li("Gestion des Risques"),
          tags$ul(
            tags$li("Allocation d'Actifs : Les gestionnaires de portefeuille peuvent 
            utiliser les estimations de volatilité pour ajuster l'allocation d'actifs 
            afin de mieux gérer les risques et les rendements attendus."),
            tags$li("Calcul du VaR : Le Value at Risk (VaR) est une mesure clé de 
            gestion des risques qui repose souvent sur des estimations de la volatilité. 
            Les modèles GARCH peuvent aider à calculer le VaR pour différents niveaux de 
            confiance.")),
          
          tags$li("Analyse de la Corrélation"),
          p("Analyse de la Corrélation Conditionnelle : Les modèles GARCH 
          peuvent être utilisés pour étudier la corrélation conditionnelle entre 
          les rendements, ce qui peut être important pour la diversification du 
          portefeuille."),
          
          tags$li("Tests d'Hypothèses"),
          p("Tests d'Efficience du Marché : Les modèles GARCH peuvent être utilisés 
          pour tester l'hypothèse d'efficience du marché en examinant la distribution 
          des résidus et en évaluant la présence éventuelle d'opportunités d'arbitrage."),
          
          tags$li("Comparaison de Modèles"),
          p("Sélection du Meilleur Modèle : En comparant les performances des modèles 
          GARCH, IGARCH, et GJR-GARCH, les analystes peuvent déterminer quel modèle offre 
          la meilleure adéquation aux données et les meilleures prévisions de volatilité.")
          ),
        p("En résumé, l'évaluation des modèles GARCH sur les rendements d'un actif 
        présente un intérêt majeur dans la modélisation de la volatilité, la gestion des 
        risques, la prévision de la volatilité future, l'analyse de la corrélation, les tests 
        d'hypothèses, et la comparaison de modèles. Ces outils sont précieux pour les 
        investisseurs, les gestionnaires de portefeuille et les analystes financiers pour 
        prendre des décisions éclairées en matière d'investissement et de gestion des risques.")
        ,
        hr(),
        #fluidRow(
          box(width = 4,
          selectInput(
            "nom_database3",
            label = h5("Sélectionner la base de données"),
            choices = c(
              "Bitcoin"  =  "BTC",
              "Ethereum"  =  "ETH",
              "BNB"  =  "BNB"
            )
            )
          ),
          box(width = 4,
          dateRangeInput(
            "dates2",
            label = h5("Sélectionner une période"),
            start = "2018-02-02",
            end = "2023-02-01")
          ),
          box(
            width = 4,
            checkboxGroupInput("models", "Sélectionner les modèles GARCH à estimer:",
                           choices = c("GARCH" = "sGARCH", "iGARCH" = "iGARCH", "Risk-metrics" = "Risk-metrics", "GJR-GARCH" = "gjrGARCH"),
                           selected = c("sGARCH", "gjrGARCH"))),

       # ),
        
    #
      ## Tableau des résultats avec une distribution Normale
      h2("Coefficients estimés par les modèles en considérant une distribution Normale"),
    fluidRow(
      box(
        width = 12,
      tableOutput("coefficients_norm"),
      downloadButton("download_norm", "Télécharger résultats (Distribution Normale)")),
      ),
    
      ## Tableau des résultats avec une distribution de Student
      h2("Coefficients estimés par les modèles en considérant une distribution de Student"),
    fluidRow(  
      box(
        width = 12,
      tableOutput("coefficients_std"),
      downloadButton("download_std", "Télécharger résultats (Distribution de Student)")),
        )
      )
    )
  )
  #)
}
