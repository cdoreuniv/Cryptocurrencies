## ------- Création des différentes bases de données ------- ##
library(tidyverse)
library(quantmod)
library(PerformanceAnalytics)
library(lubridate)
library(robustbase)


###### Je vais créer deux bases de données pour chaque cryptocurrencies
###### La première base correspond aux valeurs prises jusqu'au jour auquel se connecte l'utilisateur
###### La seconde base contient les valeurs observées jusqu'à un an avant la date de connection de l'utilisateur

### Extraction des données du Bitcoin

## Je veux sélectionner les dates jusqu'à l'an dernier à la même date qu'hier
# Obtenir la date d'hier
hier <- Sys.Date()-day(1)

# Sélectionner la date d'il y a un an
hierym1 <- hier - years(1)


# Chargement des données du bitcoin du site yahoo
getSymbols(Symbols = "BTC-EUR", src = "yahoo", 
           from = as.Date("2014-01-01"), to = hier)

# ============= Calcul des rentabilités ============= 
## Récupération des prix de cloture de l'actif
prixclot = `BTC-EUR`[,4]

## Je calcule les rentabilités de l'actif
returnsBTC <- dailyReturn(prixclot)	
returnsBTC2 <- returnsBTC^2	

`BTC-EUR`$Returns = returnsBTC
`BTC-EUR`$Returns2 = returnsBTC2

# ============= Ajustement des outliers ====================
cleanreturnsBTC = Return.clean(returnsBTC, method = "boudt")
cleanreturnsBTC2 = cleanreturnsBTC^2

`BTC-EUR`$cleanrentabilites = cleanreturnsBTC
`BTC-EUR`$cleanrentabilites2 = cleanreturnsBTC2

data <- as.data.frame(`BTC-EUR`)
#data <- rownames_to_column(data)
data$date <- as.Date(rownames(data), start = data[1,1], format = "%Y-%m-%d")

## Sauvegarde des bases de données
d = hier-hierym1 ## 365j
n = nrow(`BTC-EUR`) - as.numeric(d) ## Nombre d'observations

dataBTChier = data[,c(1,4,5,7,8,9,10,11)]
colnames(dataBTChier) = c("ouverture", "cloture", "volume", "rentabilites", "rentabilites2","cleanrentabilites", "cleanrentabilites2","date")
dataBTCym1 = data[1:n,c(1,4,5,7,8,9,10,11)]
colnames(dataBTCym1) = c("ouverture", "cloture", "volume", "rentabilites", "rentabilites2","cleanrentabilites", "cleanrentabilites2","date")
### Extraction des données Ethereun

# Chargement des données du bitcoin du site yahoo
getSymbols(Symbols = "ETH-EUR", src = "yahoo", 
           from = as.Date("2014-01-01"), to = hier)

# ============= Calcul des rentabilités ============= 
## Récupération des prix de cloture de l'actif
prixclot = `ETH-EUR`[,4]

## Je calcule les rentabilités de l'actif
returnsETH <- dailyReturn(prixclot)	
returnsETH2 <- returnsETH^2	

`ETH-EUR`$Returns = returnsETH
`ETH-EUR`$Returns2 = returnsETH2

# ============= Ajustement des outliers ====================
cleanreturnsETH = Return.clean(returnsETH, method = "boudt")
cleanreturnsETH2 = cleanreturnsETH^2

`ETH-EUR`$cleanrentabilites = cleanreturnsETH
`ETH-EUR`$cleanrentabilites2 = cleanreturnsETH2

data <- as.data.frame(`ETH-EUR`)
#data <- rownames_to_column(data)
data$date <- as.Date(rownames(data), start = data[1,1], format = "%Y-%m-%d")


## Sauvegarde des bases de données
d = hier-hierym1 ## 365j
n = nrow(`ETH-EUR`) - as.numeric(d) ## Nombre d'observations

dataETHhier = data[,c(1,4,5,7,8,9,10,11)]
colnames(dataETHhier) = c("ouverture", "cloture", "volume", "rentabilites", "rentabilites2","cleanrentabilites","cleanrentabilites2","date")
dataETHym1 = data[1:n,c(1,4,5,7,8,9,10,11)]
colnames(dataETHym1) = c("ouverture", "cloture", "volume", "rentabilites", "rentabilites2","cleanrentabilites","cleanrentabilites2","date")


### Extraction des données BNB

# Chargement des données du BNB du site yahoo
getSymbols(Symbols = "BNB-EUR", src = "yahoo", 
           from = as.Date("2014-01-01"), to = hier)

# ============= Calcul des rentabilités ============= 
## Récupération des prix de cloture de l'actif
prixclot = `BNB-EUR`[,4]

## Je calcule les rentabilités de l'actif
returnsBNB <- dailyReturn(prixclot)	
returnsBNB2 <- returnsBNB^2	

`BNB-EUR`$Returns = returnsBNB
`BNB-EUR`$Returns2 = returnsBNB2

# ============= Ajustement des outliers ====================
cleanreturnsBNB = Return.clean(returnsBNB, method = "boudt")
cleanreturnsBNB2 = cleanreturnsBNB^2

`BNB-EUR`$cleanrentabilites = cleanreturnsBNB
`BNB-EUR`$cleanrentabilites2 = cleanreturnsBNB2

data <- as.data.frame(`BNB-EUR`)
#data <- rownames_to_column(data)
data$date <- as.Date(rownames(data), start = data[1,1], format = "%Y-%m-%d")


## Sauvegarde des bases de données
d = hier-hierym1 ## 365j
n = nrow(`BNB-EUR`) - as.numeric(d) ## Nombre d'observations

dataBNBhier = data[,c(1,4,5,7,8,9,10,11)]
colnames(dataBNBhier) = c("ouverture", "cloture", "volume", "rentabilites", "rentabilites2","cleanrentabilites","cleanrentabilites2","date")
dataBNBym1 = data[1:n,c(1,4,5,7,8,9,10,11)]
colnames(dataBNBym1) = c("ouverture", "cloture", "volume", "rentabilites", "rentabilites2","cleanrentabilites","cleanrentabilites2","date")



save.image(file = "data.RData")





