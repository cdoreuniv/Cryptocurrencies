source("librairies.R")
load("data.RData")

## --------------- Page 1 : Présentation des cryptomonnaies --------------- 
nom_database <- paste0("data", "BTC", "hier")
datapage1 = as.data.frame(get(nom_database))

### -------------- Insertion du texte de présentation pour chaque crypto choisie -------------- 


### -------------- Insertion du graphique interactif en fonction de la crypto choisie -------------- 
fig <- plot_ly(data = datapage1, 
               x = ~rownames(datapage1), 
               y = ~cloture ,
               type = 'scatter', 
               mode = 'lines',
               line = list(shape = "hv", color = 'rgb(165, 49, 245)', width = 2)) %>%
  layout(xaxis = list(title = "Date",tickangle = -45),
         yaxis = list(title = "Prix de clôture (en Euros)"),
         title =paste0("Cours de l'actif ", "BTC"))
fig


## --------------- Page 2 : Analyse exploratoire des rentabilités ---------------
# Je dois rédiger sur un document txt pour expliquer ce qu'est la rentabilité d'une crypto

# On considère que l'utilisateur choisit une crypto (BTC - ETH ou BNB)
# On va aussi demander à l'utilisateur de choisir une période de 5 ans pour faire l'analyse et la prévision
# BTC est ouvert depuis 2014 tandis que les deux autres depuis 2017. 
# Il faut qu'on ne puisse pas choisir avant la date de création et donc afficher la date de création de l'actif. 
# On laisse volontairement un an afin de pouvoir prédire l'année passée jusqu'à hier

nom_database2 <- paste0("data", "BTC", "ym1")
datapage2 = as.data.frame(get(nom_database2))

### Controle des dates (il faut que ce soit 5 ans)
date_min <- as.Date("2000-01-01")
date_max <- as.Date("2022-12-31")

# Générer deux indices aléatoires
indices_aleatoires <- sample(seq_along(seq(date_min, date_max, by = "days")), 2)

# Extraire les deux dates correspondantes
dates<- sort(seq(date_min, date_max, by = "days"))[indices_aleatoires]


dates5ans = function(){
  datedeb = dates[1]
  datefin = dates[2]
  
  difference <- difftime(datefin, datedeb, units = "days")
  
  if (as.numeric(difference) == 5 * 365) {
    cat("L'écart entre les deux dates est de 5 ans.\n")
  } else {
    cat("L'écart entre les deux dates n'est pas de 5 ans.\n Veuillez élargir votre période")
  }
}
dates5ans()

datapage2 = as.data.frame(get(nom_database2))
datedeb <- as.Date("2017-01-01")
datefin <- as.Date("2022-12-31")
dates = c(datedeb,datefin)

# Je sélectionne les lignes correspondantes aux dates sélectionnées
datapage2 = datapage2[datapage2$date %in% seq(datedeb, datefin, by = "days"),]

fig <- plot_ly() %>%
  add_trace(x = ~date, 
            y = ~rentabilites,
            data = datapage2,
            type = 'scatter', 
            mode = 'lines',
            line = list(shape = "hv", color = 'rgb(165, 49, 245)', width = 2),
            name = "Rentabilités brutes") %>%
  add_trace(x = ~date, 
            y = ~cleanrentabilites,
            data = datapage2,
            type = 'scatter', 
            mode = 'lines',
            line = list(shape = "hv", color = 'black', width = 2),
            name = "Rentabilités corrigées") %>%
  layout(xaxis = list(title = "Date",tickangle = -45),
         yaxis = list(title = "Rentabilités"),
         title = paste0("Rentabilités de l'actif ", "BTC"))
fig


## Statistiques descriptives de la série corrigée

# mesures de la moyenne, écart-type, skewness, kurtosis ...
stat1 = basicStats(datapage2$cleanrentabilites)
colnames(stat1) = "Statistiques"
show(stat1) 

# Attention : donne l'excès de Kurtosis -- A mettre en annexe
# descriptive stat on creturnsBTCs in %
stat = basicStats(datapage2$cleanrentabilites*100)
colnames(stat) = "Statistiques"
show(stat)

## Calcul de v1 = (Skew - 0) / sqrt(6/T)
T = nrow(dataBNBym1)
v1 = stat1$Statistiques[rownames(stat1) == "Skewness"] / sqrt(6/T)
v1
## Calcul de v2 = (Ex-Kurt - 3)/sqrt(24/T)
v2 = stat1$Statistiques[rownames(stat1) == "Kurtosis"] / sqrt(24/T)
v2

#
stat["v1","Statistiques"] = v1
stat["v2","Statistiques"] = v2 

# Récupérer la moyenne
stat["Mean",]

# Jarque-Bera normality test with DescTools package
jb = JarqueBeraTest(datapage2$cleanrentabilites, robust = FALSE, method = "chisq")
jb$p.value

# autocorrelation Box-Pierce and Ljung-Box tests with 10 lags
bp = Box.test(datapage2$cleanrentabilites, lag = 10, type = c("Box-Pierce", "Ljung-Box"), fitdf = 0)
bp$p.value

stat["JarqueBera", "Statistiques"] = jb$p.value
stat["BoxPierce", "Statistiques"] = bp$p.value

# LM-ARCH test with 5 and 10 lags
q5 = ArchTest(datapage2$cleanrentabilites,lag=5)
q5$p.value
q10 = ArchTest(datapage2$cleanrentabilites,lag=10)
q10$p.value

stat["Q5", "Statistiques"] = q5$p.value
stat["Q10", "Statistiques"] = q10$p.value

stat['Mean',]

## --------------- Page 3 : Estimation des modèles de volatilité ---------------
### --------------- GARCH distribution Normale ---------------
library(rugarch)
y = dataBTCym1$cleanrentabilites
spec_garch_norm = ugarchspec(variance.model=list(model = "sGARCH"), 
                             mean.model=list(armaOrder=c(0,0), 
                                             include.mean=TRUE)
                             )
mod_garch_norm = ugarchfit(data = y, 
                           spec = spec_garch_norm
                           )
mod_garch_norm
coef(mod_garch_norm)



spec_garch_std = ugarchspec(variance.model=list(model = "sGARCH"), 
                            mean.model=list(armaOrder=c(0,0), 
                                            include.mean=TRUE),
                            distribution.model = "std")
mod_garch_std = ugarchfit(data = y, spec = spec_garch_std)
mod_garch_std
coef= coef(mod_garch_std)
se <- sqrt(diag(vcov(mod_garch_std)))

# Calculate t-values
t_values <- coef / se

# Calculate two-tailed p-values
p_values <- 2 * pt(abs(t_values), df = Inf, lower.tail = FALSE)

# Display p-values
print(p_values)

values = cbind(coef, p_values)


get_model <- function(model_name, distribution) {
  y = dataBTCym1$cleanrentabilites
  switch(
    model_name,
    "sGARCH" = {
      spec <- ugarchspec(variance.model=list(model = "sGARCH"), 
                         mean.model=list(armaOrder=c(0,0), 
                                         include.mean=TRUE), 
                         distribution.model = distribution)
      model = ugarchfit(data = y, spec = spec)
      
      coef_df <- data.frame(coef(model))
      tval = data.frame(model@fit[["robust.tval"]])
      
      log_likelihood <- model@fit[["LLH"]]  # Log-likelihood
      num_params <- sum(unlist(model@fit$coef))  # Total number of parameters
      
      # Calculate AIC
      AIC_value <- -2 * log_likelihood + 2 * num_params
      
      coef_df = rbind(coef_df, Log_likewood = log_likelihood, AIC = AIC_value)
      tval$coefficient = rownames(tval)
      coef_df$coefficient = rownames(coef_df)
      coef_df = left_join(coef_df, tval, by = "coefficient")
      coef_df = cbind(coeffcient = coef_df[,"coefficient"], coef_df[,-2])
      
      coef_df$Model <- "sGARCH"
      
      colnames(coef_df) = c("Coefficient","Valeur", 'Tval',"Model")
      coef_df
      
    },
    "iGARCH" = {
      spec <- ugarchspec(variance.model = list(model = "iGARCH"),
                         distribution.model = distribution)
      model = ugarchfit(data = y, spec = spec)
      
      coef_df <- data.frame(coef(model))
      tval = data.frame(model@fit[["robust.tval"]])
      
      log_likelihood <- model@fit[["LLH"]]  # Log-likelihood
      num_params <- sum(unlist(model@fit$coef))  # Total number of parameters
      
      # Calculate AIC
      AIC_value <- -2 * log_likelihood + 2 * num_params
      
      coef_df = rbind(coef_df, Log_likewood = log_likelihood, AIC = AIC_value)
      tval$coefficient = rownames(tval)
      coef_df$coefficient = rownames(coef_df)
      coef_df = left_join(coef_df, tval, by = "coefficient")
      coef_df = cbind(coeffcient = coef_df[,"coefficient"], coef_df[,-2])
      
      coef_df$Model <- "iGARCH"
      
      colnames(coef_df) = c("Coefficient","Valeur", 'Tval',"Model")
      coef_df
      
    },
    "fGARCH" = {
      spec <- ugarchspec(variance.model = list(model = "fGARCH"),
                         distribution.model = distribution)
      model = ugarchfit(data = y, spec = spec)
      
      coef_df <- data.frame(coef(model))
      tval = data.frame(model@fit[["robust.tval"]])
      
      log_likelihood <- model@fit[["LLH"]]  # Log-likelihood
      num_params <- sum(unlist(model@fit$coef))  # Total number of parameters
      
      # Calculate AIC
      AIC_value <- -2 * log_likelihood + 2 * num_params
      
      coef_df = rbind(coef_df, Log_likewood = log_likelihood, AIC = AIC_value)
      tval$coefficient = rownames(tval)
      coef_df$coefficient = rownames(coef_df)
      coef_df = left_join(coef_df, tval, by = "coefficient")
      coef_df = cbind(coeffcient = coef_df[,"coefficient"], coef_df[,-2])
      
      coef_df$Model <- "Risk-metrics"
      
      colnames(coef_df) = c("Coefficient","Valeur", 'Tval',"Model")
      coef_df
      
    },
    "gjrGARCH" = {
      spec <- ugarchspec(variance.model = list(model = "gjrGARCH"),
                         distribution.model = distribution)
      model = ugarchfit(data = y, spec = spec)
      
      coef_df <- data.frame(coef(model))
      tval = data.frame(model@fit[["robust.tval"]])
      
      log_likelihood <- model@fit[["LLH"]]  # Log-likelihood
      num_params <- sum(unlist(model@fit$coef))  # Total number of parameters
      
      # Calculate AIC
      AIC_value <- -2 * log_likelihood + 2 * num_params
      
      coef_df = rbind(coef_df, Log_likewood = log_likelihood, AIC = AIC_value)
      tval$coefficient = rownames(tval)
      coef_df$coefficient = rownames(coef_df)
      coef_df = left_join(coef_df, tval, by = "coefficient")
      coef_df = cbind(coeffcient = coef_df[,"coefficient"], coef_df[,-2])
      
      coef_df$Model <- "gjrGARCH"
      
      colnames(coef_df) = c("Coefficient","Valeur", 'Tval',"Model")
      coef_df
      
    }
  )
}

models <- lapply(c("iGARCH", "sGARCH","gjrGARCH"), get_model, distribution = "std")

if (length(models) == 0) {
  return(NULL)
}


all_coefs <- do.call(rbind, models)
all_coefs$Model <- as.factor(all_coefs$Model)

wide_coefs <- dcast(all_coefs, Coefficient  ~ Model, value.var = c("Valeur","Tval"))
wide_coefs

spec <- ugarchspec(variance.model=list(model = "sGARCH"), 
                   mean.model=list(armaOrder=c(0,0), 
                                   include.mean=TRUE), 
                   distribution.model = "std")
model = ugarchfit(data = y, spec = spec)

coef_df <- data.frame(coef(model))
tval = data.frame(mod_garch_norm@fit[["robust.tval"]])
tval$coefficient = rownames(tval)
coef_df$coefficient = rownames(coef_df)
coef_df = merge(coef_df, tval, by = "coefficient")
coef_df$Model <- "sGARCH"

colnames(coef_df) = c("Coefficient","Valeur", 'Tval',"Model")
coef_df


get_model <- function(model_name, distribution) {
  y = dataBTCym1$cleanrentabilites
  
  switch(
    model_name,
    "sGARCH" = {
      spec <- ugarchspec(variance.model = list(model = "sGARCH"),
                         mean.model = list(armaOrder=c(0,0), include.mean=TRUE),
                         distribution.model = distribution)
      model <- ugarchfit(data = y, spec = spec)
    },
    "iGARCH" = {
      spec <- ugarchspec(variance.model = list(model = "iGARCH"),
                         distribution.model = distribution)
      model <- ugarchfit(data = y, spec = spec)
    },
    "fGARCH" = {
      spec <- ugarchspec(variance.model = list(model = "fGARCH"),
                         distribution.model = distribution)
      model <- ugarchfit(data = y, spec = spec)
    },
    "gjrGARCH" = {
      spec <- ugarchspec(variance.model = list(model = "gjrGARCH"),
                         distribution.model = distribution)
      model <- ugarchfit(data = y, spec = spec)
    }
  )
  
  coef_df <- data.frame(coef(model))
  tval <- data.frame(model@fit[["robust.tval"]])
  tval$coefficient <- rownames(tval)
  coef_df$coefficient <- rownames(coef_df)
  coef_df <- merge(coef_df, tval, by = "coefficient")
  coef_df$Model <- model_name
  
  colnames(coef_df) <- c("Coefficient", "Valeur", "Tval", "Model")
  
  coef_df
}
models <- lapply(c("iGARCH", "sGARCH","gjrGARCH"), get_model, distribution = "std")
if (length(models) == 0) {
  return(NULL)
}

all_coefs <- do.call(rbind, models)
all_coefs$Model <- as.factor(all_coefs$Model)

# Use melt to reshape from long to wide
wide_coefs <- dcast(melt(all_coefs, id.vars = c("Coefficient", "Model")), Coefficient ~ Model + variable, value.var = "value")

wide_coefs





