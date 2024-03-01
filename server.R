load("data.RData")

function(input, output, session) {
  ## --------------- Page 1 : Présentation des cryptomonnaies --------------- 
  datapage1 <- reactive({
    nom_database <- paste0("data", input$database, "hier")
    data <- as.data.frame(get(nom_database))
    return(data)
    })
  explications <- lire_explications()
  output$explication <- renderUI({
    database <- input$database
    explication <- explications[[database]]$text
    HTML(explication)
  })
  
  output$coursactif <- renderPlotly({
    fig <- plot_ly(data = datapage1(),
                   x = ~date,
                   y = ~cloture ,
                   type = 'scatter',
                   mode = 'lines',
                   line = list(shape = "hv", color = 'rgb(165, 49, 245)', width = 2)) %>%
      layout(xaxis = list(title = "Date",tickangle = -45),
             yaxis = list(title = "Prix de clôture (en Euros)"),
             title =paste0("Cours de l'actif ", "BTC")
             )
    fig
  })
  
  
  ## --------------- Page 2 : Analyse exploratoire des rentabilités ---------------
  # Fonction pour vérifier la période
  checkPeriod <- function() {
    datedeb <- input$dates[1]
    datefin <- input$dates[2]
    
    difference <- as.numeric(difftime(datefin, datedeb, units = "days"))
    
    return(difference == 5 * 365)
  }
  
  # Vérification de la période
  output$message <- renderText({
    validate(
      need(checkPeriod(), "L'écart entre les deux dates n'est pas de 5 ans. Veuillez élargir votre période.")
    )
    
    return("L'écart entre les deux dates est de 5 ans")
  })
  
  datapage2 = reactive({
    # Je récupère les dates sélectionnées
    datedeb = input$dates[1]
    datefin = input$dates[2]
    
    nom_database2 = paste0("data", input$nom_database2, "ym1")
    data = as.data.frame(get(nom_database2))
    
    # Je sélectionne les lignes correspondantes aux dates sélectionnées
    masque = data$rowname >= datedeb & data$rowname <= datefin
    data = data[data$date %in% seq(datedeb, datefin, by = "days"),]
    return(data)
  })

  # Discrimination des outliers
    output$discout1 = renderPlotly({
      
     fig <- plot_ly(data = datapage2(),
                   x = ~date,
                   y = ~rentabilites,
                   type = 'scatter',
                   mode = 'lines',
                   line = list(shape = "hv", color = 'rgb(165, 49, 245)', width = 2),
                   name = "Rentabilités brutes") %>%
     #add_trace() %>%
     add_trace(data = datapage2(),
               x = ~date,
               y = ~cleanrentabilites,
               type = 'scatter',
               mode = 'lines',
               line = list(shape = "hv", color = 'black', width = 2),
               name = "Rentabilités corrigées") %>%
      layout(xaxis = list(title = "Date",tickangle = -45),
             yaxis = list(title = "Rentabilités"),
             title = paste0("Rentabilités de l'actif ", input$nom_database2))
    fig
  })

  output$discout2 = renderPlotly({
    fig <- plot_ly() %>%
      add_trace(x = ~date,
                y = ~rentabilites2,
                data = datapage2(),
                type = 'scatter',
                mode = 'lines',
                line = list(shape = "hv", color = 'rgb(165, 49, 245)', width = 2),
                name = "Rentabilités au carré brutes") %>%
      add_trace(x = ~date,
                y = ~cleanrentabilites2,
                data = datapage2(),
                type = 'scatter',
                mode = 'lines',
                line = list(shape = "hv", color = 'black', width = 2),
                name = "Rentabilités au carré corrigées") %>%
      layout(xaxis = list(title = "Date",tickangle = -45),
             yaxis = list(title = "Rentabilités"),
             title = paste0("Rentabilités au carré de l'actif ", "BTC"))
    fig
  })

  
  output$moyenne <- renderValueBox({
    data = basicStats(datapage2()$cleanrentabilites*100)
    #data = stats1()
    moy = round(data["Mean", 1], 3)
    valueBox(
      tagList(tags$sup("Mean (%)", style = "font-size: 18px")),
      moy,
      color = "blue"
      #width = 6
    )
  })
  
  output$mediane <- renderValueBox({
    data = basicStats(datapage2()$cleanrentabilites*100)
    #data <- stats1()
    med <- round(data["Median", 1], 3)
    
    valueBox(
      tagList(tags$sup("Median (%)", style = "font-size: 18px")),
      med,
      color = "blue"
      #width = 6
    )
  })
  
  output$minimum = renderValueBox({
    data = basicStats(datapage2()$cleanrentabilites*100)
    #data = stats1()
    min = round(data["Minimum",1],3)
    
    valueBox(
      tagList(tags$sup("Min (%)", style="font-size: 18px")),
      min, 
      #icon = icon("arrow-up"),
      color = "blue"
      #width = 12
      )
  })
  
  output$maximum = renderValueBox({
    data = basicStats(datapage2()$cleanrentabilites*100)
    #data = stats1()
    max = round(data["Maximum",1],3)
    
    valueBox(
      tagList(tags$sup("Max (%)", style="font-size: 18px")),
      max, 
      #icon = icon("arrow-up"),
      color = "blue"
      #width = 4
      )
  })
  
  output$etype= renderValueBox({
    data = basicStats(datapage2()$cleanrentabilites*100)
    #data = stats1()
    sd = round(data["Stdev",1],3)
    
    valueBox(
      tagList(tags$sup("Std Dev (%)", style="font-size: 18px")),
      sd, 
      #icon = icon("arrow-up"),
      color = "blue"
      #width = 2
      )
  })

  output$skewness = renderValueBox({
    data = basicStats(datapage2()$cleanrentabilites*100)
    #data = stats1()
    sk = round(data["Skewness",1],3)
    
    valueBox(
      tagList(tags$sup("Skewness", style="font-size: 18px")),
      sk, 
      #icon = icon("arrow-up"),
      color = "blue"
      #width = 2
      )
  })
  
  output$exkurto = renderValueBox({
    data = basicStats(datapage2()$cleanrentabilites*100)
    #data = stats1()
    ek = round(data["Kurtosis",1],3)
    
    valueBox(
      tagList(tags$sup("Ex-Kurtosis", style="font-size: 18px")),
      ek, 
      #icon = icon("arrow-up"),
      color = "blue"
      #width = 2
      )
  })
  
  output$v1 = renderValueBox({
    data = basicStats(datapage2()$cleanrentabilites*100)
    ## Calcul de v1 = (Skew - 0) / sqrt(6/T)
    T = nrow(datapage2())
    v1 = data["Skewness",1] / sqrt(6/T)
    #data["v1","Statistiques"] = v1
    
    valueBox(
      tagList(tags$sup("V1", style="font-size: 18px")),
      round(v1,3), 
      #icon = icon("arrow-up"),
      color = "blue"
      #width = 2
      )
  })
  
  output$v2 = renderValueBox({
    data = basicStats(datapage2()$cleanrentabilites*100)
    T = nrow(datapage2())
    ## Calcul de v2 = (Ex-Kurt - 3)/sqrt(24/T)
    v2 = data[ "Kurtosis",1] / sqrt(24/T)
    #data = stats1()
    
    valueBox(
      tagList(tags$sup("V2", style="font-size: 18px")),
      round(v2,3), 
      #icon = icon("arrow-up"),
      color = "blue"
      #width = 2
      )
  })
  
  output$jb = renderValueBox({
    # Jarque-Bera normality test with DescTools package
    jb1 = JarqueBeraTest(datapage2()$cleanrentabilites, robust = FALSE, method = "chisq")
    jb = jb1$p.value
    
    valueBox(
      tagList(tags$sup("J-Bera (pvalue)", style="font-size: 18px")),
      jb, 
      #icon = icon("arrow-up"),
      color = "blue"
      #width = 2
      )
  })
  
  output$bp = renderValueBox({
    # autocorrelation Box-Pierce and Ljung-Box tests with 10 lags
    bp1 = Box.test(datapage2()$cleanrentabilites, lag = 10, type = c("Box-Pierce", "Ljung-Box"), fitdf = 0)
    bp = bp1$p.value
    
    #data = stats1()
    #bp = round(data["BoxPierce",1],3)
    
    valueBox(
      tagList(tags$sup("B-Pierce (pvalue)", style="font-size: 18px")),
      round(bp,3), 
      #icon = icon("arrow-up"),
      color = "blue"
      #width = 2
      )
  })
  
  output$q5 = renderValueBox({
    # LM-ARCH test with 5 lags
    q51 = ArchTest(datapage2()$cleanrentabilites,lag=5)
    q5 = q51$p.value
    
    #data = stats1()
    #q5 = round(data["Q5",1],3)
    
    valueBox(
      tagList(tags$sup("LMARCH test (5 lags)", style="font-size: 18px")),
      round(q5,3), 
      #icon = icon("arrow-up"),
      color = "blue"
      #width = 6
      )
  })
  

  
  
  ## --------------- Page 3 : Estimation des modèles de volatilité ---------------
#   tion des données
datapage3 <- reactive({
  # Je récupère les dates sélectionnées
  datedeb = input$dates2[1]
  datefin = input$dates2[2]

  nom_database3 = paste0("data", input$nom_database3, "ym1")
  data = as.data.frame(get(nom_database3))

  # Je sélectionne les lignes correspondantes aux dates sélectionnées
  data$date <- as.Date(data$date)  # Make sure date column is in Date format

  # Filter rows based on date range
  data = data[data$date >= datedeb & data$date <= datefin, ]

  return(data)
})

  
  get_model_std <- reactive({
    y = datapage3()$cleanrentabilites
    get_model = function(model_name, distribution){
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
      "Risk-metrics" = {
        spec <- ugarchspec(variance.model=list(model = "iGARCH"), 
                           mean.model=list(armaOrder=c(0,0), 
                                           include.mean=TRUE), 
                           fixed.pars=list(omega=0,
                                           alpha1=0.06,
                                           beta1=0.94),
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
    models <- lapply(input$models, get_model, distribution = "std")
    if (length(models) == 0) {
      return(NULL)
    }
    
    all_coefs <- do.call(rbind, models)
    all_coefs$Model <- as.factor(all_coefs$Model)
    
    # Use melt to reshape from long to wide
    wide_coefs <- dcast(melt(all_coefs,
                             id.vars = c("Coefficient", "Model")), 
                        Coefficient ~ Model + variable, 
                        value.var = "value")
    
    wide_coefs
  })
  
# # On récupère les résultats des modèles avec une distribution de Student
# models_selected_std <- reactive({
#   models <- lapply(input$models, get_model, distribution = "std")
#   if (length(models) == 0) {
#     return(NULL)
#   }
#   
#   all_coefs <- do.call(rbind, models)
#   all_coefs$Model <- as.factor(all_coefs$Model)
#   
#   # Use melt to reshape from long to wide
#   wide_coefs <- dcast(melt(all_coefs,
#                            id.vars = c("Coefficient", "Model")), 
#                       Coefficient ~ Model + variable, 
#                       value.var = "value")
#   
#   wide_coefs
# })

# # On récupère les résultats des modèles avec une distribution normale
# models_selected_norm <- reactive({
#   models <- lapply(input$models, get_model, distribution = "norm")
#   if (length(models) == 0) {
#     return(NULL)
#   }
#   
#   all_coefs <- do.call(rbind, models)
#   all_coefs$Model <- as.factor(all_coefs$Model)
#   
#   # Use melt to reshape from long to wide
#   wide_coefs <- dcast(melt(all_coefs, 
#                            id.vars = c("Coefficient", "Model")), 
#                       Coefficient ~ Model + variable, 
#                       value.var = "value")
#   
#   wide_coefs
# })

  get_model_norm <- reactive({
    y = datapage3()$cleanrentabilites
    get_model = function(model_name, distribution){
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
        "Risk-metrics" = {
          spec <- ugarchspec(variance.model=list(model = "iGARCH"), 
                             mean.model=list(armaOrder=c(0,0), 
                                             include.mean=TRUE), 
                             fixed.pars=list(omega=0,
                                             alpha1=0.06,
                                             beta1=0.94),
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
    models <- lapply(input$models, get_model, distribution = "norm")
    if (length(models) == 0) {
      return(NULL)
    }
    
    all_coefs <- do.call(rbind, models)
    all_coefs$Model <- as.factor(all_coefs$Model)
    
    # Use melt to reshape from long to wide
    wide_coefs <- dcast(melt(all_coefs,
                             id.vars = c("Coefficient", "Model")), 
                        Coefficient ~ Model + variable, 
                        value.var = "value")
    
    wide_coefs
  })
  
# Render the tables
output$coefficients_std <- renderTable({
  get_model_std()
})

# Téléchargement des résultats (Distribution Normale)
output$download_norm <- downloadHandler(
  filename = function() {
    paste("resultats_modeles_norm", Sys.Date(), ".csv", sep = "")
  },
  content = function(file) {
    # Données à télécharger
    write.csv(get_model_norm(), file, row.names = FALSE)
  }
)

output$coefficients_norm <- renderTable({
  get_model_norm()
})

# Téléchargement des résultats (Distribution de Student)
output$download_std <- downloadHandler(
  filename = function() {
    paste("resultats_modeles_GARCH_student", Sys.Date(), ".csv", sep = "")
  },
  content = function(file) {
    # Données à télécharger
    write.csv(get_model_std(), file, row.names = FALSE)
  }
)


}