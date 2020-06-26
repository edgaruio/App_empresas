# Aplicacion empresas

shinyServer(function(input, output, session) {
  
  ######## PRIMERA VENTANA ===========================================================================
  
  data_f1 <- eventReactive(input$go2,{
    data_f1 <- bd_empresas12 %>% 
      filter(if (input$xpiramide1 != "Total") piramide1 == input$xpiramide1 else TRUE,
             if (input$xpiramide2 != "Total") piramide2 == input$xpiramide2 else TRUE,
             if (input$xactividad != "Total") actividadciiu == input$xactividad else TRUE)
      # filter((if (input$xpiramide1 != "Total") piramide_1 == input$xpiramide1 else TRUE) | (if (input$xpiramide2 != "Total") piramide_2 == input$xpiramide2 else TRUE))
      # filter(if (input$xpiramide1 != "Total") piramide_1 == input$xpiramide1 else TRUE,
      #        if (input$xpiramide2 != "Total") piramide_2 == input$xpiramide2 else TRUE)
      # filter(xor(if (input$xpiramide1 != "Total") piramide_1 == input$xpiramide1 else TRUE, 
                 # if (input$xpiramide2 != "Total") piramide_2 == input$xpiramide2 else TRUE))
    return(data_f1)
  })
  
  data_f1_c <- reactive({
    data_f1_c <- bd_empresas %>% 
      filter(if (input$xpiramide1 != "Total") piramide1 == input$xpiramide1 else TRUE,
             if (input$xpiramide2 != "Total") piramide2 == input$xpiramide2 else TRUE,
             if (input$xactividad != "Total") actividadciiu == input$xactividad else TRUE) %>% 
      filter(agrupado == input$xagrupador_glob)
    return(data_f1_c)
  })
  
  con_piramide2 <- eventReactive(input$go2,{
    con_piramide2 <- consulta_piramide %>% 
      filter(if (input$xpiramide1 != "Total") Piramide1 == input$xpiramide1 else TRUE,
             if (input$xpiramide2 != "Total") Piramide2 == input$xpiramide2 else TRUE,
             if (input$xactividad != "Total") ActividadCIIU == input$xactividad else TRUE)
    return(con_piramide2)
  })
  
  con_empresa2 <- eventReactive(input$go2,{
    con_empresa2 <- consumo_emp %>% 
      filter(if (input$xpiramide1 != "Total") piramide1 == input$xpiramide1 else TRUE,
             if (input$xpiramide2 != "Total") piramide2 == input$xpiramide2 else TRUE,
             if (input$xactividad != "Total") actividadciiu == input$xactividad else TRUE) %>% 
      data.frame()
    return(con_empresa2)
  })
  
  # info_afil_tot
  output$info_afil_emp <- renderValueBox({
    data_f1<-data_f1()
    valueBox(
      value = formatC(input$xpiramide1,format="s"),
      subtitle = "Empresas Piramide 1",
      icon = icon("users",lib="font-awesome"),
      color = "blue"
    )
  })
  
  output$info_afil_emp2 <- renderValueBox({
    data_f1<-data_f1()
    valueBox(
      value = formatC(input$xpiramide2,format="s"),
      subtitle = "Empresas Piramide 2",
      icon = icon("users",lib="font-awesome"),
      color = "blue"
    )
  })
  
  output$info_empresas_prin <- renderValueBox({
    data_f1<-data_f1() %>% 
      filter(tipo_nit == "principal")
    valueBox(
      value = formatC(length(unique(data_f1$id_empresa)),digits = 0, format = "d", big.mark=","),
      subtitle = "Empresas Principales",
      icon = icon("home"),
      color = "blue"
    )
  })

  output$info_empresas_fil <- renderValueBox({
    data_f1<-data_f1() %>%
      filter(tipo_nit == "secundaria")
    valueBox(
      value = formatC(length(unique(data_f1$id_empresa)),digits = 0, format = "d", big.mark=","),
      subtitle = "Empresas Filiales",
      icon = icon("home"),
      color = "blue"
    )
  })

  output$info_afil_glob <- renderValueBox({
    data_f1<-con_piramide2() 
    valueBox(
      value = formatC(dim(data_f1)[1],digits = 0, format = "d", big.mark=","),
      subtitle = "Afiliados",
      icon = icon("child"),
      color = "blue"
    )
  })

  output$plot_pira_glob <- renderPlotly({
    aux1 <- con_piramide2() %>%
      filter(!is.na(Edad)) %>%
      mutate(edad_agru =  cut(Edad, breaks = c(0,10,20,30,40,50,60,80,90,100,110,120,130))) %>%
      group_by(edad_agru, Genero) %>%
      summarise(clientes=n_distinct(id_persona)) %>%
      mutate(con_clientes = round(ifelse(test = Genero == "M",yes = -clientes, no = clientes))) %>%
      filter(!is.na(edad_agru))

    m <- list(l = 50,r = 50,b = 50,t = 100, pad = 0)
    f1 <- list(family = "Arial, sans-serif",size = 18,color = "lightgrey")
    f2 <- list(family = "Old Standard TT, serif",size = 14,color = "lightgrey")
    
    aux1 %>%
      plot_ly(x= ~con_clientes, y=~edad_agru,color=~Genero) %>%
      add_bars(orientation = 'h', hoverinfo = 'text', text = ~comma(round(clientes)), textposition = "outside", textfont = list(color = 'darkgrey', size = 10)) %>%
      layout(margin = m,
             bargap = 0.1,
             barmode = 'overlay',
             title = 'Piramide Poblacional', 
             font = list(color = 'lightgrey'),
             xaxis = list(title='Afiliados', titlefont = f1, tickfont = f2,  zeroline = FALSE, showline = FALSE, showgrid = FALSE, showticklabels = FALSE,
                          range = c(-round(max(abs(aux1$con_clientes))*1.5, digits = 0), round(max(abs(aux1$con_clientes))*1.5, digits = 0))),
             yaxis = list(title='Edad Agru', titlefont = f1, tickfont = f2),
             paper_bgcolor='transparent',
             plot_bgcolor='transparent') %>%
      config(displayModeBar = F)

  })

  output$plot1_glob <- renderPlotly({
    data_plot <- data_f1() 
    data_plot <- data_plot %>%
      dplyr::select(id_empresa,seg_alto:seg_medio) %>%
      group_by(id_empresa) %>%
      summarise(Basico = sum(seg_basico),
                Medio = sum(seg_medio),
                Joven = sum(seg_joven),
                Alto = sum(seg_alto)) %>%
      gather(key = "Segmento", value = "Conteo", 2:5)

    m <- list(l = 0,r = 0,b = 100,t = 100, pad = 0)
    colors <- c('rgb(128,133,133)', 'rgb(144,103,167)', 'rgb(171,104,87)', 'rgb(214,147,203)')
    f1 <- list(family = "Arial, sans-serif", size = 18, color = "lightgrey")
    f2 <- list(family = "Old Standard TT, serif", size = 14, color = "lightgrey")
    
    p1 <- plot_ly(data_plot, labels = ~Segmento, values = ~Conteo, type = 'pie', hole = 0, alpha = 0.9,
                  textinfo = 'label+percent',
                  insidetextfont = list(color = '#FFFFFF'),
                  # hoverinfo = 'text', 
                  text = ~paste('Empleados:'),
                  showlegend = T,
                  marker = list(colors = colors,
                                line = list(color = '#FFFFFF', width = 2))) %>%
      layout(margin = m,
             title = 'Participación por Segmento',
             font = list(color = 'lightgrey'),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, titlefont = f1, tickfont = f2),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, titlefont = f1, tickfont = f2),
             legend = list(x = 0.8, y = 0.5),
             paper_bgcolor='transparent',
             plot_bgcolor='transparent') %>%
      config(displayModeBar = F)
    p1
  })

  output$plot2_glob <- renderPlotly({
    data_plot <- data_f1() %>%
      filter(tipo_nit == "principal")
    data_plot <- data_plot %>%
      dplyr::select(id_empresa,cat_a:cat_c) %>%
      group_by(id_empresa) %>%
      summarise(A = sum(cat_a),
                B = sum(cat_b),
                C = sum(cat_c)) %>%
      gather(key = "Categoria", value = "Conteo", 2:4)

    m <- list(l = 0,r = 0,b = 100,t = 100, pad = 0)
    colors <- c('rgb(333,133,133)', 'rgb(111,103,167)', 'rgb(222,104,87)', 'rgb(114,147,203)')
    f1 <- list(family = "Arial, sans-serif",size = 18,color = "lightgrey")
    f2 <- list(family = "Old Standard TT, serif",size = 14,color = "lightgrey")
    
    p1 <- plot_ly(data_plot, labels =~ Categoria, values = ~Conteo, type = 'pie',hole = 0,
                  textinfo = 'label+percent',
                  insidetextfont = list(color = '#FFFFFF'),
                  # hoverinfo = 'text',
                  text = ~paste('Empleados:'),
                  showlegend = T,
                  marker = list(colors = colors,
                                line = list(color = '#FFFFFF', width = 2))) %>%
      layout(margin = m,
             title = 'Participación por Categoria',
             font = list(color = 'lightgrey'),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, titlefont = f1, tickfont = f2),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, titlefont = f1, tickfont = f2),
             legend = list(x = 0.8, y = 0.5),
             paper_bgcolor='transparent',
             plot_bgcolor='transparent') %>%
      config(displayModeBar = F)
    p1
  })
  
  output$conteo_famisanar_glob <- renderValueBox({
    data_f1<-data_f1()
    valueBox(
      value = paste(formatC(100*sum(data_f1$num_famisanar, na.rm = T)/sum(data_f1$afiliados, na.rm = T),
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(sum(data_f1$num_famisanar, na.rm = T)),")",sep = ""),
      subtitle = "Famisanar (Total)",
      icon = icon("briefcase"),
      color = "purple"
    )
  })

  output$conteo_ips_glob <- renderValueBox({
    data_f1<-data_f1()
    valueBox(
      value = paste(formatC(100*sum(data_f1$num_ips_colsubsidio)/sum(data_f1$num_famisanar),
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(sum(data_f1$num_ips_colsubsidio)),")",sep = ""),
      subtitle = "IPS Colsubsidio (Total)",
      icon = icon("briefcase"),
      color = "purple"
    )
  })

  output$conteo_pac_famisanar_glob <- renderValueBox({
    data_f1<-data_f1()
    valueBox(
      value = paste(formatC(100*sum(data_f1$num_pac_famisanar)/sum(data_f1$num_famisanar),
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(sum(data_f1$num_pac_famisanar)),")",sep = ""),
      subtitle = "PAC Famisanar (Total)",
      icon = icon("briefcase"),
      color = "purple"
    )
  })

  output$conteo_sura_eps_glob <- renderValueBox({
    data_f1<-data_f1()
    valueBox(
      value = paste(formatC(100*sum(data_f1$num_suramericana, na.rm = T)/sum(data_f1$afiliados, na.rm = T),
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(sum(data_f1$num_suramericana, na.rm = T)),")",sep = ""),
      subtitle = "Suramericana (Total)",
      icon = icon("briefcase"),
      color = "purple"
    )
  })

  output$conteo_pac_sura_glob <- renderValueBox({
    data_f1<-data_f1()
    valueBox(
      value = paste(formatC(100*sum(data_f1$num_pac_suramericana)/sum(data_f1$num_suramericana),
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(sum(data_f1$num_pac_suramericana)),")",sep = ""),
      subtitle = "Pac Suramericana (Total)",
      icon = icon("briefcase"),
      color = "purple"
    )
  })

  output$pro_salario_glob <- renderValueBox({
    data_f1<-data_f1()
    valueBox(
      value = paste("$ ",formatC(mean(data_f1$pro_salario, na.rm = T),digits = 0, format = "d", big.mark=","), sep = " "),
      subtitle = "Promedio Salario",
      icon = icon("credit-card"),
      color = "purple"
    )
  })

  output$pro_edad_glob <- renderValueBox({
    data_f1<-data_f1()
    valueBox(
      value = formatC(mean(data_f1$pro_edad, na.rm = T),digits = 0, format = "d", big.mark=","),
      subtitle = "Promedio Edad",
      icon = icon("user"),
      color = "purple"
    )
  })
  
  output$conteo_cupo_credito_glob <- renderValueBox({
    data_f1<-data_f1()
    valueBox(
      value = paste(formatC(100*sum(data_f1$cupo_credito, na.rm = T)/sum(data_f1$afiliados, na.rm = T),
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(sum(data_f1$cupo_credito, na.rm = T)),")",sep = ""),
      subtitle = "Crédito Cupo",
      icon = icon("credit-card"),
      color = "purple"
    )
  })
  
  # Consumos
  output$conteo_consumo_credito_glob <- renderValueBox({
    data_f1<-data_f1_c()
    valueBox(
      value = paste(formatC(100*sum(data_f1$consumo_credito, na.rm = T)/sum(data_f1$afiliados, na.rm = T),
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(sum(data_f1$consumo_credito, na.rm = T)),")",sep = ""),
      subtitle = "Crédito Consumo (Total)",
      icon = icon("credit-card"),
      color = "teal"
    )
  })

  output$conteo_uso_mes_glob <- renderValueBox({
    data_f1<-data_f1_c()
    valueBox(
      value = paste(formatC(100*sum(data_f1$uso_mes, na.rm = T)/sum(data_f1$afiliados, na.rm = T),
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(sum(data_f1$uso_mes, na.rm = T)),")",sep = ""),
      subtitle = "Uso TMS Convenios (Total)",
      icon = icon("ok", lib = "glyphicon"),
      color = "teal"
    )
  })
  
  output$conteo_salud_glob <- renderValueBox({
    data_f1<-data_f1_c()
    valueBox(
      value = paste(formatC(100*sum(data_f1$salud, na.rm = T)/sum(data_f1$afiliados, na.rm = T),
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(sum(data_f1$salud, na.rm = T)),")",sep = ""),
      subtitle = "Salud (Total)",
      icon = icon("briefcase"),
      color = "teal"
    )
  })
  
  output$conteo_supermercado_glob <- renderValueBox({
    data_f1<-data_f1_c()
    valueBox(
      value = paste(formatC(100*sum(data_f1$supermercados, na.rm = T)/sum(data_f1$afiliados, na.rm = T),
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(sum(data_f1$supermercados, na.rm = T)),")",sep = ""),
      subtitle = "Supermercado (Total)",
      icon = icon("cart-plus"),
      color = "teal"
    )
  })

  output$conteo_drogueria_glob <- renderValueBox({
    data_f1<-data_f1_c()
    valueBox(
      value = paste(formatC(100*sum(data_f1$medicamentos, na.rm = T)/sum(data_f1$afiliados, na.rm = T),
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(sum(data_f1$medicamentos, na.rm = T)),")",sep = ""),
      subtitle = "Droguerias",
      icon = icon("plus"),
      color = "teal"
    )
  })
  
  output$conteo_vivienda_glob <- renderValueBox({
    data_f1<-data_f1_c()
    valueBox(
      value = formatC(sum(data_f1$compra_vivienda, na.rm = T), digits = 0, format = "f", big.mark=","),
      subtitle = "Proyecto de Vivienda (Fecha de entrega)",
      icon = icon("home"),
      color = "teal"
    )
  })
  
  output$conteo_educacion_glob <- renderValueBox({
    data_f1<-data_f1_c()
    valueBox(
      value = formatC(sum(data_f1$educacion, na.rm = T),digits = 0, format = "f", big.mark=","),
      subtitle = "Educacion (Niños en colegios - Total)",
      icon = icon("graduation-cap"),
      color = "teal"
    )
  })
  
  # output$conteo_subsidio_glob <- renderValueBox({
  #   data_f1<-data_f1()
  #   valueBox(
  #     value = paste(formatC(100*sum(data_f1$Auxilios_pago)/sum(data_f1$Afiliados),
  #                           digits = 1, format = "f", big.mark=","),"%", " (",sum(data_f1$Auxilios_pago),")",sep = ""),
  #     subtitle = "Otros auxilios entregados (Total)",
  #     icon = icon("telegram"),
  #     color = "purple"
  #   )
  # })

  # RyT
  output$conteo_ryt_club_glob <- renderValueBox({
    data_f1<-data_f1_c()
    valueBox(
      value = paste(formatC(100*sum(data_f1$club, na.rm = T)/sum(data_f1$afiliados, na.rm = T),
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(sum(data_f1$club, na.rm = T)),")",sep = ""),
      subtitle = "Recreacion y Turismo - Club",
      icon = icon("rocket"),
      color = "teal"
    )
  })

  output$conteo_ryt_hoteles_glob <- renderValueBox({
    data_f1<-data_f1_c()
    valueBox(
      value = paste(formatC(100*sum(data_f1$hotel, na.rm = T)/sum(data_f1$afiliados, na.rm = T),
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(sum(data_f1$hotel, na.rm = T)),")",sep = ""),
      subtitle = "Recreacion y Turismo - Hoteles",
      icon = icon("rocket"),
      color = "teal"
    )
  })

  output$conteo_ryt_piscilago_glob <- renderValueBox({
    data_f1<-data_f1_c()
    valueBox(
      value = paste(formatC(100*sum(data_f1$piscilago, na.rm = T)/sum(data_f1$afiliados, na.rm = T),
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(sum(data_f1$piscilago, na.rm = T)),")",sep = ""),
      subtitle = "Recreacion y Turismo - Piscilago",
      icon = icon("rocket"),
      color = "teal"
    )
  })

  output$conteo_ryt_glob <- renderValueBox({
    data_f1<-data_f1_c()
    valueBox(
      value = paste(formatC(100*sum(data_f1$ryt, na.rm = T)/sum(data_f1$afiliados, na.rm = T),
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(sum(data_f1$ryt, na.rm = T)),")",sep = ""),
      subtitle = "Recreacion y Turismo - Global",
      icon = icon("rocket"),
      color = "teal"
    )
  })

  output$conteo_subsidio_vivienda_glob <- renderValueBox({
    data_f1<-data_f1()
    valueBox(
      value = paste(formatC(100*sum(data_f1$subsidio_asignado, na.rm = T)/sum(data_f1$afiliados, na.rm = T),
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(sum(data_f1$subsidio_asignado, na.rm = T)),")",sep = ""),
      subtitle = "Subsidio Vivienda (Total)",
      icon = icon("hotel"),
      color = "purple"
    )
  })

  output$conteo_hoteles_glob <- renderValueBox({
      data_f1<-data_f1()
      valueBox(
        value = paste(formatC(100*sum(data_f1$pros_hotel, na.rm = T)/sum(data_f1$afiliados, na.rm = T),
                              digits = 1, format = "f", big.mark=","),"%", " (",comma(sum(data_f1$pros_hotel, na.rm = T)),")",sep = ""),
        subtitle = "Hoteles (Prospectos)",
        icon = icon("bicycle",lib="font-awesome"),
        color = "purple"
      )
    })

  output$conteo_piscilago_glob <- renderValueBox({
    data_f1<-data_f1()
    valueBox(
      value = paste(formatC(100*sum(data_f1$pros_pisi, na.rm = T)/sum(data_f1$afiliados, na.rm = T),
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(sum(data_f1$pros_pisi, na.rm = T)),")",sep = ""),
      subtitle = "Piscilago (Prospectos)",
      icon = icon("bicycle",lib="font-awesome"),
      color = "purple"
    )
  })

  output$conteo_club_glob <- renderValueBox({
    data_f1<-data_f1()
    valueBox(
      value = paste(formatC(100*sum(data_f1$pros_club, na.rm = T)/sum(data_f1$afiliados, na.rm = T),
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(sum(data_f1$pros_club, na.rm = T)),")",sep = ""),
      subtitle = "Clubes (Prospectos)",
      icon = icon("bicycle",lib="font-awesome"),
      color = "purple"
    )
  })
  
  output$conteo_kit_glob <- renderValueBox({
    data_f1<-data_f1()
    valueBox(
      value = paste(formatC(100*sum(data_f1$kit_redimido, na.rm = T)/sum(data_f1$kit_derecho, na.rm = T),
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(sum(data_f1$kit_redimido, na.rm = T)),"/",comma(sum(data_f1$kit_derecho, na.rm = T)),")",sep = ""),
      subtitle = "Kit Escolar",
      icon = icon("book"),
      color = "green"
    )
  })
  
  output$conteo_monetarias_glob <- renderValueBox({
    data_f1<-data_f1()
    valueBox(
      value = paste(formatC(100*sum(data_f1$cuota_redimida, na.rm = T)/sum(data_f1$cuota_derecho, na.rm = T),
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(sum(data_f1$cuota_redimida, na.rm = T)),"/",comma(sum(data_f1$cuota_derecho, na.rm = T)),")",sep = ""),
      subtitle = "Cuotas Monetarias",
      icon = icon("play"),
      color = "green"
    )
  })
  
  output$conteo_data_glob <- renderValueBox({
    data_f1<-data_f1()
    valueBox(
      value = paste(formatC(100*sum(data_f1$habeas_data, na.rm = T)/sum(data_f1$afiliados, na.rm = T),
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(sum(data_f1$habeas_data, na.rm = T)),")",sep = ""),
      subtitle = "Habeas Data",
      icon = icon("database"),
      color = "purple"
    )
  })
  
  output$conteo_lonchera_glob <- renderValueBox({
    data_f1<-data_f1()
    valueBox(
      value = paste(formatC(100*sum(data_f1$bono_redimido, na.rm = T)/sum(data_f1$bono_derecho, na.rm = T),
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(sum(data_f1$bono_redimido, na.rm = T)),"/",comma(sum(data_f1$bono_derecho, na.rm = T)),")",sep = ""),
      subtitle = "Bono Lonchera",
      icon = icon("star"),
      color = "green"
    )
  })
  
  output$pre_aprobado_hipotecario_glob <- renderValueBox({
    data_f1<-data_f1()
    valueBox(
      value = paste(formatC(100*sum(data_f1$pre_aprobado_hipo, na.rm = T)/sum(data_f1$afiliados, na.rm = T),
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(sum(data_f1$pre_aprobado_hipo, na.rm = T)),")",sep = ""),
      subtitle = "Pre Aprobado Hipotecario",
      icon = icon("eraser"),
      color = "olive"
    )
  })

  output$pre_aprobado_cupo_glob <- renderValueBox({
    data_f1<-data_f1()
    valueBox(
      value = paste(formatC(100*sum(data_f1$pre_aprobado_cupo, na.rm = T)/sum(data_f1$afiliados, na.rm = T),
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(sum(data_f1$pre_aprobado_cupo, na.rm = T)),")",sep = ""),
      subtitle = "Pre Aprobado Cupo",
      icon = icon("eraser"),
      color = "olive"
    )
  })
  
  output$conteo_cuad_a_glob <- renderValueBox({
    data_f1<-data_f1()
    valueBox(
      value = paste(formatC(100*sum(data_f1$a3, na.rm = T)/sum(data_f1$afiliados, na.rm = T),
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(sum(data_f1$a3, na.rm = T)),")",sep = ""),
      subtitle = "Cuadrante A: En caso de tener Crédito hipotecario sin desembolsar,  sin tener otorgado necesariamente subsidio de vivienda",
      icon = icon("home"),
      color = "teal"
    )
  })

  output$conteo_cuad_a1_glob <- renderValueBox({
    data_f1<-data_f1()
    valueBox(
      value = paste(formatC(100*sum(data_f1$a1, na.rm = T)/sum(data_f1$afiliados, na.rm = T),
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(sum(data_f1$a1, na.rm = T)),")",sep = ""),
      subtitle = "Cuadrante A1: Con subsidio asignado con vigencia  menor a 6 meses",
      icon = icon("home"),
      color = "teal"
    )
  })

  output$conteo_cuad_a2_glob <- renderValueBox({
    data_f1<-data_f1()
    valueBox(
      value = paste(formatC(100*sum(data_f1$a2, na.rm = T)/sum(data_f1$afiliados, na.rm = T),
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(sum(data_f1$a2, na.rm = T)),")",sep = ""),
      subtitle = "Cuadrante A2: Con subsidio asignado con vigencia  mayor de  6 meses",
      icon = icon("home"),
      color = "teal"
    )
  })

  output$conteo_cuad_b_glob <- renderValueBox({
    data_f1<-data_f1()
    valueBox(
      value = paste(formatC(100*sum(data_f1$b1, na.rm = T)/sum(data_f1$afiliados, na.rm = T),
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(sum(data_f1$b1, na.rm = T)),")",sep = ""),
      subtitle = "Cuadrante B: Afiliados con interés de compra (Basado en encuestas y diagnóstico de necesidades)",
      icon = icon("home"),
      color = "teal"
    )
  })
  
  
  # output$conteo_ues1 <- renderValueBox({
  #   
  #   # if (nrow(con_empresa2()) == 0) {
  #   #   data_f1 <- data.frame(consumo = 0)
  #   # } else {
  #   #   data_f1 <- con_empresa2() %>% 
  #   #     dplyr::filter(ues == "Credito social") %>% 
  #   #     summarise(consumo = round(sum(consumo, na.rm = T),1))
  #   # }
  #   
  #   data_f1 <- con_empresa2() %>% 
  #     filter(ues %in% c("Mercadeo social")) %>% 
  #     group_by(ues) %>% 
  #     summarise(consumo = sum(consumo, na.rm = T)) %>% 
  #     data.frame()
  #   
  #   valueBox(
  #     value = paste("$ ",formatC(sum(data_f1$consumo, na.rm = T), digits = 0, format = "d", big.mark=","), sep = " "),
  #     subtitle = "Mercadeo social",
  #     icon = icon("dollar"),
  #     color = "teal"
  #   )
  # })
  
  output$consumo_valor_glob <- renderPlotly({
    data_plot <- con_empresa2() %>%
      group_by(ues) %>%
      summarise(consumo = round(sum(consumo, na.rm = T)/1000000,1)) %>%
      data.frame()

    m <- list(l = 50,r = 50,b = 100,t = 100, pad = 0)
    colors <- c('rgb(333,133,133)', 'rgb(111,103,167)', 'rgb(222,104,87)', 'rgb(114,147,203)')
    f1 <- list(family = "Arial, sans-serif",size = 18,color = "lightgrey")
    f2 <- list(family = "Old Standard TT, serif",size = 14,color = "lightgrey")

    p1 <- plot_ly(data_plot, x = ~ues, y = ~consumo, type = 'bar', text = ~comma(consumo), textposition = 'auto') %>%
      layout(margin = m,
             title = 'Consumo por UES (Millones)',
             font = list(color = 'lightgrey'),
             xaxis = list(showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE, titlefont = f1, tickfont = f2, tickcolor = 'rgb(127,127,127)'),
             yaxis = list(showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE, titlefont = f1, tickfont = f2, tickcolor = 'rgb(127,127,127)'),
             legend = list(x = 0.8, y = 0.5),
             paper_bgcolor='transparent',
             plot_bgcolor='transparent') %>%
      config(displayModeBar = F)
    p1
  })
  
  output$consumo_transa_glob <- renderPlotly({
    data_plot <- con_empresa2() %>%
      group_by(piramide1) %>%
      summarise(consumo = round(sum(consumo, na.rm = T)/1000000,1)) %>%
      data.frame()

    m <- list(l = 50,r = 50,b = 100,t = 100, pad = 0)
    colors <- c('rgb(333,133,133)', 'rgb(111,103,167)', 'rgb(222,104,87)', 'rgb(114,147,203)')
    f1 <- list(family = "Arial, sans-serif",size = 18,color = "lightgrey")
    f2 <- list(family = "Old Standard TT, serif",size = 14,color = "lightgrey")

    p1 <- plot_ly(data_plot, x = ~piramide1, y = ~consumo, type = 'bar',text = ~comma(consumo), textposition = 'auto') %>%
      layout(margin = m,
             title = 'Consumo por Piramide (Millones)',
             font = list(color = 'lightgrey'),
             xaxis = list(showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE, titlefont = f1, tickfont = f2, tickcolor = 'rgb(127,127,127)'),
             yaxis = list(showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE, titlefont = f1, tickfont = f2, tickcolor = 'rgb(127,127,127)'),
             legend = list(x = 0.8, y = 0.5),
             paper_bgcolor='transparent',
             plot_bgcolor='transparent') %>%
      config(displayModeBar = F)
    p1
  })
  
  output$downloadData <- downloadHandler(
    filename = function(){
      paste("Empresas_glob", Sys.time(), ".csv", sep = "_")
    },
    content = function(file){
      fwrite(data_f1(), file, row.names = F, sep = ";", dec = ",")
    }
  )

  ######## SEGUNDA VENTANA ==================================================================================================================
  # ====== primera fila ----------------------

  data_f <- eventReactive(input$go,{
    id = as.character(paste0(input$tipodoc,input$nit_empresa))
    data_f <- bd_empresas12 %>%
      filter(id_empresa == id)
      # filter(nombre == input$xname_empresa | id_empresa == id)
    return(data_f)
  })
  
  data_f_c <- reactive({
    id = as.character(paste0(input$tipodoc,input$nit_empresa))
    data_f <- bd_empresas %>%
      filter(id_empresa == id) %>%
      filter(agrupado == input$xagrupador_ind)
    return(data_f)
  })
  
  con_piramide <- eventReactive(input$go,{
    id = as.character(paste0(input$tipodoc,input$nit_empresa))
    con_piramide <- consulta_piramide %>%
      filter(id_empresa == id)
    return(con_piramide)
  })

  output$info_empresa <- renderValueBox({
    data_f<-data_f()
    valueBox(
      value = formatC(data_f$razonsocial[1],format="s"),
      subtitle = "Empresa",
      icon = icon("users"),
      color = "blue"
    )
  })

  output$info_cluster <- renderValueBox({
    data_f<-data_f()
    valueBox(
      value = formatC(data_f$cluster[1],format="s"),
      subtitle = "Cluster",
      icon = icon("users"),
      color = "blue"
    )
  })

  output$info_pir1 <- renderValueBox({
    data_f<-data_f()
    valueBox(
      value = formatC(data_f$piramide1[1],format="s"),
      subtitle = "Piramide 1",
      icon = icon("home"),
      color = "blue"
    )
  })

  output$info_pir2 <- renderValueBox({
    data_f<-data_f()
    valueBox(
      value = formatC(data_f$piramide2[1],format="s"),
      subtitle = "Piramide 2",
      icon = icon("home"),
      color = "blue"
    )
  })

  output$conteo_empleados <- renderValueBox({
    data_f<-data_f() %>%
      select(id_empresa,afiliados) %>%
      group_by(id_empresa) %>%
      summarise(afiliados = sum(afiliados))
    valueBox(
      value = formatC(data_f$afiliados,digits = 0, format = "d", big.mark=","),
      subtitle = "Total Empleados",
      icon = icon("child"),
      color = "blue"
    )
  })

  output$plot_pira_ind <- renderPlotly({
    aux1 <- con_piramide() %>%
      filter(!is.na(Edad)) %>%
      mutate(edad_agru =  cut(Edad, breaks = c(0,10,20,30,40,50,60,80,90,100,110,120,130))) %>%
      group_by(edad_agru, Genero) %>%
      summarise(clientes=n_distinct(id_persona)) %>%
      mutate(con_clientes = round(ifelse(test = Genero == "M",yes = -clientes, no = clientes))) %>%
      filter(!is.na(edad_agru))

    m <- list(l = 50,r = 50,b = 50,t = 100, pad = 0)
    f1 <- list(family = "Arial, sans-serif",size = 18,color = "lightgrey")
    f2 <- list(family = "Old Standard TT, serif",size = 14,color = "lightgrey")
    
    aux1 %>%
      plot_ly(x= ~con_clientes, y=~edad_agru,color=~Genero) %>%
      add_bars(orientation = 'h', hoverinfo = 'text', text = ~comma(round(clientes)), textposition = "outside", textfont = list(color = 'darkgrey', size = 10)) %>%
      layout(margin = m,
             bargap = 0.1,
             barmode = 'overlay',
             title = 'Piramide Poblacional',
             xaxis = list(title='Afiliados', titlefont = f1, tickfont = f2,  zeroline = FALSE, showline = FALSE, showgrid = FALSE, showticklabels = FALSE,
                          range = c(-round(max(abs(aux1$con_clientes))*1.5, digits = 0), round(max(abs(aux1$con_clientes))*1.5, digits = 0))),
             yaxis = list(title='Edad Agru', titlefont = f1, tickfont = f2),
             font = list(color = 'lightgrey'),
             paper_bgcolor='transparent',
             plot_bgcolor='transparent') %>%
      config(displayModeBar = F)

  })

  output$plot1 <- renderPlotly({
    data_plot <- data_f() %>%
      dplyr::select(id_empresa,seg_alto:seg_medio) %>%
      group_by(id_empresa) %>%
      summarise(Basico = sum(seg_basico),
                Medio = sum(seg_medio),
                Joven = sum(seg_joven),
                Alto = sum(seg_alto)) %>%
      gather(key = "Segmento", value = "Conteo", 2:5)

    m <- list(l = 0,r = 0,b = 100,t = 100, pad = 0)
    colors <- c('rgb(128,133,133)', 'rgb(144,103,167)', 'rgb(171,104,87)', 'rgb(114,147,203)')
    f1 <- list(family = "Arial, sans-serif", size = 18, color = "lightgrey")
    f2 <- list(family = "Old Standard TT, serif", size = 14, color = "lightgrey")

    p1 <- plot_ly(data_plot, labels = ~Segmento, values = ~Conteo, type = 'pie',hole = 0,
                  textinfo = 'label+percent',
                  insidetextfont = list(color = '#FFFFFF'),
                  hoverinfo = 'text',text = ~paste('Total empleados:', Conteo),showlegend = T,
                  marker = list(colors = colors,
                                line = list(color = '#FFFFFF', width = 2))) %>%
      layout(margin = m,
             title = 'Participación por Segmento',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, titlefont = f1, tickfont = f2),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, titlefont = f1, tickfont = f2),
             legend = list(x = 0.8, y = 0.5),
             font = list(color = 'lightgrey'),
             paper_bgcolor='transparent',
             plot_bgcolor='transparent') %>%
      config(displayModeBar = F)
    p1
  })

  output$plot2 <- renderPlotly({
    data_plot <- data_f()
    data_plot <- data_plot %>%
      dplyr::select(id_empresa,cat_a:cat_c) %>%
      group_by(id_empresa) %>%
      summarise(A = sum(cat_a),
                B = sum(cat_b),
                C = sum(cat_c)) %>%
      gather(key = "Categoria", value = "Conteo", 2:4)

    m <- list(l = 0,r = 0,b = 100,t = 100, pad = 0)
    colors <- c('rgb(333,133,133)', 'rgb(111,103,167)', 'rgb(222,104,87)', 'rgb(114,147,203)')
    f1 <- list(family = "Arial, sans-serif", size = 18, color = "lightgrey")
    f2 <- list(family = "Old Standard TT, serif", size = 14, color = "lightgrey")
    
    p1 <- plot_ly(data_plot, labels =~ Categoria, values = ~Conteo, type = 'pie',hole = 0,
                  textinfo = 'label+percent',
                  insidetextfont = list(color = '#FFFFFF'),
                  hoverinfo = 'text',text = ~paste('Total empleados:', Conteo),showlegend = T,
                  marker = list(colors = colors,
                                line = list(color = '#FFFFFF', width = 2))) %>%
      layout(margin = m,
             title = 'Participación por Categoria',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, titlefont = f1, tickfont = f2),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, titlefont = f1, tickfont = f2),
             legend = list(x = 0.8, y = 0.5),
             font = list(color = 'lightgrey'),
             paper_bgcolor='transparent',
             plot_bgcolor='transparent') %>%
      config(displayModeBar = F)
    p1
  })

  # ====== Segunda fila ----------------------

  output$conteo_famisanar <- renderValueBox({
    data_f<-data_f() %>%
      select(id_empresa,num_famisanar,afiliados) %>%
      group_by(id_empresa) %>%
      summarise(famisanar = sum(num_famisanar),
                Afiliados = sum(afiliados))
    valueBox(
      value = paste(formatC(100*data_f$famisanar/data_f$Afiliados,
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(data_f$famisanar),")",sep = ""),
      subtitle = "Famisanar",
      icon = icon("briefcase"),
      color = "purple"
    )
  })

  output$conteo_ips <- renderValueBox({
    data_f<-data_f() %>%
      select(id_empresa,num_ips_colsubsidio,num_famisanar) %>%
      group_by(id_empresa) %>%
      summarise(famisanar = sum(num_famisanar),
                ips_colsubsidio = sum(num_ips_colsubsidio))
    valueBox(
      value = paste(formatC(100*data_f$ips_colsubsidio/data_f$famisanar,
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(data_f$ips_colsubsidio),")",sep = ""),
      subtitle = "IPS Colsubsidio",
      icon = icon("briefcase"),
      color = "purple"
    )
  })

  output$conteo_pac_famisanar <- renderValueBox({
    data_f<-data_f() %>%
      select(id_empresa,num_pac_famisanar,num_famisanar) %>%
      group_by(id_empresa) %>%
      summarise(famisanar = sum(num_famisanar),
                pac_famisanar = sum(num_pac_famisanar))
    valueBox(
      value = paste(formatC(100*data_f$pac_famisanar/data_f$famisanar,
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(data_f$pac_famisanar),")",sep = ""),
      subtitle = "PAC Famisanar",
      icon = icon("briefcase"),
      color = "purple"
    )
  })

  output$conteo_sura_eps <- renderValueBox({
    data_f<-data_f() %>%
      select(id_empresa,num_suramericana,afiliados) %>%
      group_by(id_empresa) %>%
      summarise(suramericana = sum(num_suramericana),
                Afiliados = sum(afiliados))
    valueBox(
      value = paste(formatC(100*data_f$suramericana/data_f$Afiliados,
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(data_f$suramericana),")",sep = ""),
      subtitle = "Suramericana",
      icon = icon("briefcase"),
      color = "purple"
    )
  })

  output$conteo_pac_sura <- renderValueBox({
    data_f<-data_f() %>%
      select(id_empresa,num_pac_suramericana,num_suramericana) %>%
      group_by(id_empresa) %>%
      summarise(pac_suramericana = sum(num_pac_suramericana),
                suramericana = sum(num_suramericana))
    valueBox(
      value = paste(formatC(100*data_f$pac_suramericana/data_f$suramericana,
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(data_f$pac_suramericana),")",sep = ""),
      subtitle = "Pac Suramericana",
      icon = icon("briefcase"),
      color = "purple"
    )
  })

  output$pro_salario <- renderValueBox({
    data_f<-data_f() %>%
      select(id_empresa,pro_salario) %>%
      group_by(id_empresa) %>%
      summarise(Promedio.de.Salario = mean(pro_salario))
    valueBox(
      value = paste("$ ",formatC(data_f$Promedio.de.Salario,digits = 0, format = "d", big.mark=","), sep = " "),
      subtitle = "Promedio Salario",
      icon = icon("credit-card"),
      color = "purple"
    )
  })

  output$pro_edad <- renderValueBox({
    data_f<-data_f() %>%
      select(id_empresa,pro_edad) %>%
      group_by(id_empresa) %>%
      summarise(Promedio.de.edad = mean(pro_edad))
    valueBox(
      value = formatC(data_f$Promedio.de.edad,digits = 0, format = "d", big.mark=","),
      subtitle = "Promedio Edad",
      icon = icon("user"),
      color = "purple"
    )
  })
  
  output$conteo_cupo_credito <- renderValueBox({
    data_f<-data_f() %>%
      select(id_empresa,cupo_credito,afiliados) %>%
      group_by(id_empresa) %>%
      summarise(cupo_credito = sum(cupo_credito),
                Afiliados = sum(afiliados))
    valueBox(
      value = paste(formatC(100*data_f$cupo_credito/data_f$Afiliados,
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(data_f$cupo_credito),")",sep = ""),
      subtitle = "Credito Cupo",
      icon = icon("credit-card"),
      color = "purple"
    )
  })
  
  output$conteo_vivienda <- renderValueBox({
    data_f<-data_f_c() %>%
      select(id_empresa,compra_vivienda) %>%
      group_by(id_empresa) %>%
      summarise(Compra_vivienda = sum(compra_vivienda))
    valueBox(
      value = formatC(data_f$Compra_vivienda, digits = 0, format = "f", big.mark=","),
      subtitle = "Proyecto de Vivienda (Fecha en entrega)",
      icon = icon("home"),
      color = "teal"
    )
  })

  output$conteo_educacion <- renderValueBox({
    data_f<-data_f_c() %>%
      select(id_empresa,educacion) %>%
      group_by(id_empresa) %>%
      summarise(Educacion = sum(educacion))
    valueBox(
      value = formatC(data_f$Educacion,digits = 0, format = "f", big.mark=","),
      subtitle = "Educacion (Niños en colegios)",
      icon = icon("graduation-cap"),
      color = "teal"
    )
  })

  output$conteo_consumo_credito <- renderValueBox({
    data_f<-data_f_c() %>%
      select(id_empresa,consumo_credito,afiliados) %>%
      group_by(id_empresa) %>%
      summarise(Consumo_credito = sum(consumo_credito),
                Afiliados = sum(afiliados))
    valueBox(
      value = paste(formatC(100*data_f$Consumo_credito/data_f$Afiliados,
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(data_f$Consumo_credito),")",sep = ""),
      subtitle = "Crédito Consumo",
      icon = icon("credit-card"),
      color = "teal"
    )
  })

  output$conteo_uso_mes <- renderValueBox({
    data_f<-data_f_c() %>%
      select(id_empresa,uso_mes,afiliados) %>%
      group_by(id_empresa) %>%
      summarise(uso_mes = sum(uso_mes),
                Afiliados = sum(afiliados))
    valueBox(
      value = paste(formatC(100*data_f$uso_mes/data_f$Afiliados,
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(data_f$uso_mes),")",sep = ""),
      subtitle = "Uso TMS Convenios",
      icon = icon("ok", lib = "glyphicon"),
      color = "teal"
    )
  })
  
  # Salud
  output$conteo_salud <- renderValueBox({
    data_f<-data_f_c() %>%
      select(id_empresa,salud,afiliados) %>%
      group_by(id_empresa) %>%
      summarise(Salud = sum(salud),
                Afiliados = sum(afiliados))
    valueBox(
      value = paste(formatC(100*data_f$Salud/data_f$Afiliados,
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(data_f$Salud),")",sep = ""),
      subtitle = "Salud",
      icon = icon("briefcase"),
      color = "teal"
    )
  })
  # Supermecados
  output$conteo_supermercado <- renderValueBox({
    data_f<-data_f_c() %>%
      select(id_empresa,supermercados,afiliados) %>%
      group_by(id_empresa) %>%
      summarise(Supermercados = sum(supermercados),
                Afiliados = sum(afiliados))
    valueBox(
      value = paste(formatC(100*data_f$Supermercados/data_f$Afiliados,
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(data_f$Supermercados),")",sep = ""),
      subtitle = "Supermercado",
      icon = icon("cart-plus"),
      color = "teal"
    )
  })

  # Medicamentos
  output$conteo_drogueria <- renderValueBox({
    data_f<-data_f_c() %>%
      select(id_empresa,medicamentos,afiliados) %>%
      group_by(id_empresa) %>%
      summarise(Medicamentos = sum(medicamentos),
                Afiliados = sum(afiliados))
    valueBox(
      value = paste(formatC(100*data_f$Medicamentos/data_f$Afiliados,
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(data_f$Medicamentos),")",sep = ""),
      subtitle = "Droguerias",
      icon = icon("plus"),
      color = "teal"
    )
  })

  # RyT
  output$conteo_ryt_club <- renderValueBox({
    data_f<-data_f_c() %>%
      select(id_empresa,club,afiliados) %>%
      group_by(id_empresa) %>%
      summarise(Club = sum(club),
                Afiliados = sum(afiliados))
    valueBox(
      value = paste(formatC(100*data_f$Club/data_f$Afiliados,
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(data_f$Club),")",sep = ""),
      subtitle = "Recreacion y Turismo - Club",
      icon = icon("rocket"),
      color = "teal"
    )
  })

  output$conteo_ryt_hoteles <- renderValueBox({
    data_f<-data_f_c() %>%
      select(id_empresa,hotel,afiliados) %>%
      group_by(id_empresa) %>%
      summarise(Hotel = sum(hotel),
                Afiliados = sum(afiliados))
    valueBox(
      value = paste(formatC(100*data_f$Hotel/data_f$Afiliados,
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(data_f$Hotel),")",sep = ""),
      subtitle = "Recreacion y Turismo - Hoteles",
      icon = icon("rocket"),
      color = "teal"
    )
  })

  output$conteo_ryt_piscilago <- renderValueBox({
    data_f<-data_f_c() %>%
      select(id_empresa,piscilago,afiliados) %>%
      group_by(id_empresa) %>%
      summarise(Piscilago = sum(piscilago),
                Afiliados = sum(afiliados))
    valueBox(
      value = paste(formatC(100*data_f$Piscilago/data_f$Afiliados,
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(data_f$Piscilago),")",sep = ""),
      subtitle = "Recreacion y Turismo - Piscilago",
      icon = icon("rocket"),
      color = "teal"
    )
  })

  output$conteo_ryt <- renderValueBox({
    data_f<-data_f_c() %>%
      select(id_empresa,ryt,afiliados) %>%
      group_by(id_empresa) %>%
      summarise(RyT = sum(ryt),
                Afiliados = sum(afiliados))
    valueBox(
      value = paste(formatC(100*data_f$RyT/data_f$Afiliados,
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(data_f$RyT),")",sep = ""),
      subtitle = "Recreacion y Turismo",
      icon = icon("rocket"),
      color = "teal"
    )
  })

  # ====== tercera fila ----------------------

  output$conteo_kit <- renderValueBox({
    data_f<-data_f() %>%
      select(id_empresa,kit_redimido,kit_derecho) %>%
      group_by(id_empresa) %>%
      summarise(kit_redimido = sum(kit_redimido),
                kit_derecho = sum(kit_derecho))
    valueBox(
      value = paste(formatC(100*data_f$kit_redimido/data_f$kit_derecho,
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(data_f$kit_redimido),"/",comma(data_f$kit_derecho),")",sep = ""),
      subtitle = "Kit Escolar",
      icon = icon("book"),
      color = "green"
    )
  })

  output$conteo_monetarias <- renderValueBox({
    data_f<-data_f() %>%
      select(id_empresa,cuota_redimida,cuota_derecho) %>%
      group_by(id_empresa) %>%
      summarise(Cuota_redimida = sum(cuota_redimida),
                Cuota_derecho = sum(cuota_derecho))
    valueBox(
      value = paste(formatC(100*data_f$Cuota_redimida/data_f$Cuota_derecho,
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(data_f$Cuota_redimida),"/",comma(data_f$Cuota_derecho),")",sep = ""),
      subtitle = "Cuotas Monetarias",
      icon = icon("play"),
      color = "green"
    )
  })

  output$conteo_data <- renderValueBox({
    data_f<-data_f() %>%
      select(id_empresa,habeas_data,afiliados) %>%
      group_by(id_empresa) %>%
      summarise(Habeas_data = sum(habeas_data),
                Afiliados = sum(afiliados))
    valueBox(
      value = paste(formatC(100*data_f$Habeas_data/data_f$Afiliados,
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(data_f$Habeas_data),")",sep = ""),
      subtitle = "Habeas Data",
      icon = icon("database"),
      color = "purple"
    )
  })

  output$conteo_lonchera <- renderValueBox({
    data_f<-data_f() %>%
      select(id_empresa,bono_redimido,bono_derecho) %>%
      group_by(id_empresa) %>%
      summarise(Bono_redimido = sum(bono_redimido),
                Bono_derecho = sum(bono_derecho))
    valueBox(
      value = paste(formatC(100*data_f$Bono_redimido/data_f$Bono_derecho,
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(data_f$Bono_redimido),"/",comma(data_f$Bono_derecho),")",sep = ""),
      subtitle = "Bono Lonchera",
      icon = icon("star"),
      color = "green"
    )
  })

  # output$conteo_subsidio <- renderValueBox({
  #   data_f<-data_f() %>%
  #     select(id_empresa,Auxilios_pago,Afiliados) %>%
  #     group_by(id_empresa) %>%
  #     summarise(Auxilios_pago = sum(Auxilios_pago),
  #               Afiliados = sum(Afiliados))
  #   valueBox(
  #     value = paste(formatC(100*data_f$Auxilios_pago/data_f$Afiliados,
  #                           digits = 1, format = "f", big.mark=","),"%", " (",data_f$Auxilios_pago,")",sep = ""),
  #     subtitle = "Otros auxilios entregados",
  #     icon = icon("telegram"),
  #     color = "purple"
  #   )
  # })

  output$conteo_subsidio_vivienda <- renderValueBox({
    data_f<-data_f() %>%
      select(id_empresa,subsidio_asignado,afiliados) %>%
      group_by(id_empresa) %>%
      summarise(Subsidio_asignado = sum(subsidio_asignado),
                Afiliados = sum(afiliados))
    valueBox(
      value = paste(formatC(100*data_f$Subsidio_asignado/data_f$Afiliados,
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(data_f$Subsidio_asignado),")",sep = ""),
      subtitle = "Subsidio Vivienda",
      icon = icon("hotel"),
      color = "purple"
    )
  })

  # ====== cuarta fila ----------------------

  output$conteo_hoteles <- renderValueBox({
    data_f<-data_f() %>%
      select(id_empresa,pros_hotel,afiliados) %>%
      group_by(id_empresa) %>%
      summarise(Hoteles_prospectos = sum(pros_hotel),
                Afiliados = sum(afiliados))
    valueBox(
      value = paste(formatC(100*data_f$Hoteles_prospectos/data_f$Afiliados,
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(data_f$Hoteles_prospectos),")",sep = ""),
      subtitle = "Hoteles Prospectos",
      icon = icon("bicycle"),
      color = "purple"
    )
  })

  output$conteo_piscilago <- renderValueBox({
    data_f<-data_f() %>%
      select(id_empresa,pros_pisi,afiliados) %>%
      group_by(id_empresa) %>%
      summarise(Piscilago_prospectos = sum(pros_pisi),
                Afiliados = sum(afiliados))
    valueBox(
      value = paste(formatC(100*data_f$Piscilago_prospectos/data_f$Afiliados,
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(data_f$Piscilago_prospectos),")",sep = ""),
      subtitle = "Piscilago Prospectos",
      icon = icon("bicycle"),
      color = "purple"
    )
  })

  output$conteo_club <- renderValueBox({
    data_f<-data_f() %>%
      select(id_empresa,pros_club,afiliados) %>%
      group_by(id_empresa) %>%
      summarise(Club_prospectos = sum(pros_club),
                Afiliados = sum(afiliados))
    valueBox(
      value = paste(formatC(100*data_f$Club_prospectos/data_f$Afiliados,
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(data_f$Club_prospectos),")",sep = ""),
      subtitle = "Clubes Prospectos",
      icon = icon("bicycle"),
      color = "purple"
    )
  })

  output$pre_aprobado_hipotecario <- renderValueBox({
    data_f<-data_f() %>%
      select(id_empresa,pre_aprobado_hipo,afiliados) %>%
      group_by(id_empresa) %>%
      summarise(pre_aprobado_hipotecario = sum(pre_aprobado_hipo),
                Afiliados = sum(afiliados))
    valueBox(
      value = paste(formatC(100*sum(data_f$pre_aprobado_hipotecario, na.rm = T)/sum(data_f$Afiliados, na.rm = T),
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(sum(data_f$pre_aprobado_hipotecario, na.rm = T)),")",sep = ""),
      subtitle = "Pre Aprobado Hipotecario",
      icon = icon("eraser"),
      color = "olive"
    )
  })

  output$pre_aprobado_cupo <- renderValueBox({
    data_f<-data_f() %>%
      select(id_empresa,pre_aprobado_cupo,afiliados) %>%
      group_by(id_empresa) %>%
      summarise(pre_aprobado_cupo = sum(pre_aprobado_cupo),
                Afiliados = sum(afiliados))
    valueBox(
      value = paste(formatC(100*sum(data_f$pre_aprobado_cupo, na.rm = T)/sum(data_f$Afiliados, na.rm = T),
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(sum(data_f$pre_aprobado_cupo, na.rm = T)),")",sep = ""),
      subtitle = "Pre Aprobado Cupo",
      icon = icon("eraser"),
      color = "olive"
    )
  })

  output$conteo_cuad_a <- renderValueBox({
    data_f<-data_f() %>%
      select(id_empresa,a3,afiliados) %>%
      group_by(id_empresa) %>%
      summarise(Cuad_A = sum(a3),
                Afiliados = sum(afiliados))
    valueBox(
      value = paste(formatC(100*data_f$Cuad_A/data_f$Afiliados,
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(data_f$Cuad_A),")",sep = ""),
      subtitle = "Cuadrante A: En caso de tener Crédito hipotecario sin desembolsar,  sin tener otorgado necesariamente subsidio de vivienda",
      icon = icon("home"),
      color = "blue"
    )
  })

  output$conteo_cuad_a1 <- renderValueBox({
    data_f<-data_f() %>%
      select(id_empresa,a1,afiliados) %>%
      group_by(id_empresa) %>%
      summarise(Cuad_A1 = sum(a1),
                Afiliados = sum(afiliados))
    valueBox(
      value = paste(formatC(100*data_f$Cuad_A1/data_f$Afiliados,
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(data_f$Cuad_A1),")",sep = ""),
      subtitle = "Cuadrante A1: Con subsidio asignado con vigencia  menor a 6 meses",
      icon = icon("home"),
      color = "blue"
    )
  })

  output$conteo_cuad_a2 <- renderValueBox({
    data_f<-data_f() %>%
      select(id_empresa,a2,afiliados) %>%
      group_by(id_empresa) %>%
      summarise(Cuad_A2 = sum(a2),
                Afiliados = sum(afiliados))
    valueBox(
      value = paste(formatC(100*data_f$Cuad_A2/data_f$Afiliados,
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(data_f$Cuad_A2),")",sep = ""),
      subtitle = "Cuadrante A2: Con subsidio asignado con vigencia  mayor de  6 meses",
      icon = icon("home"),
      color = "blue"
    )
  })

  output$conteo_cuad_b <- renderValueBox({
    data_f<-data_f() %>%
      select(id_empresa,b1,afiliados) %>%
      group_by(id_empresa) %>%
      summarise(Cuad_B = sum(b1),
                Afiliados = sum(afiliados))
    valueBox(
      value = paste(formatC(100*data_f$Cuad_B/data_f$Afiliados,
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(data_f$Cuad_B),")",sep = ""),
      subtitle = "Cuadrante B: Afiliados con interés de compra (Basado en encuestas y diagnóstico de necesidades)",
      icon = icon("home"),
      color = "blue"
    )
  })

  output$downloadData2 <- downloadHandler(
    filename = function(){
      paste("Empresas_ind", Sys.time(), ".csv", sep = "")
    },
    content = function(file){
      fwrite(data_f(), file, row.names = F)
    }
  )
  
  output$consumo_valor_ind <- renderPlotly({
    data_plot <- data_f() %>% 
      left_join(consumo_emp, by = "id_empresa") %>% 
      group_by(ues) %>% 
      summarise(consumo = round(sum(consumo, na.rm = T)/1000000,1)) %>% 
      data.frame()
    
    m <- list(l = 50,r = 50,b = 100,t = 100, pad = 0)
    colors <- c('rgb(333,133,133)', 'rgb(111,103,167)', 'rgb(222,104,87)', 'rgb(114,147,203)')
    f1 <- list(family = "Arial, sans-serif",size = 18,color = "lightgrey")
    f2 <- list(family = "Old Standard TT, serif",size = 14,color = "lightgrey")
    
    p1 <- plot_ly(data_plot, x = ~ues, y = ~consumo, type = 'bar', text = ~comma(consumo), textposition = 'auto') %>%
      layout(margin = m,
             title = 'Consumo por UES (Millones)',
             font = list(color = 'lightgrey'),
             xaxis = list(showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE, titlefont = f1, tickfont = f2, tickcolor = 'rgb(127,127,127)'),
             yaxis = list(showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE, titlefont = f1, tickfont = f2, tickcolor = 'rgb(127,127,127)'),
             legend = list(x = 0.8, y = 0.5),
             paper_bgcolor='transparent',
             plot_bgcolor='transparent') %>%
      config(displayModeBar = F)
    p1
  })
  
  output$consumo_transa_ind <- renderPlotly({
    data_plot <- data_f() %>% 
      left_join(consumo_emp, by = "id_empresa") %>% 
      group_by(servicio) %>% 
      summarise(consumo = round(sum(consumo, na.rm = T)/1000000,1)) %>% 
      data.frame()
    
    m <- list(l = 50,r = 50,b = 100,t = 100, pad = 0)
    colors <- c('rgb(333,133,133)', 'rgb(111,103,167)', 'rgb(222,104,87)', 'rgb(114,147,203)')
    f1 <- list(family = "Arial, sans-serif",size = 18,color = "lightgrey")
    f2 <- list(family = "Old Standard TT, serif",size = 14,color = "lightgrey")
    
    p1 <- plot_ly(data_plot, x = ~servicio, y = ~consumo, type = 'bar', text = ~comma(consumo), textposition = 'auto') %>%
      layout(margin = m,
             title = 'Consumo por Servicio (Millones)',
             font = list(color = 'lightgrey'),
             xaxis = list(showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE, titlefont = f1, tickfont = f2, tickcolor = 'rgb(127,127,127)'),
             yaxis = list(showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE, titlefont = f1, tickfont = f2, tickcolor = 'rgb(127,127,127)'),
             legend = list(x = 0.8, y = 0.5),
             paper_bgcolor='transparent',
             plot_bgcolor='transparent') %>%
      config(displayModeBar = F)
    p1
  })
  
})



