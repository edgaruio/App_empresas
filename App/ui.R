
dashboardPage(skin = "blue",
              dashboardHeader(title = "Empresas"),
              dashboardSidebar(
                sidebarMenu(disable = TRUE,br(),
                            tags$img(src = "logo.png", height=40, width=200, align="center"),
                            tags$hr(),
                            shinyjs::hidden(menuItem("INSTRUCCIONES", tabName = "dummy")),
                            tags$hr(),
                            menuItem("GLOBAL", tabName = "global", icon = icon("area-chart"),
                                     menuSubItem("Check",tabName = "global",icon = icon("check-circle")),
                                     selectInput("xpiramide1", label = "Piramide 1:",choices = name_piramide1,
                                                 selected = "Total", multiple = F),
                                     selectInput("xpiramide2", label = "Piramide 2:",choices = name_piramide2,
                                                 selected = "Total", multiple = F),
                                     selectInput("xactividad", label = "Actividad:",choices = name_actividad,
                                                 selected = "Total", multiple = F),
                                     actionButton("go2", label = "Aplicar Filtros")),
                            tags$hr(),
                            menuItem("INDIVIDUAL", tabName = "individual", icon = icon("area-chart"),
                                     menuSubItem("Check",tabName = "individual",icon = icon("check-circle")),
                                     radioButtons("tipodoc","Tipo de documento",
                                                  choices = c("NIT" = "NIT",
                                                              "Cédula de Ciudadania" = "CC",
                                                              "Tarjeta de Identidad" = "TI",
                                                              "Registro Civil" = "RC",
                                                              "Cedula de Extranjería" = "CE",
                                                              "NUIP" = "NUIP",
                                                              "Pasaporte" = "PAS",
                                                              "Carnet Diplomatico" = "CD"),
                                                  selected = "NIT"),
                                     textInput("nit_empresa","Identificación Empresa",value = "8600073861"),
                                     actionButton("go", label = "Aplicar Filtros"),
                                     tags$hr()))),
              dashboardBody(
                shinyDashboardThemes(
                  theme = "purple_gradient"
                ),
                tags$style(type="text/css",
                           ".shiny-output-error { visibility: hidden; }",
                           ".shiny-output-error:before { visibility: hidden; }"),
                tabItems(
                  tabItem("dummy",
                          fluidRow(
                            column(1),
                            column(10,
                                   h1("INSTRUCCIONES"),
                                   tags$video(src = "Presentacion1.mp4", type = "video/mp4", autoplay = T, controls = NA),
                                   br(),br(),
                                   h3("Nota: Esta aplicación muestra consumos de afiliados por empresa (s) sin tener en cuenta agregados por grupo empresa.")
                            ),
                            column(1)
                          )
                  ),
                  ### Global ====
                  tabItem(tabName = "global",
                          fluidPage(
                            title = "DB",
                            fluidRow(
                              box(title = "Consolidado Información (Corte Mayo)",width=12,status="primary",solidHeader=TRUE,collapsible=FALSE,collapsed = TRUE,
                                  fluidRow(
                                    valueBoxOutput("info_afil_emp",width = 3),
                                    valueBoxOutput("info_afil_emp2",width = 3),
                                    valueBoxOutput("info_empresas_prin",width = 2),
                                    valueBoxOutput("info_empresas_fil",width = 2),
                                    valueBoxOutput("info_afil_glob",width = 2)
                                  ),
                                  fluidRow(
                                    column(width = 4,
                                           withLoader(plotlyOutput("plot_pira_glob", height = 500), type = "html", loader = "loader1")),
                                    column(width = 4,
                                           withLoader(plotlyOutput("plot1_glob", height = 500), type = "html", loader = "loader1")),
                                    column(width = 4,
                                           withLoader(plotlyOutput("plot2_glob", height = 500), type = "html", loader = "loader1"))
                                    )
                                  )
                            ),
                            fluidRow(
                              box(title = "Gestion de Afiliados Cobertura",width=12,status="primary",solidHeader=FALSE,collapsible=TRUE,collapsed = TRUE,
                                  br(),
                                  h3("Corte Mayo"),
                                  br(),
                                  fluidRow(
                                    valueBoxOutput("conteo_famisanar_glob",width = 3),
                                    valueBoxOutput("conteo_pac_famisanar_glob",width = 3),
                                    valueBoxOutput("conteo_sura_eps_glob",width = 3),
                                    valueBoxOutput("conteo_pac_sura_glob",width = 3),
                                    valueBoxOutput("conteo_ips_glob",width = 3),
                                    valueBoxOutput("pro_salario_glob",width = 3),
                                    valueBoxOutput("pro_edad_glob",width = 3),
                                    valueBoxOutput("conteo_cupo_credito_glob",width = 3)
                                  ),
                                  br(),
                                  fluidRow(
                                    column(2,
                                           h3("Seleccione Consumos"),
                                           br()
                                    ),
                                    column(3,
                                           radioButtons("xagrupador_glob", label = "",
                                                        choices = c("12 meses" = "12 meses",
                                                                    "Último mes" = "ultimo mes"),
                                                        selected = "12 meses"))
                                  ),
                                  br(),
                                  fluidRow(
                                    valueBoxOutput("conteo_consumo_credito_glob",width = 3),
                                    valueBoxOutput("conteo_uso_mes_glob",width = 3),
                                    valueBoxOutput("conteo_salud_glob",width = 3),
                                    valueBoxOutput("conteo_supermercado_glob",width = 3),
                                    valueBoxOutput("conteo_drogueria_glob",width = 3),
                                    valueBoxOutput("conteo_vivienda_glob",width = 3),
                                    valueBoxOutput("conteo_educacion_glob",width = 3),
                                    valueBoxOutput("conteo_ryt_hoteles_glob",width = 3),
                                    valueBoxOutput("conteo_ryt_club_glob",width = 3),
                                    valueBoxOutput("conteo_ryt_piscilago_glob",width = 3),
                                    valueBoxOutput("conteo_ryt_glob",width = 3)
                                  )
                              )
                            ),
                            fluidRow(
                              box(title = "Gestion de Derechos",width=12,status="primary",solidHeader=FALSE,collapsible=TRUE,collapsed = TRUE,
                                  fluidRow(
                                    valueBoxOutput("conteo_kit_glob",width = 4),
                                    valueBoxOutput("conteo_monetarias_glob",width = 4),
                                    valueBoxOutput("conteo_lonchera_glob",width = 4)
                                    ),
                                  fluidRow(
                                    valueBoxOutput("conteo_data_glob",width = 6),
                                    valueBoxOutput("conteo_subsidio_vivienda_glob",width = 6)
                                  )
                              )
                            ),
                            fluidRow(
                              box(title = "Prospectos",width=12,status="primary",solidHeader=FALSE,collapsible=TRUE,collapsed = TRUE,
                                  fluidRow(
                                    valueBoxOutput("conteo_hoteles_glob",width = 4),
                                    valueBoxOutput("conteo_piscilago_glob",width = 4),
                                    valueBoxOutput("conteo_club_glob",width = 4)
                                  ),
                                  fluidRow(
                                    valueBoxOutput("pre_aprobado_hipotecario_glob",width = 6),
                                    valueBoxOutput("pre_aprobado_cupo_glob",width = 6)
                                  ),
                                  fluidRow(
                                    valueBoxOutput("conteo_cuad_a_glob",width = 3),
                                    valueBoxOutput("conteo_cuad_a1_glob",width = 3),
                                    valueBoxOutput("conteo_cuad_a2_glob",width = 3),
                                    valueBoxOutput("conteo_cuad_b_glob",width = 3)
                                  )
                              )
                            ),
                            fluidRow(
                              box(title = "Consumo Empresarial (Últimos 12 meses)", width = 12, status = "primary", solidHeader = FALSE, collapsible = TRUE, collapsed = TRUE,
                                  fluidRow(
                                    br(),
                                    column(6,
                                           # h3("Consumo por UES"),br(),
                                           plotlyOutput("consumo_valor_glob", height = 500) #,
                                           # valueBoxOutput("conteo_ues1",width = 6),
                                           # valueBoxOutput("conteo_ues2",width = 6),
                                           # valueBoxOutput("conteo_ues3",width = 6),
                                           # valueBoxOutput("conteo_ues4",width = 6),
                                           # valueBoxOutput("conteo_ues5",width = 6)
                                           ),
                                    column(6,
                                           # h3("Consumo por Piramide"),br(),
                                           plotlyOutput("consumo_transa_glob", height = 500) #,
                                           # valueBoxOutput("conteo_pir1",width = 3),
                                           # valueBoxOutput("conteo_pir2",width = 3),
                                           # valueBoxOutput("conteo_pir3",width = 3),
                                           # valueBoxOutput("conteo_pir4",width = 3)
                                           ),
                                    br()
                                    ))
                              ),
                            downloadButton("downloadData", "Descargar Base Empresas"))
                  ),
                  ### Individual ====
                  tabItem(tabName = "individual",
                          fluidPage(
                            title = "DB",
                            fluidRow(
                              box(title = "Información por empresa (Corte Mayo)",width=12,status="primary",solidHeader=TRUE,collapsible=FALSE,collapsed = TRUE,
                                  fluidRow(
                                    valueBoxOutput("info_empresa",width = 9),
                                    valueBoxOutput("conteo_empleados",width = 3)
                                  ),
                                  fluidRow(
                                    valueBoxOutput("info_pir1",width = 4),
                                    valueBoxOutput("info_pir2",width = 4),
                                    valueBoxOutput("info_cluster",width = 4)
                                  ),
                                  fluidRow(br(),
                                           column(width = 4,
                                                  withLoader(plotlyOutput("plot_pira_ind", height = 500), type = "html", loader = "loader1")),
                                           column(width = 4,
                                                  withLoader(plotlyOutput("plot1", height = 500), type = "html", loader = "loader1")),
                                           column(width = 4,
                                                  withLoader(plotlyOutput("plot2", height = 500), type = "html", loader = "loader1")),
                                           br()
                                  )
                              )
                            ),
                            fluidRow(
                              box(title = "Gestion de Afiliados Cobertura",width=12,status="primary",solidHeader=FALSE,collapsible=TRUE,collapsed = TRUE,
                                  br(),
                                  h3("Corte Mayo"),
                                  br(),
                                  fluidRow(
                                    valueBoxOutput("conteo_famisanar",width = 3),
                                    bsTooltip("conteo_famisanar", "Afiliados con cobertura en Famisanar. Tiene como base el total de afiliados de UNIANDES", 
                                              placement = "bottom", trigger = "click",options = NULL),
                                    valueBoxOutput("conteo_pac_famisanar",width = 3),
                                    bsTooltip("conteo_pac_famisanar", "Afilidos con cobertura Plan Atención Complementario Famisanar. Tiene como base el total de afiliados con cobertura en Famisanar", 
                                              placement = "bottom", trigger = "click",options = NULL),
                                    valueBoxOutput("conteo_sura_eps",width = 3),
                                    bsTooltip("conteo_sura_eps", "Afiliados con cobertura Suramericana. Tiene como base el total de afiliados de UNIANDES", 
                                              placement = "bottom", trigger = "click",options = NULL),
                                    valueBoxOutput("conteo_pac_sura",width = 3),
                                    bsTooltip("conteo_pac_sura", "Afilidos con cobertura Plan Atención Complementario Suramericana. Tiene como base el total de afiliados con cobertura en Suramericana", 
                                              placement = "bottom", trigger = "click",options = NULL),
                                    valueBoxOutput("conteo_ips",width = 3),
                                    bsTooltip("conteo_ips", "Afiliados con cobertura IPS Colsubsidio. Tiene como base el total de afiliados con cobertura Famisanar", 
                                              placement = "bottom", trigger = "click",options = NULL),
                                    valueBoxOutput("pro_salario",width = 3),
                                    bsTooltip("pro_salario", "Promedio Salario", 
                                              placement = "bottom", trigger = "click",options = NULL),
                                    valueBoxOutput("pro_edad",width = 3),
                                    bsTooltip("pro_edad", "Promedio Edad", 
                                              placement = "bottom", trigger = "click",options = NULL),
                                    valueBoxOutput("conteo_cupo_credito",width = 3),
                                    bsTooltip("conteo_cupo_credito", "Afiliados con Crédito Cupo Activo", 
                                              placement = "bottom", trigger = "click",options = NULL)
                                  ),
                                  br(),
                                  fluidRow(
                                    column(2,
                                           h3("Seleccione Consumos"),
                                           br()
                                    ),
                                    column(3,
                                           radioButtons("xagrupador_ind", label = "",
                                                        choices = c("12 meses" = "12 meses",
                                                                    "Último mes" = "ultimo mes"),
                                                        selected = "12 meses"))
                                  ),
                                  br(),
                                  fluidRow(
                                    valueBoxOutput("conteo_consumo_credito",width = 3),
                                    bsTooltip("conteo_consumo_credito", "Afiliados con Uso de Créditos de Consumo", 
                                              placement = "bottom", trigger = "click",options = NULL),
                                    valueBoxOutput("conteo_uso_mes",width = 3),
                                    bsTooltip("conteo_uso_mes", "Afilidos con Uso de TMS en Convenios", 
                                              placement = "bottom", trigger = "click",options = NULL),
                                    valueBoxOutput("conteo_salud",width = 3),
                                    bsTooltip("conteo_salud", "Afiliados con consumo de productos de Salud", 
                                              placement = "bottom", trigger = "click",options = NULL),
                                    valueBoxOutput("conteo_supermercado",width = 3),
                                    bsTooltip("conteo_supermercado", "Afiliados con compras en Supermecado", 
                                              placement = "bottom", trigger = "click",options = NULL),
                                    valueBoxOutput("conteo_drogueria",width = 3),
                                    bsTooltip("conteo_drogueria", "Afiliados con compras en Droguerías", 
                                              placement = "bottom", trigger = "click",options = NULL),
                                    valueBoxOutput("conteo_educacion",width = 3),
                                    bsTooltip("conteo_educacion", "Número de hijos en Colegios Colsubsidio", 
                                              placement = "bottom", trigger = "click",options = NULL),
                                    valueBoxOutput("conteo_vivienda",width = 3),
                                    bsTooltip("conteo_vivienda", "Afiliados con compra de vivienda en 2020", 
                                              placement = "bottom", trigger = "click",options = NULL),
                                    valueBoxOutput("conteo_ryt_hoteles",width = 3),
                                    bsTooltip("conteo_ryt_hoteles", "Afiliados con uso de Hoteles Colsubsidio", 
                                              placement = "bottom", trigger = "click",options = NULL),
                                    valueBoxOutput("conteo_ryt_club",width = 3),
                                    bsTooltip("conteo_ryt_club", "Afiliados con uso de Clubes Colsubsidio", 
                                              placement = "bottom", trigger = "click",options = NULL),
                                    valueBoxOutput("conteo_ryt_piscilago",width = 3),
                                    bsTooltip("conteo_ryt_piscilago", "Afiliados con uso de Piscilago", 
                                              placement = "bottom", trigger = "click",options = NULL),
                                    valueBoxOutput("conteo_ryt",width = 3),
                                    bsTooltip("conteo_ryt", "Afiliados con uso de al menos una vez en Hotel, Club, Piscilago", 
                                              placement = "bottom", trigger = "click",options = NULL)
                                  )
                              )
                            ),
                            fluidRow(
                              box(title = "Gestion de Derechos",width=12,status="primary",solidHeader=FALSE,collapsible=TRUE,collapsed = TRUE,
                                  fluidRow(
                                    valueBoxOutput("conteo_kit",width = 4),
                                    bsTooltip("conteo_kit", "Redención kit escolar. Se calcula mediante el cociente del número de afiliados que redimen y el número de afiliados con derecho a kit escolar", 
                                              placement = "bottom", trigger = "click",options = NULL),
                                    valueBoxOutput("conteo_monetarias",width = 4),
                                    bsTooltip("conteo_monetarias", "Redención cuota monetaria. Corresponde al cociente del número de afiliados que redimen y el número de afiliados con derecho a cuota monetaria", 
                                              placement = "bottom", trigger = "click",options = NULL),
                                    valueBoxOutput("conteo_lonchera",width = 4),
                                    bsTooltip("conteo_lonchera", "Redención cuota monetaria. Corresponde al cociente del número de afiliados que redimen y el número de afiliados con derecho a bono lonchera", 
                                              placement = "bottom", trigger = "click",options = NULL)
                                  ),
                                  fluidRow(
                                    valueBoxOutput("conteo_data",width = 6),
                                    bsTooltip("conteo_data", "Corresponde al porcentaje de Afiliados con autorización de contactabilidad", 
                                              placement = "bottom", trigger = "click",options = NULL),
                                    valueBoxOutput("conteo_subsidio_vivienda",width = 6),
                                    bsTooltip("conteo_subsidio_vivienda", "Corresponde al porcentaje de afiliados con Subsidio de Vivienda", 
                                              placement = "bottom", trigger = "click",options = NULL)
                                  )
                              )
                            ),
                            fluidRow(
                              box(title = "Prospectos",width=12,status="primary",solidHeader=FALSE,collapsible=TRUE,collapsed = TRUE,
                                  fluidRow(
                                    valueBoxOutput("conteo_hoteles",width = 4),
                                    bsTooltip("conteo_hoteles", "Corresponde al porcentaje de afiliados Prospecto Hoteles", 
                                              placement = "bottom", trigger = "click",options = NULL),
                                    valueBoxOutput("conteo_piscilago",width = 4),
                                    bsTooltip("conteo_piscilago", "Corresponde al porcentaje de afiliados Prospecto Piscilago", 
                                              placement = "bottom", trigger = "click",options = NULL),
                                    valueBoxOutput("conteo_club",width = 4),
                                    bsTooltip("conteo_club", "Corresponde al porcentaje de afiliados Prospecto Clubes", 
                                              placement = "bottom", trigger = "click",options = NULL)
                                  ),
                                  fluidRow(
                                    valueBoxOutput("pre_aprobado_hipotecario",width = 6),
                                    bsTooltip("pre_aprobado_hipotecario", "Corresponde al porcentaje de afiliados Preaprobados Hipotecario", 
                                              placement = "bottom", trigger = "click",options = NULL),
                                    valueBoxOutput("pre_aprobado_cupo",width = 6),
                                    bsTooltip("pre_aprobado_cupo", "Corresponde al porcentaje de afiliados Preaprobados cupo", 
                                              placement = "bottom", trigger = "click",options = NULL)
                                  ),
                                  fluidRow(
                                    valueBoxOutput("conteo_cuad_a",width = 3),
                                    bsTooltip("conteo_cuad_a", "Afiliados en caso de tener Crédito hipotecario sin desembolsar, sin tener otorgado necesariamente subsidio de vivienda", 
                                              placement = "bottom", trigger = "click",options = NULL),
                                    valueBoxOutput("conteo_cuad_a1",width = 3),
                                    bsTooltip("conteo_cuad_a1", "Afiliados con subsidio asignado con vigencia menor a 6 meses", 
                                              placement = "bottom", trigger = "click",options = NULL),
                                    valueBoxOutput("conteo_cuad_a2",width = 3),
                                    bsTooltip("conteo_cuad_a2", "Afiliados con subsidio asignado con vigencia mayor de 6 meses", 
                                              placement = "bottom", trigger = "click",options = NULL),
                                    valueBoxOutput("conteo_cuad_b",width = 3),
                                    bsTooltip("conteo_cuad_b", "Afiliados con interés de compra (Basado en encuestas y diagnóstico de necesidades)", 
                                              placement = "bottom", trigger = "click",options = NULL)
                                  )
                              )
                            ),
                            fluidRow(
                              box(title = "Consumo por Empresa (Últimos 12 meses)", width = 12, status = "primary", solidHeader = FALSE, collapsible = TRUE, collapsed = TRUE,
                                  fluidRow(
                                    br(),
                                    column(1),
                                    column(10,
                                           h3("Consumo UES"),
                                           valueBoxOutput("conteo_ues1",width = 6),
                                           valueBoxOutput("conteo_ues2",width = 6),
                                           valueBoxOutput("conteo_ues3",width = 6),
                                           valueBoxOutput("conteo_ues4",width = 6),
                                           valueBoxOutput("conteo_ues5",width = 6)
                                           # plotlyOutput("consumo_valor_ues", height = 500)
                                    ),
                                    column(1),
                                    #        # plotlyOutput("consumo_transa_servicio", height = 500)
                                    #        ),
                                    br()
                                  ))
                            ))
                  )
                )
              )
)

