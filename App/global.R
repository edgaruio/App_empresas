# Cargamos librerias
# options(scipen = 999)
rm(list = ls())
library(shiny); library(ggplot2); library(dplyr); library(plotly)
library(shinydashboard);library(DT); library(shinyjs); library(reshape)
library(data.table); library(tidyr); library(stringr); library(shinydashboardPlus)
library(Hmisc); library(shinythemes); library(dashboardthemes)
library(scales); library(shinycustomloader)
library(shinyBS)

# library(devtools)
# install_github("shinyGlobe", "trestletech")
# library(devtools)
# install_github("nik01010/dashboardthemes")
# 11188821
# Cargamos bases de datos

# Empresas en 8 municipios

bd_empresas12 <- readRDS("Data/info_actualizada12_23062020.rds") %>% 
  data.frame() %>% 
  mutate(afiliados = numempleados,
         tipo_nit = ifelse(id_empresa == id_empresa_principal, "principal", "secundaria"),
         agrupado = "12 meses")
str(bd_empresas12)

bd_empresas1 <- readRDS("Data/info_actualizada1_23062020.rds") %>% 
  data.frame() %>% 
  mutate(afiliados = numempleados,
         tipo_nit = ifelse(id_empresa == id_empresa_principal, "principal", "secundaria"),
         agrupado = "ultimo mes")
str(bd_empresas1)

bd_empresas <- bind_rows(bd_empresas12, bd_empresas1) %>% 
  data.frame()
str(bd_empresas)

# name_tipo_nit <- c("Principal","Secundaria")
name_piramide1 <- c("Total","1 Emp Grandes","2 Emp Medio","3 Empresas Pymes","4 Micro")
name_piramide2 <- c("Total","1.1 Platinum","1.2 Premium","2.1 Gold","2.2 Silver","3.1 VIP","3.2 VIP Estándar",
                    "4.1 Estándar","4.2 Trans. Mas de 100 Trab.","4.3 Trans.Juridica Ent. 11 a 99 Trab.",
                    "4.4 Trans.Natural Ent. 11 a 99 Trab.","4.5 Transaccional")
name_actividad <- c("Total","Actividades de entretenimiento y recreativas","Actividades floricultura","Actividades inmobiliarias",                    
                    "Agricultura, caza, silvicultura y pesca","Comercio al por mayor y al por menor","Construcción, demoliciones, terrenos, vías",   
                    "Elaboración de productos alimenticios","Electricidad, gas, agua, explotación de minas","Fabricación de productos textiles",            
                    "Hidrocarburos","Industria","Prestación de servicio de comunicaciones","Prestación de servicio de transporte","Prestación de servicios",
                    "Prestación de servicios de limpieza","Prestación de servicios de salud","Prestación de servicios educativos",
                    "Prestación de servicios financieros","Sector público","Sin Información","Temporales","Transporte y almacenamiento") 

# Ajuste piramide_poblacional
consulta_piramide <- readRDS("Data/consulta_piramide_plot.rds") %>% 
  data.frame()
str(consulta_piramide)
table(consulta_piramide$Genero)

# Ajuste Consumo empresarial
consumo_emp <- readRDS("Data/consumo_empresarial_Mayo2020.rds") %>% 
  mutate(anio_mes = paste(anno, mes, sep = "_")) %>% 
  dplyr::filter(anio_mes %in% c("2019_6", "2019_7", "2019_8", "2019_9", "2019_10", 
                                "2019_11", "2019_12", "2020_1", "2020_2", "2020_3", "2020_4", "2020_5")) %>% 
  inner_join(bd_empresas12 %>% select(id_empresa,piramide1,piramide2,actividadciiu), 
             by = "id_empresa") %>% 
  data.frame()
str(consumo_emp)
table(consumo_emp$ues)

# ## Para cuadros con color
# output$conteo_salud_glob <- renderValueBox({
#   data_f1<-data_f1() 
#   # %>%
#   #   filter(tipo_nit == "principal")
#   # valor <- as.numeric(100*sum(data_f1$Salud, na.rm = T)/sum(data_f1$Afiliados, na.rm = T))
#   valueBox(
#     value = paste(formatC(100*sum(data_f1$salud, na.rm = T)/sum(data_f1$afiliados, na.rm = T),
#                           digits = 1, format = "f", big.mark=","),"%", " (",comma(sum(data_f1$Salud, na.rm = T)),")",sep = ""),
#     subtitle = "Salud (Total)",
#     icon = icon("briefcase"),
#     # color = ifelse(valor > 70, "green", "orange")
#     color = "purple"
#   )
# })


# test <- bd_empresas %>% 
#   select(id_empresa, numempleados, bono_derecho, bono_redimido, kit_derecho, kit_redimido, cuota_derecho, cuota_redimida) %>% 
#   mutate(test = ifelse(numempleados < bono_redimido, 1, 0),
#          test2 = ifelse(numempleados < kit_redimido, 1, 0),
#          test3 = ifelse(numempleados < cuota_redimida, 1, 0)) %>%
#   filter(test == 1 | test2 == 1 | test3 == 1)

# Base entrega don adolfo
sort(names(bd_empresas12))
consulta_empresa <- bd_empresas12 %>% 
  select(id_empresa, prospecto_club = pros_club, prospecto_hotel = pros_hotel, prospecto_piscilago = pros_pisi,
         pre_aprobado_cupo, pre_aprobado_hipo,
         Cuadrante_A1 = a1, Cuadrante_A2 = a2, Cuadrante_A = a3, Cuadrante_B = b1) 
str(consulta_empresa)

library(writexl)
write_xlsx(consulta_empresa, "Datos/Originales_Abril_2020/consulta_empresas_abril.xlsx")
