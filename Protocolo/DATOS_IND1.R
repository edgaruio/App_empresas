# PREPARAR DATOS
# Nota: Version 11032019
# Se lee info_consulta y consumos para constiruir consumos
rm(list = ls())
library(data.table); library(dplyr); library(readxl); library(tidyr); library(stringr)
dir("Datos/")

# Afiliados con empresa
afil_seg_ind <- readRDS("Datos/Originales_Mayo_2020/bd_afiliados1_23062020.rds") %>% 
  mutate(Cupo_credito = ifelse(Cupo_credito == "ACTIVO", 1, 0),
         Habeas_data = ifelse(is.na(Habeas_data), 0, Habeas_data)) 
sort(names(afil_seg_ind))

consumos <- afil_seg_ind %>% 
  # select(id_empresa, Salario, Edad, filial_famisanar:filial_proteccion, Bono_derecho:pros_pisi, Habeas_data) %>% 
  group_by(id_empresa) %>% 
  summarise(
    pro_salario = mean(Salario, na.rm = T),
    pro_edad = round(mean(Edad, na.rm = T)),
    marca_aventurero = sum(marca_aventurero, na.rm = T),
    Bono_derecho = sum(Bono_derecho, na.rm = T),
    Bono_redimido = sum(Bono_redimido, na.rm = T),
    kit_redimido = sum(kit_redimido, na.rm = T),
    kit_derecho = sum(kit_derecho, na.rm = T),
    n_kit_derecho = sum(n_kit_derecho, na.rm = T),
    cuota_derecho = sum(cuota_derecho, na.rm = T),
    cuota_redimida = sum(cuota_redimida, na.rm = T),
    n_beneficario_cm = sum(numero_beneficiario_cuota_monetaria, na.rm = T),
    pre_aprobado_cupo = sum(preaprobado_cupo, na.rm = T),
    pre_aprobado_hipo = sum(preaprobado_hipo, na.rm = T),
    Piscilago = sum(Piscilago, na.rm = T),
    Club = sum(Club, na.rm = T),
    Hotel = sum(Hotel, na.rm = T),
    RyT = sum(RyT, na.rm = T),
    Cupo_credito = sum(Cupo_credito, na.rm = T),
    Consumo_credito = sum(Consumo_credito, na.rm = T),
    Educacion = sum(Educacion, na.rm = T),
    Medicamentos = sum(Medicamentos, na.rm = T),
    Supermercados = sum(Supermercados, na.rm = T),
    Salud = sum(Salud, na.rm = T),
    Compra_vivienda = sum(Compra_vivienda, na.rm = T),
    Subsidio_asignado  = sum(Subsidio_asignado, na.rm = T),
    subsidio_entregado = sum(subsidio_entregado, na.rm = T),
    uso_mes = sum(uso_mes, na.rm = T),
    pros_hotel = sum(pros_hotel, na.rm = T),
    pros_club = sum(pros_club, na.rm = T),
    pros_pisi = sum(pros_pisi, na.rm = T),
    Habeas_data = sum(Habeas_data, na.rm = T)
    )
str(consumos)

cuadrantes <-afil_seg_ind %>% 
  select(id_empresa,CuadranteViv) %>% 
  na.omit() %>% 
  group_by(id_empresa,CuadranteViv) %>% 
  summarise(conteo = n()) %>% 
  spread(CuadranteViv, conteo) %>% 
  data.frame()
str(cuadrantes)  

info_consolidada <- readRDS("Datos/Originales_Mayo_2020/ConsolidacionMAY2020.rds") %>% 
  select(id_empresa:Num_cesantias) %>% 
  distinct() %>% 
  mutate(Gen_F = Gen_f + Gen_F) %>% 
  select(-Gen_f) %>% 
  left_join(consumos, by = "id_empresa") %>% 
  left_join(cuadrantes, by = "id_empresa")
str(info_consolidada)
names(info_consolidada)<- chartr("áéíóú","aeiou",tolower(names(info_consolidada)))
table(sort(names(info_consolidada)))

bd_empresas_app <- info_consolidada %>%
  dplyr::select(id_empresa, seg_basico, seg_medio, seg_joven, seg_alto, numempleados) %>% 
  mutate(P_basico=seg_basico/numempleados,
         P_medio=seg_medio/numempleados,
         P_joven=seg_joven/numempleados,
         P_alto=seg_alto/numempleados) %>%
  mutate(cluster = ifelse(P_basico >= .65, "1. Basico" ,
                          ifelse(P_medio >= .65, "2. Medio" ,
                                 ifelse(P_joven >= .65,  "3. Joven",
                                        ifelse(P_alto >= .5, "4. Alto",
                                               ifelse((P_basico + P_medio + P_joven >= .65) & (P_basico > .2 & P_medio > .2 & P_joven > .2) , "5. Basico  - Medio - Joven",
                                                      ifelse((P_basico + P_medio + P_alto >= .65) & (P_basico > .2 & P_medio > .2 & P_alto > .2) , "6. Basico - Medio - Alto",
                                                             ifelse((P_medio + P_joven + P_alto >= .65) & (P_medio > .2 & P_joven > .2 & P_alto > .2) , "7. Medio - Joven - Alto",
                                                                    ifelse((P_basico + P_medio > .65) & (P_basico > .25 & P_medio > .25) , "8. Basico - Medio",
                                                                           ifelse((P_basico + P_joven > .65) & (P_basico > .2 & P_joven > .2) , "9. Basico - Joven",
                                                                                  ifelse((P_basico + P_alto > .65) & (P_basico > .25 & P_alto > .25) , "10. Basico - Alto",
                                                                                         ifelse((P_medio + P_joven > .65) & (P_medio > .25 & P_joven > .25) , "11. Medio - Joven",
                                                                                                ifelse((P_medio + P_alto > .65) & (P_medio > .25 & P_alto > .25) , "11. Medio - Alto",
                                                                                                       ifelse((P_joven + P_alto > .65) & (P_joven > .25 & P_alto > .25) , "13. Joven - Alto",NA
                                                                                                       )))))))))))))) %>%
  data.frame() %>% 
  select(-c(seg_basico, seg_medio, seg_joven, seg_alto, numempleados))
str(bd_empresas_app)
sum(is.na(bd_empresas_app$cluster))

bd_empresas_app1 <- bd_empresas_app %>%
  filter(is.na(cluster)) %>%
  dplyr::mutate(cluster = ifelse(P_basico+P_medio+P_joven+P_alto < 0.1, NA,
                                 ifelse(is.na(cluster),colnames(bd_empresas_app[2:5])[apply(bd_empresas_app[2:5],1,which.max)],cluster))) %>%
  dplyr::mutate(cluster = ifelse(cluster=="P_basico", "1. Basico",
                                 ifelse(cluster=="P_joven", "3. Joven",
                                        ifelse(cluster=="P_medio", "2. Medio",
                                               ifelse(cluster=="P_alto", "4. Alto",cluster)))))
table(bd_empresas_app1$cluster)

info_cluster <- bd_empresas_app %>% 
  mutate(cluster = ifelse(id_empresa %in% bd_empresas_app1$id_empresa, bd_empresas_app1$cluster, cluster))
sum(is.na(info_cluster$cluster))
names(info_cluster)

info_actualizada <- info_consolidada %>% 
  left_join(info_cluster %>% select(id_empresa, cluster), by = "id_empresa")
str(info_actualizada)

# SALIDA
saveRDS(info_actualizada, file = "App/Data/info_actualizada1_23062020.rds")
saveRDS(info_actualizada, file = "Datos/Originales_Abril_2020/info_actualizada1_23062020.rds")
 
# Informacion adicional
info_piramide <- readRDS("Datos/Originales_Mayo_2020/ConsolidacionMAY2020.rds") %>% 
  select(id_persona:Segmento_poblacional,id_empresa,Piramide1,Piramide2,ActividadCIIU,-Nombre) %>% 
  mutate(Genero = toupper(Genero))
str(info_piramide)
saveRDS(info_piramide, file = "App/Data/consulta_piramide_plot.rds")
