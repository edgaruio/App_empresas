# PREPARAR DATOS
# Nota: Version 11032019
# Se lee info_consulta y consumos para constiruir consumos
rm(list = ls())
library(data.table); library(dplyr); library(readxl); library(tm); library(tidyr); library(stringr); library(tm)
dir("Datos/")

# Afiliados con empresa
afil_seg_ind <- readRDS("Datos/Originales_diciembre/bd_afiliados_28012020.rds")
str(afil_seg_ind)

# Consumos para RyT
source("Datos/Funciones.R")

consumo_individual <- readRDS("Datos/Originales_diciembre/consumo_individual_Diciembre.rds") %>% 
  data.frame() %>% 
  mutate(segmento_poblacional = iconv(segmento_poblacional,to="ASCII//TRANSLIT"),
         servicio = iconv(servicio,to="ASCII//TRANSLIT"),
         anio_mes = paste(anno,mes,sep="")) %>% 
  filter(anno >= 2019)
str(consumo_individual)
table(consumo_individual$ues)
table(consumo_individual$servicio)
table(consumo_individual$anio_mes)

# consumo_individual_ryt$segmento_poblacional=iconv(consumo_individual_ryt$segmento_poblacional,to="ASCII//TRANSLIT")
consumo_individual_ryt <- consumo_individual %>%
  filter(ues == "RyT")
str(consumo_individual_ryt)
table(consumo_individual$servicio)

# CC1019078197
consumo_ryt_emp1 <- consumo_individual_ryt %>% 
  select(id_empresa,id_persona,servicio) %>% 
  group_by(id_empresa, servicio) %>% 
  summarise(n_id_persona = n_distinct(id_persona)) %>% 
  spread(key = servicio, value = n_id_persona) %>% 
  summarise_all(funs(max(., na.rm = TRUE))) %>% 
  mutate_all(funs(ifelse(is.infinite(.),0,.))) %>%
  mutate(Club = `Club Bellavista` + `Club Calle 195` + `Club El Cubo` + `Club La Colina`,
         Hotel = `Hotel Alcaravan` + `Hotel Bosques De Athan` + `Hotel Penalisa` + `Hoteles Paipa`) %>% 
  select(c(id_empresa,Piscilago,Club,Hotel))
str(consumo_ryt_emp1)

consumo_ryt_emp2 <- consumo_individual_ryt %>% 
  select(id_empresa,id_persona,ues) %>% 
  group_by(id_empresa, ues) %>% 
  summarise(RyT = n_distinct(id_persona)) %>% 
  select(-ues)
  
consumo_ryt_emp <- left_join(consumo_ryt_emp1,consumo_ryt_emp2)
str(consumo_ryt_emp)
rm(consumo_ryt_emp1,consumo_ryt_emp2)

# Penetracion Credito
estados_tarjeta <-fread("Datos/Cobranzas/EstadosTarjeta.csv") %>% 
  data.frame()
str(estados_tarjeta)

cupo_credito <- fread("Datos/Cobranzas/cobranzas_diciembre_2019.csv", encoding = 'UTF-8') %>% 
  data.frame() %>% 
  select(1,2,31,Desc.Bloqueo) %>% 
  filter(Tipo.Identificación %in% c(2,3,4)) %>% 
  mutate(tipo_id = ifelse(Tipo.Identificación == 2, "CC",
                          ifelse(Tipo.Identificación == 3, "CE", "TI"))) %>% 
  mutate(id_persona = paste0(tipo_id,Nro.Identificación)) %>% 
  left_join(estados_tarjeta, by = c("Estado.Tarjeta"="estadotarjeta")) %>% 
  select(c(id_persona,ESTADOCUPO)) %>% 
  group_by(id_persona) %>% 
  filter(row_number()==1) %>% 
  data.frame() %>% 
  filter(ESTADOCUPO == "ACTIVO") %>% 
  inner_join(afil_seg_ind, select(id_persona,id_empresa),by = "id_persona") %>% 
  group_by(id_empresa) %>% 
  summarise(cupo_credito = n())
str(cupo_credito)
sum(cupo_credito$cupo_credito)

consumo_credito <- consumo_individual %>% 
  filter(servicio %in% c("Libranza","No Libranza")) %>% 
  select(id_empresa,id_persona,ues) %>% 
  group_by(id_empresa) %>% 
  summarise(Consumo_credito = 1)
str(consumo_credito)

educacion <- consumo_individual %>% 
  filter(ues == "Educacion") %>% 
  select(id_empresa,id_persona) %>% 
  group_by(id_empresa) %>% 
  summarise(Educacion = n_distinct(id_persona))
str(educacion)

medicamentos <- consumo_individual %>% 
  filter(servicio == "Medicamentos") %>% 
  select(id_empresa,id_persona,ues) %>% 
  group_by(id_empresa) %>% 
  summarise(Medicamentos = n_distinct(id_persona))
str(medicamentos)

supermercados <- consumo_individual %>% 
  filter(servicio == "Supermercados") %>% 
  select(id_empresa,id_persona,ues) %>% 
  group_by(id_empresa) %>% 
  summarise(Supermercados = n_distinct(id_persona))
str(supermercados)

salud <- consumo_individual %>% 
  filter(ues == "Salud") %>% 
  select(id_empresa,id_persona,ues) %>% 
  group_by(id_empresa) %>% 
  summarise(Salud = n_distinct(id_persona))
str(salud)

compra_vivienda <- consumo_individual %>% 
  filter(servicio == "Fecha Entrega") %>% 
  select(id_empresa,id_persona,ues) %>% 
  group_by(id_empresa) %>% 
  summarise(Compra_vivienda = n_distinct(id_persona))
str(compra_vivienda)

subsidio_asignado <- consumo_individual %>% 
  filter(servicio == "Subsidio Asignado") %>% 
  select(id_empresa,id_persona,ues) %>% 
  group_by(id_empresa) %>% 
  summarise(Subsidio_asignado = n_distinct(id_persona))
str(subsidio_asignado)

subsidio_entregado <- consumo_individual %>% 
  filter(servicio == "Subsidio Entregado") %>% 
  select(id_empresa,id_persona,ues) %>% 
  group_by(id_empresa) %>% 
  summarise(subsidio_entregado = n_distinct(id_persona))
str(subsidio_entregado)
  
uso_tms <- consumo_individual %>% 
  filter(servicio == "Credito convenios") %>% 
  select(id_empresa,id_persona,ues) %>% 
  group_by(id_empresa) %>% 
  summarise(uso_mes = n_distinct(id_persona))
str(subsidio_entregado)
  
bd_empresas <- readRDS("Datos/Originales_diciembre/bd_empresas28012020_p10.rds") %>% 
  select(id_empresa,n_empleados,Alto:n_redencion_cm,n0_famisanar,n0_ips_colsubsidio,n0_pac_famisanar,n0_suramericana,n0_pac_sura)
names(bd_empresas) <- chartr("áéíóú","aeiou",names(bd_empresas))
str(bd_empresas)

# info_empresas <- fread("Datos/Originales_octubre/Consulta_Seg_Emp.txt", sep = ";", dec = ",") %>%
#   select(id_persona,id_empresa,razon_social,id_empresa_principal,nombre_empresa_principal,piramide_1,piramide_2,
#          Salario,ID_CIIU,id_sectorComercial_22_actividad,Edad) %>%
#   group_by(id_empresa,razon_social,id_empresa_principal,nombre_empresa_principal,piramide_1,piramide_2,ID_CIIU,id_sectorComercial_22_actividad) %>%
#   summarise(Promedio.Salario = mean(Salario, na.rm = T),
#             Promedio.Edad = mean(Edad, na.rm = T)) %>%
#   data.frame()
# saveRDS(info_empresas, file = "Datos/Originales_octubre/info_empresas.rds")
info_empresas <- readRDS("Datos/Originales_noviembre/info_empresa.rds")
str(info_empresas)

pre_aprobados <- afil_seg_ind %>% 
  select(id_empresa,id_persona,pre_aprobado_hipo,pre_aprobado_cupo) %>% 
  distinct() %>% 
  group_by(id_empresa) %>% 
  summarise(pre_aprobado_hipotecario = sum(pre_aprobado_hipo, na.rm = T),
            pre_aprobado_cupo = sum(pre_aprobado_cupo, na.rm = T)) %>% 
  data.frame()
str(pre_aprobados)

# Info Prospectos
str(afil_seg_ind)
info_prospectos_ryt <- afil_seg_ind %>% 
  select(id_empresa,pros_club,pros_hotel,pros_pisi) %>%
  group_by(id_empresa) %>% 
  summarise(pros_club = sum(pros_club,na.rm = T),
            pros_hotel = sum(pros_hotel,na.rm = T),
            pros_pisi = sum(pros_pisi,na.rm = T)) %>% 
  data.frame() %>% 
  dplyr::rename(ProspHot_n = pros_hotel,
         ProspClu_n = pros_club,
         ProspPis_n = pros_pisi)
str(info_prospectos_ryt)

info_prospectos_viv <- afil_seg_ind %>% 
  select(id_empresa,CuadranteViv) %>% 
  na.omit() %>% 
  group_by(id_empresa,CuadranteViv) %>% 
  summarise(conteo_cuadrante = n()) %>% 
  spread(CuadranteViv,value = conteo_cuadrante, fill = 0) %>% 
  data.frame() %>% 
  dplyr::rename(Cuadrante_A = A,
         Cuadrante_A1 = A1,
         Cuadrante_A2 = A2,
         Cuadrante_B = B,
         Cuadrante_C = C,
         Cuadrante_D = D,
         Cuadrante_E = E)
str(info_prospectos_viv)

info_prospectos <- full_join(info_prospectos_ryt, info_prospectos_viv, by = "id_empresa")
str(info_prospectos)

str(info_empresas)
info_act <- info_empresas %>% 
  left_join(bd_empresas, by = "id_empresa") %>% 
  left_join(consumo_ryt_emp, by = "id_empresa") %>% 
  left_join(cupo_credito, by = "id_empresa") %>% 
  left_join(consumo_credito, by = "id_empresa") %>% 
  left_join(educacion, by = "id_empresa") %>% 
  left_join(medicamentos, by = "id_empresa") %>% 
  left_join(supermercados, by = "id_empresa") %>% 
  left_join(salud, by = "id_empresa") %>% 
  left_join(compra_vivienda, by = "id_empresa") %>% 
  left_join(subsidio_asignado, by = "id_empresa") %>% 
  left_join(subsidio_entregado, by = "id_empresa") %>% 
  left_join(uso_tms, by = "id_empresa") %>% 
  left_join(pre_aprobados, by = "id_empresa") %>% 
  left_join(info_prospectos, by = "id_empresa") %>%  
  data.frame()
str(info_act)
info_act[,c(11:59)][is.na(info_act[,c(11:59)])] <- 0


info_act <- info_act %>% 
  na.omit() %>% 
  mutate(tipo_nit = ifelse(id_empresa == id_empresa_principal, "principal", "secundario"),
         ACTIVIDAD = ifelse(is.na(ActividadCIIU),"Sin Información", ActividadCIIU))
str(info_act)

# saveRDS(bd_empresas, file = "Data/Originales_Abril/bd_empresas_test.rds")

principales <- info_act %>% 
  group_by(id_empresa_principal,nombre_empresa_principal,Piramide1,Piramide2) %>% 
  summarise(n_empleados = sum(n_empleados),
            Alto = sum(Alto),
            Basico = sum(Basico),
            Joven = sum(Joven),
            Medio = sum(Medio),
            A = sum(A),
            B = sum(B),
            C = sum(C),
            Promedio.de.Salario = mean(Promedio.de.Salario),
            Promedio.de.edad = mean(Promedio.de.edad),
            n_habeas = sum(n_habeas),
            kit_derecho = sum(kit_derecho),
            kit_redimido = sum(kit_redimido),
            n_derecho_cm = sum(n_derecho_cm),
            n_redencion_cm = sum(n_redencion_cm),
            n0_famisanar = sum(n0_famisanar),
            n0_ips_colsubsidio = sum(n0_ips_colsubsidio),
            n_cafam = sum(n_cafam),
            n_otraips = sum(n_otraips),
            n0_pac_famisanar = sum(n0_pac_famisanar),
            n0_suramericana = sum(n0_suramericana),
            n0_pac_sura = sum(n0_pac_sura),
            n_proteccion = sum(n_proteccion),
            cupo_credito = sum(cupo_credito),
            Consumo_credito = sum(Consumo_credito),
            uso_mes = sum(uso_mes),
            Educacion = sum(Educacion),
            Medicamentos = sum(Medicamentos),
            Supermercados = sum(Supermercados),
            Salud = sum(Salud),
            Club = sum(Club),
            Hotel = sum(Hotel),
            Piscilago = sum(Piscilago),
            RyT = sum(RyT),
            Compra_vivienda = sum(Compra_vivienda),
            Subsidio_asignado = sum(Subsidio_asignado),
            subsidio_entregado = sum(subsidio_entregado),
            n_bono_derecho = sum(n_bono_derecho),
            n_bono_redimido = sum(n_bono_redimido),
            pre_aprobado_hipotecario = sum(pre_aprobado_hipotecario),
            pre_aprobado_cupo = sum(pre_aprobado_cupo),
            ProspHot_n = sum(ProspHot_n),
            ProspClu_n = sum(ProspClu_n),
            ProspPis_n = sum(ProspPis_n),
            Cuadrante_A = sum(Cuadrante_A),
            Cuadrante_A1 = sum(Cuadrante_A1),
            Cuadrante_A2 = sum(Cuadrante_A2),
            Cuadrante_B= sum(Cuadrante_B)) %>% 
  data.frame() %>% 
  dplyr::rename(id_empresa = id_empresa_principal,
                nombre = nombre_empresa_principal) %>% 
  mutate(tipo_nit = "principal") %>% 
  left_join(info_act %>% 
              select(id_empresa,ACTIVIDAD),
            by = c("id_empresa"="id_empresa"))
str(principales)

secundarias <- info_act %>% 
  filter(tipo_nit %in% c("secundario")) %>% 
  select(id_empresa,RazonSocial,Piramide1,Piramide2,Promedio.de.Salario:Cuadrante_B) %>% 
  dplyr::rename(nombre = RazonSocial) %>%
  mutate(tipo_nit = "secundaria") %>% 
  left_join(info_act %>% 
              select(id_empresa,ACTIVIDAD),
            by = c("id_empresa"="id_empresa")) %>%
  select(names(principales))
str(secundarias)

bd_empresas_app <- rbind(principales, secundarias)
str(bd_empresas_app)

bd_empresas_app <- bd_empresas_app %>% 
  mutate(P_basico=Basico/n_empleados,
         P_medio=Medio/n_empleados,
         P_joven=Joven/n_empleados,
         P_alto=Alto/n_empleados) %>% 
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
  data.frame() 
str(bd_empresas_app)
sum(is.na(bd_empresas_app$cluster))

bd_empresas_app1 <- bd_empresas_app %>%
  filter(!is.na(cluster)) %>% 
  dplyr::mutate(cluster = ifelse(P_basico+P_medio+P_joven+P_alto < 0.1, NA,
                          ifelse(is.na(cluster),colnames(bd_empresas[55:58])[apply(bd_empresas[55:58],1,which.max)],cluster))) %>%
  dplyr::mutate(cluster = ifelse(cluster=="P_basico", "1. Basico",
                          ifelse(cluster=="P_joven", "3. Joven",
                                 ifelse(cluster=="P_medio", "2. Medio",
                                        ifelse(cluster=="P_alto", "4. Alto",cluster)))))
         

bd_empresas_app <- rbind(bd_empresas_app %>% filter(is.na(cluster)),
                         bd_empresas_app1) %>% 
  left_join(info_act %>% select(id_empresa,ActividadCIIU), by = "id_empresa") %>% 
  data.frame()
str(bd_empresas_app)

write.csv2(bd_empresas_app, file = "App/Data/Info_emp_act_28012020.csv", row.names = FALSE)
saveRDS(bd_empresas_app, file = "App/Data/Info_emp_act_28012020.rds")

# # Ajuste Consumo empresarial
consumo_emp <- readRDS("Datos/Originales_diciembre//consumo_empresarial_Diciembre.rds") %>% 
  mutate(Fecha = paste(anno,mes,01,sep="/"),
         Fecha = as.Date.character(Fecha,format = "%Y/%m/%d"))
str(consumo_emp)
saveRDS(consumo_emp, file = "App/Data/consumo_empresarial_diciembre_edit.rds")

## Informacion adicional
consulta_piramide <- readRDS("Datos/Originales_diciembre/ConsolidacionDIC2019.rds") %>% 
  select(id_empresa,id_persona,Piramide1,Piramide2,Segmento_poblacional,Categoria,Edad,Genero,ActividadCIIU)
str(consulta_piramide)

write.csv2(consulta_piramide, file = "App/Data/consulta_piramide_plot.csv", row.names = FALSE)
saveRDS(consulta_piramide, file = "App/Data/consulta_piramide_plot.rds")






# #### Consulta PAC=====================
# rm(list = ls())
# library(dplyr); library(data.table); library(tidyr)
# 
# bd_personas <- fread("Consulta_PAC.txt", sep = ";", dec = ",") %>% 
#   data.frame()
# names_pir2 <- names(table(bd_personas$piramide_2))[c(1:11,15)]
# 
# bd_personas <- bd_personas %>% 
#   filter(piramide_2 %in% names_pir2) %>% 
#   mutate(cm = ifelse(numero_cuota_monetaria >= 1, 1, 0))
# str(bd_personas)  
# 
# tb_cat <- bd_personas %>% 
#   filter(marca_afiliado_unico == "X") %>% 
#   group_by(piramide_2,categoria) %>% 
#   summarise(Conteo = n_distinct(id_persona)) %>% 
#   spread(key = "categoria", value = "Conteo") %>% 
#   select(-C) %>% 
#   data.frame()
# str(tb_cat)
#   
# tb_cm_A <- bd_personas %>% 
#   filter(marca_afiliado_unico == "X" & categoria == "A" & cm == 1) %>% 
#   group_by(piramide_2) %>% 
#   summarise(A_CM = sum(cm, na.rm = T))
# str(tb_cm_A)
# 
# tb_cm_B <- bd_personas %>% 
#   filter(marca_afiliado_unico == "X" & categoria == "B" & cm == 1) %>% 
#   group_by(piramide_2) %>% 
#   summarise(B_CM = sum(cm, na.rm = T))
# str(tb_cm_B)
# 
# tb_fam_A <- bd_personas %>% 
#   filter(marca_afiliado_unico == "X" & categoria == "A" & filial_famisanar == 1) %>% 
#   group_by(piramide_2) %>% 
#   summarise(famisanar = n_distinct(id_persona)) %>% 
#   dplyr::rename("A_Famisanar"="famisanar")
# str(tb_fam_A)
# 
# tb_fam_B <- bd_personas %>% 
#   filter(marca_afiliado_unico == "X" & categoria == "B" & filial_famisanar == 1) %>% 
#   group_by(piramide_2) %>% 
#   summarise(famisanar = n_distinct(id_persona)) %>% 
#   dplyr::rename("B_Famisanar"="famisanar")
# str(tb_fam_B)
# 
# tb_cm_fam <- bd_personas %>% 
#   filter(marca_afiliado_unico == "X" & categoria %in% c("A","B") & filial_famisanar == 1, cm == 1) %>% 
#   group_by(piramide_2,categoria) %>% 
#   summarise(cm_fam = n_distinct(id_persona)) %>% 
#   spread(categoria,cm_fam) %>% 
#   dplyr::rename("CM_A_FAM"="A","CM_B_FAM"="B")
# str(tb_cm_fam)
# 
# tb_nofam <- bd_personas %>% 
#   filter(marca_afiliado_unico == "X" & categoria %in% c("A","B") & filial_famisanar == 0) %>%
#   group_by(piramide_2,categoria) %>% 
#   summarise(nofam = n_distinct(id_persona)) %>% 
#   spread(categoria,nofam) %>% 
#   dplyr::rename("NoFam_A"="A","Nofam_B"="B")
# str(tb_nofam)
# 
# tb_cm_nofam <- bd_personas %>% 
#   filter(marca_afiliado_unico == "X" & categoria %in% c("A","B") & filial_famisanar == 0 & cm == 1) %>%
#   group_by(piramide_2,categoria) %>% 
#   summarise(nofam = n_distinct(id_persona)) %>% 
#   spread(categoria,nofam) %>% 
#   dplyr::rename("CM_NoFam_A"="A","CM_Nofam_B"="B")
# str(tb_nofam)
# 
# tb_fam_ipscol <- bd_personas %>% 
#   filter(marca_afiliado_unico == "X" & categoria %in% c("A","B") & filial_famisanar == 1 & filial_ips_colsubsidio == 1) %>% 
#   group_by(piramide_2,categoria) %>% 
#   summarise(fam_ipscol = n_distinct(id_persona)) %>% 
#   spread(categoria,fam_ipscol) %>% 
#   dplyr::rename("Fam_ipscol_A"="A","Fam_ipscol_B"="B")
# str(tb_fam_ipscol)
# 
# tb_cm_fam_ipscol <- bd_personas %>% 
#   filter(marca_afiliado_unico == "X" & categoria %in% c("A","B") & filial_famisanar == 1 & filial_ips_colsubsidio == 1 & cm == 1) %>% 
#   group_by(piramide_2,categoria) %>% 
#   summarise(fam_ipscol = n_distinct(id_persona)) %>% 
#   spread(categoria,fam_ipscol) %>% 
#   dplyr::rename("CM_Fam_ipscol_A"="A","CM_Fam_ipscol_B"="B")
# str(tb_cm_fam_ipscol)
# 
# tb_fam_ipscafam <- bd_personas %>% 
#   filter(marca_afiliado_unico == "X" & categoria %in% c("A","B") & filial_famisanar == 1 & filial_ips_cafam == 1) %>% 
#   group_by(piramide_2,categoria) %>% 
#   summarise(fam_ipscafam = n_distinct(id_persona)) %>% 
#   spread(categoria,fam_ipscafam) %>% 
#   dplyr::rename("Fam_ipscafam_A"="A","Fam_ipscafam_B"="B")
# str(tb_cm_fam_ipscol)
# 
# tb_cm_fam_ipscafam <- bd_personas %>% 
#   filter(marca_afiliado_unico == "X" & categoria %in% c("A","B") & filial_famisanar == 1 & filial_ips_cafam == 1 & cm == 1) %>% 
#   group_by(piramide_2,categoria) %>% 
#   summarise(cm_fam_ipscafam = n_distinct(id_persona)) %>% 
#   spread(categoria,cm_fam_ipscafam) %>% 
#   dplyr::rename("CM_Fam_ipscafam_A"="A","CM_Fam_ipscafam_B"="B")
# str(tb_cm_fam_ipscafam)
# 
# tb_fam_ipscol_pacfam <- bd_personas %>% 
#   filter(marca_afiliado_unico == "X" & categoria %in% c("A","B") & filial_famisanar == 1 & filial_ips_colsubsidio == 1 & filial_pac_famisanar == 1) %>% 
#   group_by(piramide_2,categoria) %>% 
#   summarise(fam_ipscol_pacfam = n_distinct(id_persona)) %>% 
#   spread(categoria,fam_ipscol_pacfam) %>% 
#   dplyr::rename("Fam_ipscol_pacfam_A"="A","Fam_ipscol_pacfam_B"="B")
# str(tb_fam_ipscol_pacfam)
# 
# tb_cm_fam_ipscol_pacfam <- bd_personas %>% 
#   filter(marca_afiliado_unico == "X" & categoria %in% c("A","B") & filial_famisanar == 1 & filial_ips_colsubsidio == 1 & filial_pac_famisanar == 1 & cm == 1) %>% 
#   group_by(piramide_2,categoria) %>% 
#   summarise(cm_fam_ipscol_pacfam = n_distinct(id_persona)) %>% 
#   spread(categoria,cm_fam_ipscol_pacfam) %>% 
#   dplyr::rename("CM_fam_ipscol_pacfam_A"="A","CM_fam_ipscol_pacfam_B"="B")
# str(tb_fam_ipscol_pacfam)
# 
# tb_fam_ipscol_nopacfam <- bd_personas %>% 
#   filter(marca_afiliado_unico == "X" & categoria %in% c("A","B") & filial_famisanar == 1 & filial_ips_colsubsidio == 1 & filial_pac_famisanar == 0) %>%
#   group_by(piramide_2,categoria) %>% 
#   summarise(fam_ipscol_nopacfam = n_distinct(id_persona)) %>% 
#   spread(categoria,fam_ipscol_nopacfam) %>% 
#   dplyr::rename("Fam_ipscol_nopacfam_A"="A","Fam_ipscol_nopacfam_B"="B")
# str(tb_fam_ipscol_nopacfam)
# 
# tb_cm_fam_ipscol_nopacfam <- bd_personas %>% 
#   filter(marca_afiliado_unico == "X" & categoria %in% c("A","B") & filial_famisanar == 1 & filial_ips_colsubsidio == 1 & filial_pac_famisanar == 0 & cm == 1) %>%
#   group_by(piramide_2,categoria) %>% 
#   summarise(cm_fam_ipscol_nopacfam = n_distinct(id_persona)) %>% 
#   spread(categoria,cm_fam_ipscol_nopacfam) %>% 
#   dplyr::rename("CM_Fam_ipscol_nopacfam_A"="A","CM_Fam_ipscol_nopacfam_B"="B")
# str(tb_cm_fam_ipscol_nopacfam)
# 
# tb_fam_ipscafam_pacfam <- bd_personas %>% 
#   filter(marca_afiliado_unico == "X" & categoria %in% c("A","B") & filial_famisanar == 1 & filial_ips_cafam == 1 & filial_pac_famisanar == 1) %>%
#   group_by(piramide_2,categoria) %>% 
#   summarise(fam_ipscafam_pacfam = n_distinct(id_persona)) %>% 
#   spread(categoria,fam_ipscafam_pacfam) %>% 
#   dplyr::rename("fam_ipscafam_pacfam_A"="A","fam_ipscafam_pacfam_B"="B")
# str(tb_fam_ipscafam_pacfam)
# 
# tb_cm_fam_ipscafam_pacfam <- bd_personas %>% 
#   filter(marca_afiliado_unico == "X" & categoria %in% c("A","B") & filial_famisanar == 1 & filial_ips_cafam == 1 & filial_pac_famisanar == 1 & cm == 1) %>%
#   group_by(piramide_2,categoria) %>% 
#   summarise(cm_fam_ipscafam_pacfam = n_distinct(id_persona)) %>% 
#   spread(categoria,cm_fam_ipscafam_pacfam) %>% 
#   dplyr::rename("cm_fam_ipscafam_pacfam_A"="A","cm_fam_ipscafam_pacfam_B"="B")
# str(tb_cm_fam_ipscafam_pacfam)
# 
# tb_fam_ipscafam_nopacfam <- bd_personas %>% 
#   filter(marca_afiliado_unico == "X" & categoria %in% c("A","B") & filial_famisanar == 1 & filial_ips_cafam == 1 & filial_pac_famisanar == 0) %>%
#   group_by(piramide_2,categoria) %>% 
#   summarise(fam_ipscafam_nopacfam = n_distinct(id_persona)) %>% 
#   spread(categoria,fam_ipscafam_nopacfam) %>% 
#   dplyr::rename("fam_ipscafam_nopacfam_A"="A","fam_ipscafam_nopacfam_B"="B")
# str(tb_fam_ipscafam_nopacfam)
# 
# tb_cm_fam_ipscafam_nopacfam <- bd_personas %>% 
#   filter(marca_afiliado_unico == "X" & categoria %in% c("A","B") & filial_famisanar == 1 & filial_ips_cafam == 1 & filial_pac_famisanar == 0 & cm == 1) %>%
#   group_by(piramide_2,categoria) %>% 
#   summarise(cm_fam_ipscafam_nopacfam = n_distinct(id_persona)) %>% 
#   spread(categoria,cm_fam_ipscafam_nopacfam) %>% 
#   dplyr::rename("cm_fam_ipscafam_nopacfam_A"="A","cm_fam_ipscafam_nopacfam_B"="B")
# str(tb_cm_fam_ipscafam_nopacfam)
# 
# str(bd_personas)
# bd_consulta <- tb_cat %>% 
#   left_join(tb_cm_A, by = "piramide_2") %>% 
#   left_join(tb_cm_B, by = "piramide_2") %>% 
#   left_join(tb_fam_A, by = "piramide_2") %>% 
#   left_join(tb_fam_B, by = "piramide_2") %>% 
#   left_join(tb_cm_fam, by = "piramide_2") %>% 
#   left_join(tb_nofam, by = "piramide_2") %>% 
#   left_join(tb_cm_nofam, by = "piramide_2") %>% 
#   left_join(tb_fam_ipscol, by = "piramide_2") %>% 
#   left_join(tb_cm_fam_ipscol, by = "piramide_2") %>% 
#   left_join(tb_fam_ipscafam, by = "piramide_2") %>% 
#   left_join(tb_cm_fam_ipscafam, by = "piramide_2") %>% 
#   left_join(tb_fam_ipscol_pacfam, by = "piramide_2") %>% 
#   left_join(tb_cm_fam_ipscol_pacfam, by = "piramide_2") %>% 
#   left_join(tb_fam_ipscol_nopacfam, by = "piramide_2") %>% 
#   left_join(tb_cm_fam_ipscol_nopacfam, by = "piramide_2") %>% 
#   left_join(tb_fam_ipscafam_pacfam, by = "piramide_2") %>% 
#   left_join(tb_cm_fam_ipscafam_pacfam, by = "piramide_2") %>% 
#   left_join(tb_fam_ipscafam_nopacfam, by = "piramide_2") %>% 
#   left_join(tb_cm_fam_ipscafam_nopacfam, by = "piramide_2")
# str(bd_consulta)
# 
# 
# write.csv2(bd_consulta, file = "CONSULTA_COL_PAC.csv", row.names = F)
# 
# 
# # library(devtools)
# # install_github("nik01010/dashboardthemes")
# 
# 
# 
# 
# #### COnsulta Edad ====
# library(dplyr)
# bd_seg_ind <- readRDS("Data/Originales_Septiembre/bd_afiliados_24102019.rds") %>% 
#   select(id_empresa,id_persona,Edad) %>% 
#   distinct() %>% 
#   mutate(rangoedad=as.factor(ifelse(Edad<18,"<18",
#                                     ifelse(Edad>=18&Edad<25,"[18-25)",
#                                            ifelse(Edad>=26&Edad<36,"[26-36)",
#                                                   ifelse(Edad>=37&Edad<47,"[37-47)",
#                                                          ifelse(Edad>=48&Edad<68,"[48-68)",">68")))))))
# #Correcion de niveles
# bd_seg_ind$rangoedad <- factor(bd_seg_ind$rangoedad, levels = c("<18","[18-25)","[26-36)","[37-47)","[48-68)",">68"))
# str(bd_seg_ind)
# 
# library(tidyr)
# empresas_edad <- bd_seg_ind %>% 
#   group_by(id_empresa,rangoedad) %>% 
#   summarise(conteo = n()) %>% 
#   spread(rangoedad,conteo) %>% 
#   data.frame() %>% 
#   rename("Menos 18"="X.18","[18-25)"="X.18.25.","[26-36)"="X.26.36.","[37-47)"="X.37.47.","[48-68)"="X.48.68.","Mas 68"="X.68")
# names(empresas_edad)
# 
# bd_empresas <- readRDS("Data/Originales_Septiembre/Info_emp_act_23102019.rds") %>% 
#   left_join(empresas_edad, by = "id_empresa") %>% 
#   mutate(n_empleados2 = rowSums(.[,c("Menos 18","[18-25)","[26-36)","[37-47)","[48-68)","Mas 68")],na.rm = T),
#          test_n = ifelse(n_empleados == n_empleados2, "Si", "No")) %>% 
#   select(-n_empleados)
# str(bd_empresas)
# saveRDS(bd_empresas, 
#         file = "Data/Originales_Septiembre/Info_empresas_empleados2.rds")
# 
# 
