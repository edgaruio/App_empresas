# PREPARAR DATOS
# Nota: Ver sion 11032019
# Se lee info_consulta y consumos para constiruir consumos
rm(list = ls())
library(data.table); library(dplyr); library(readxl); library(tm); library(tidyr); library(stringr); library(tm)
dir("Data/")

# Consumos para RyT

source("Data/Funciones.R")

consumo_individual <- readRDS("Data/Originales_Abril/consumo_individual_abril.rds") %>% 
  data.frame() %>% 
  mutate(segmento_poblacional = iconv(segmento_poblacional,to="ASCII//TRANSLIT"),
         servicio = iconv(servicio,to="ASCII//TRANSLIT"),
         anio_mes = paste(anno,mes,sep="")) %>% 
  filter(anio_mes >= 20181)
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

cupo_credito <- consumo_individual %>% 
  filter(servicio == "Cupo Titular") %>% 
  select(id_empresa,id_persona,ues) %>% 
  group_by(id_empresa) %>% 
  summarise(Cupo_credito = n_distinct(id_persona))
str(cupo_credito)

consumo_credito <- consumo_individual %>% 
  filter(servicio %in% c("Libranza","No Libranza")) %>% 
  select(id_empresa,id_persona,ues) %>% 
  group_by(id_empresa) %>% 
  summarise(Consumo_credito = n_distinct(id_persona))
str(consumo_credito)

educacion <- consumo_individual %>% 
  filter(ues == "Educacion") %>% 
  select(id_empresa,id_persona,ues) %>% 
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


# Informacion empresas KEVIN
# info_inicial <- fread("Data/Info1_emp_obj.txt")
# str(info_inicial)
# info1 <- fread("Data/info_p1.txt", sep = ";", dec = ",") %>%
#   data.frame()
# colnames(info1) <- chartr("áéíóú","aeiou",names(info1))
# names(info1)
# info2 <- fread("Data/info_p2.txt") %>%
#   data.frame() %>%
#   mutate_all(funs(ifelse(is.na(.),0,.)))
# colnames(info2) <- chartr("áéíóú","aeiou",names(info2))
# str(info2)

bd_empresas <- readRDS("Data/Originales_Mayo/bd_empresas11062019_p10.rds") %>% 
  select(id_empresa,n_empleados,Alto:n_cuota_redimida,n0_famisanar,n0_ips_colsubsidio,n0_pac_famisanar,n0_suramericana,n0_pac_sura)
names(bd_empresas) <- chartr("áéíóú","aeiou",names(bd_empresas))
str(bd_empresas)

info_empresas <- readRDS("Data/Originales_Mayo/Info_empresa.rds")
str(info_empresas)

pre_aprobados <- fread("Data/bonos.txt") %>% 
  select(id_empresa,pre_aprobado_hipotecario,pre_aprobado_cupo) %>% 
  data.frame()
str(pre_aprobados)

# Info Prospectos
info_prospectos <- read_excel("Data/Req_20190220_BaseEmpresas.xlsx", sheet = "Hoja1") %>% 
  select(id_empresa,ProspClu_n,ProspPis_n,ProspHot_n,Cuadrante_A,Cuadrante_A1,Cuadrante_A2,Cuadrante_B)
str(info_prospectos)

ciiu <- fread("Data/Originales_Abril/CIIU.txt") %>% 
  select(ID_CIIU,DescripcionCIIU) %>% 
  data.frame()
str(ciiu)

ciiu22 <- fread("Data/Originales_Abril/CIIU_SectorEconomico22.txt") %>% 
  select(id_sectorComercial_22_actividad,ACTIVIDAD) %>% 
  data.frame()
str(ciiu22)

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
  left_join(ciiu22, by = "id_sectorComercial_22_actividad") %>% 
  data.frame()
str(info_act)
info_act[,c(7:56)][is.na(info_act[,c(7:56)])] <- 0


info_act <- info_act %>% 
  na.omit() %>% 
  mutate(tipo_nit = ifelse(id_empresa == id_empresa_principal, "principal", "secundario"),
         ACTIVIDAD = ifelse(is.na(ACTIVIDAD),"Sin Información", ACTIVIDAD))
str(info_act)

# saveRDS(bd_empresas, file = "Data/Originales_Abril/bd_empresas_test.rds")

principales <- info_act %>% 
  group_by(id_empresa_principal,nombre_empresa_principal,piramide_1,piramide_2,ACTIVIDAD) %>% 
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
            n_cuota_derecho = sum(n_cuota_derecho),
            n_cuota_redimida = sum(n_cuota_redimida),
            n0_famisanar = sum(n0_famisanar),
            n0_ips_colsubsidio = sum(n0_ips_colsubsidio),
            n_cafam = sum(n_cafam),
            n_otraips = sum(n_otraips),
            n0_pac_famisanar = sum(n0_pac_famisanar),
            n0_suramericana = sum(n0_suramericana),
            n0_pac_sura = sum(n0_pac_sura),
            n_proteccion = sum(n_proteccion),
            Cupo_credito = sum(Cupo_credito),
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
  mutate(tipo_nit = "principal")
str(principales)
str(info_act)

secundarias <- info_act %>% 
  filter(tipo_nit %in% c("secundario")) %>% 
  select(id_empresa,razon_social,piramide_1,piramide_2,ACTIVIDAD,Promedio.de.Salario:Cuadrante_B) %>% 
  dplyr::rename(nombre = razon_social) %>%
  mutate(tipo_nit = "secundaria") %>% 
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
                         bd_empresas_app1)

write.csv2(bd_empresas_app, file = "Data/Originales_Mayo/Info_emp_act_13062019.csv", row.names = FALSE)
saveRDS(bd_empresas_app, file = "Data/Originales_Mayo/Info_emp_act_13062019.rds")


## Informacion adicional

consulta_seg_empresas <- fread("Data/Originales_Abril/Consulta_Seg_Emp.txt") %>% 
  data.frame() %>% 
  select(id_persona,id_sectorComercial_22_actividad,marca_afiliado_unico) %>% 
  filter(marca_afiliado_unico == "X") %>% 
  left_join(ciiu22, by = "id_sectorComercial_22_actividad") %>% 
  select(-c(id_sectorComercial_22_actividad,marca_afiliado_unico))
str(consulta_seg_empresas)

consulta_piramide <- fread("Data/Consulta_piramide.txt") %>% 
  filter(marca_afiliado_unico == "X") %>% 
  select(-marca_afiliado_unico) %>% 
  left_join(consulta_seg_empresas, by = "id_persona")
str(consulta_piramide)

write.csv2(consulta_piramide, file = "Data/Originales_Mayo/consulta_piramide_plot.csv", row.names = FALSE)
saveRDS(consulta_piramide, file = "Data/Originales_Mayo/consulta_piramide_plot.rds")



### Consulta PAC=====================
rm(list = ls())
library(dplyr); library(data.table); library(tidyr)

bd_personas <- fread("Consulta_PAC.txt", sep = ";", dec = ",") %>% 
  data.frame()
names_pir2 <- names(table(bd_personas$piramide_2))[c(1:11,15)]

bd_personas <- bd_personas %>% 
  filter(piramide_2 %in% names_pir2) %>% 
  mutate(cm = ifelse(numero_cuota_monetaria >= 1, 1, 0))
str(bd_personas)  

tb_cat <- bd_personas %>% 
  filter(marca_afiliado_unico == "X") %>% 
  group_by(piramide_2,categoria) %>% 
  summarise(Conteo = n_distinct(id_persona)) %>% 
  spread(key = "categoria", value = "Conteo") %>% 
  select(-C) %>% 
  data.frame()
str(tb_cat)

tb_cm_A <- bd_personas %>% 
  filter(marca_afiliado_unico == "X" & categoria == "A" & cm == 1) %>% 
  group_by(piramide_2) %>% 
  summarise(A_CM = sum(cm, na.rm = T))
str(tb_cm_A)

tb_cm_B <- bd_personas %>% 
  filter(marca_afiliado_unico == "X" & categoria == "B" & cm == 1) %>% 
  group_by(piramide_2) %>% 
  summarise(B_CM = sum(cm, na.rm = T))
str(tb_cm_B)

tb_fam_A <- bd_personas %>% 
  filter(marca_afiliado_unico == "X" & categoria == "A" & filial_famisanar == 1) %>% 
  group_by(piramide_2) %>% 
  summarise(famisanar = n_distinct(id_persona)) %>% 
  dplyr::rename("A_Famisanar"="famisanar")
str(tb_fam_A)

tb_fam_B <- bd_personas %>% 
  filter(marca_afiliado_unico == "X" & categoria == "B" & filial_famisanar == 1) %>% 
  group_by(piramide_2) %>% 
  summarise(famisanar = n_distinct(id_persona)) %>% 
  dplyr::rename("B_Famisanar"="famisanar")
str(tb_fam_B)

tb_cm_fam <- bd_personas %>% 
  filter(marca_afiliado_unico == "X" & categoria %in% c("A","B") & filial_famisanar == 1, cm == 1) %>% 
  group_by(piramide_2,categoria) %>% 
  summarise(cm_fam = n_distinct(id_persona)) %>% 
  spread(categoria,cm_fam) %>% 
  dplyr::rename("CM_A_FAM"="A","CM_B_FAM"="B")
str(tb_cm_fam)

tb_nofam <- bd_personas %>% 
  filter(marca_afiliado_unico == "X" & categoria %in% c("A","B") & filial_famisanar == 0) %>%
  group_by(piramide_2,categoria) %>% 
  summarise(nofam = n_distinct(id_persona)) %>% 
  spread(categoria,nofam) %>% 
  dplyr::rename("NoFam_A"="A","Nofam_B"="B")
str(tb_nofam)

tb_cm_nofam <- bd_personas %>% 
  filter(marca_afiliado_unico == "X" & categoria %in% c("A","B") & filial_famisanar == 0 & cm == 1) %>%
  group_by(piramide_2,categoria) %>% 
  summarise(nofam = n_distinct(id_persona)) %>% 
  spread(categoria,nofam) %>% 
  dplyr::rename("CM_NoFam_A"="A","CM_Nofam_B"="B")
str(tb_nofam)

tb_fam_ipscol <- bd_personas %>% 
  filter(marca_afiliado_unico == "X" & categoria %in% c("A","B") & filial_famisanar == 1 & filial_ips_colsubsidio == 1) %>% 
  group_by(piramide_2,categoria) %>% 
  summarise(fam_ipscol = n_distinct(id_persona)) %>% 
  spread(categoria,fam_ipscol) %>% 
  dplyr::rename("Fam_ipscol_A"="A","Fam_ipscol_B"="B")
str(tb_fam_ipscol)

tb_cm_fam_ipscol <- bd_personas %>% 
  filter(marca_afiliado_unico == "X" & categoria %in% c("A","B") & filial_famisanar == 1 & filial_ips_colsubsidio == 1 & cm == 1) %>% 
  group_by(piramide_2,categoria) %>% 
  summarise(fam_ipscol = n_distinct(id_persona)) %>% 
  spread(categoria,fam_ipscol) %>% 
  dplyr::rename("CM_Fam_ipscol_A"="A","CM_Fam_ipscol_B"="B")
str(tb_cm_fam_ipscol)

tb_fam_ipscafam <- bd_personas %>% 
  filter(marca_afiliado_unico == "X" & categoria %in% c("A","B") & filial_famisanar == 1 & filial_ips_cafam == 1) %>% 
  group_by(piramide_2,categoria) %>% 
  summarise(fam_ipscafam = n_distinct(id_persona)) %>% 
  spread(categoria,fam_ipscafam) %>% 
  dplyr::rename("Fam_ipscafam_A"="A","Fam_ipscafam_B"="B")
str(tb_cm_fam_ipscol)

tb_cm_fam_ipscafam <- bd_personas %>% 
  filter(marca_afiliado_unico == "X" & categoria %in% c("A","B") & filial_famisanar == 1 & filial_ips_cafam == 1 & cm == 1) %>% 
  group_by(piramide_2,categoria) %>% 
  summarise(cm_fam_ipscafam = n_distinct(id_persona)) %>% 
  spread(categoria,cm_fam_ipscafam) %>% 
  dplyr::rename("CM_Fam_ipscafam_A"="A","CM_Fam_ipscafam_B"="B")
str(tb_cm_fam_ipscafam)

tb_fam_ipscol_pacfam <- bd_personas %>% 
  filter(marca_afiliado_unico == "X" & categoria %in% c("A","B") & filial_famisanar == 1 & filial_ips_colsubsidio == 1 & filial_pac_famisanar == 1) %>% 
  group_by(piramide_2,categoria) %>% 
  summarise(fam_ipscol_pacfam = n_distinct(id_persona)) %>% 
  spread(categoria,fam_ipscol_pacfam) %>% 
  dplyr::rename("Fam_ipscol_pacfam_A"="A","Fam_ipscol_pacfam_B"="B")
str(tb_fam_ipscol_pacfam)

tb_cm_fam_ipscol_pacfam <- bd_personas %>% 
  filter(marca_afiliado_unico == "X" & categoria %in% c("A","B") & filial_famisanar == 1 & filial_ips_colsubsidio == 1 & filial_pac_famisanar == 1 & cm == 1) %>% 
  group_by(piramide_2,categoria) %>% 
  summarise(cm_fam_ipscol_pacfam = n_distinct(id_persona)) %>% 
  spread(categoria,cm_fam_ipscol_pacfam) %>% 
  dplyr::rename("CM_fam_ipscol_pacfam_A"="A","CM_fam_ipscol_pacfam_B"="B")
str(tb_fam_ipscol_pacfam)

tb_fam_ipscol_nopacfam <- bd_personas %>% 
  filter(marca_afiliado_unico == "X" & categoria %in% c("A","B") & filial_famisanar == 1 & filial_ips_colsubsidio == 1 & filial_pac_famisanar == 0) %>%
  group_by(piramide_2,categoria) %>% 
  summarise(fam_ipscol_nopacfam = n_distinct(id_persona)) %>% 
  spread(categoria,fam_ipscol_nopacfam) %>% 
  dplyr::rename("Fam_ipscol_nopacfam_A"="A","Fam_ipscol_nopacfam_B"="B")
str(tb_fam_ipscol_nopacfam)

tb_cm_fam_ipscol_nopacfam <- bd_personas %>% 
  filter(marca_afiliado_unico == "X" & categoria %in% c("A","B") & filial_famisanar == 1 & filial_ips_colsubsidio == 1 & filial_pac_famisanar == 0 & cm == 1) %>%
  group_by(piramide_2,categoria) %>% 
  summarise(cm_fam_ipscol_nopacfam = n_distinct(id_persona)) %>% 
  spread(categoria,cm_fam_ipscol_nopacfam) %>% 
  dplyr::rename("CM_Fam_ipscol_nopacfam_A"="A","CM_Fam_ipscol_nopacfam_B"="B")
str(tb_cm_fam_ipscol_nopacfam)

tb_fam_ipscafam_pacfam <- bd_personas %>% 
  filter(marca_afiliado_unico == "X" & categoria %in% c("A","B") & filial_famisanar == 1 & filial_ips_cafam == 1 & filial_pac_famisanar == 1) %>%
  group_by(piramide_2,categoria) %>% 
  summarise(fam_ipscafam_pacfam = n_distinct(id_persona)) %>% 
  spread(categoria,fam_ipscafam_pacfam) %>% 
  dplyr::rename("fam_ipscafam_pacfam_A"="A","fam_ipscafam_pacfam_B"="B")
str(tb_fam_ipscafam_pacfam)

tb_cm_fam_ipscafam_pacfam <- bd_personas %>% 
  filter(marca_afiliado_unico == "X" & categoria %in% c("A","B") & filial_famisanar == 1 & filial_ips_cafam == 1 & filial_pac_famisanar == 1 & cm == 1) %>%
  group_by(piramide_2,categoria) %>% 
  summarise(cm_fam_ipscafam_pacfam = n_distinct(id_persona)) %>% 
  spread(categoria,cm_fam_ipscafam_pacfam) %>% 
  dplyr::rename("cm_fam_ipscafam_pacfam_A"="A","cm_fam_ipscafam_pacfam_B"="B")
str(tb_cm_fam_ipscafam_pacfam)

tb_fam_ipscafam_nopacfam <- bd_personas %>% 
  filter(marca_afiliado_unico == "X" & categoria %in% c("A","B") & filial_famisanar == 1 & filial_ips_cafam == 1 & filial_pac_famisanar == 0) %>%
  group_by(piramide_2,categoria) %>% 
  summarise(fam_ipscafam_nopacfam = n_distinct(id_persona)) %>% 
  spread(categoria,fam_ipscafam_nopacfam) %>% 
  dplyr::rename("fam_ipscafam_nopacfam_A"="A","fam_ipscafam_nopacfam_B"="B")
str(tb_fam_ipscafam_nopacfam)

tb_cm_fam_ipscafam_nopacfam <- bd_personas %>% 
  filter(marca_afiliado_unico == "X" & categoria %in% c("A","B") & filial_famisanar == 1 & filial_ips_cafam == 1 & filial_pac_famisanar == 0 & cm == 1) %>%
  group_by(piramide_2,categoria) %>% 
  summarise(cm_fam_ipscafam_nopacfam = n_distinct(id_persona)) %>% 
  spread(categoria,cm_fam_ipscafam_nopacfam) %>% 
  dplyr::rename("cm_fam_ipscafam_nopacfam_A"="A","cm_fam_ipscafam_nopacfam_B"="B")
str(tb_cm_fam_ipscafam_nopacfam)

str(bd_personas)
bd_consulta <- tb_cat %>% 
  left_join(tb_cm_A, by = "piramide_2") %>% 
  left_join(tb_cm_B, by = "piramide_2") %>% 
  left_join(tb_fam_A, by = "piramide_2") %>% 
  left_join(tb_fam_B, by = "piramide_2") %>% 
  left_join(tb_cm_fam, by = "piramide_2") %>% 
  left_join(tb_nofam, by = "piramide_2") %>% 
  left_join(tb_cm_nofam, by = "piramide_2") %>% 
  left_join(tb_fam_ipscol, by = "piramide_2") %>% 
  left_join(tb_cm_fam_ipscol, by = "piramide_2") %>% 
  left_join(tb_fam_ipscafam, by = "piramide_2") %>% 
  left_join(tb_cm_fam_ipscafam, by = "piramide_2") %>% 
  left_join(tb_fam_ipscol_pacfam, by = "piramide_2") %>% 
  left_join(tb_cm_fam_ipscol_pacfam, by = "piramide_2") %>% 
  left_join(tb_fam_ipscol_nopacfam, by = "piramide_2") %>% 
  left_join(tb_cm_fam_ipscol_nopacfam, by = "piramide_2") %>% 
  left_join(tb_fam_ipscafam_pacfam, by = "piramide_2") %>% 
  left_join(tb_cm_fam_ipscafam_pacfam, by = "piramide_2") %>% 
  left_join(tb_fam_ipscafam_nopacfam, by = "piramide_2") %>% 
  left_join(tb_cm_fam_ipscafam_nopacfam, by = "piramide_2")
str(bd_consulta)


write.csv2(bd_consulta, file = "CONSULTA_COL_PAC.csv", row.names = F)


# library(devtools)
# install_github("nik01010/dashboardthemes")
