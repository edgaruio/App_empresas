# PREPARAR DATOS
# Nota: Ver sion 11032019
# Se lee info_consulta y consumos para constiruir consumos
rm(list = ls())
library(data.table); library(dplyr); library(readxl); library(tm); library(tidyr); library(stringr); library(tm)
dir("Data/")

# Consumos para RyT

source("Data/Funciones.R")

# Base Segemento Individual
  
info_act <- readRDS("Data/Originales_Abril/bd_empresas07062019.rds")
names(info_act) <- chartr("áéíóú","aeiou",names(info_act))
str(info_act)

info_empresas <- fread("Data/Originales_Abril/Info_empresa.txt") %>% 
  data.frame()
str(info_empresas)

info_persona <- fread("Data/Originales_Abril/Info_persona.txt", sep =";", dec = ",") %>% 
  data.frame() %>% 
  group_by(id_empresa) %>% 
  summarise(pro_salario = mean(Salario, na.rm = T),
            pro_edad = mean(edad, na.rm = T))
str(info_persona)

# Informacion empresas KEVIN
info_inicial <- fread("Data/Info1_emp_obj.txt") 
str(info_inicial)

# Info Prospectos
info_prospectos <- read_excel("Data/Req_20190220_BaseEmpresas.xlsx", sheet = "Hoja1") %>% 
  select(id_empresa,ProspClu_n,ProspPis_n,ProspHot_n,Cuadrante_A,Cuadrante_A1,Cuadrante_A2,Cuadrante_B)
str(info_prospectos)

bd_empresas <- info_act %>% 
  left_join(info_prospectos, by = "id_empresa") %>% 
  left_join(info_empresas, by = "id_empresa") %>% 
  left_join(info_persona, by = "id_empresa") %>% 
  mutate(tipo_nit = ifelse(id_empresa == id_empresa_principal, "principal", "secundario"))
str(bd_empresas)

principales <- bd_empresas %>% 
  group_by(id_empresa_principal,nombre_empresa_principal,piramide_1,piramide_2) %>% 
  summarise(Afiliados = sum(n_empleados),
            Basico = sum(Basico),
            Medio = sum(Medio),
            Joven = sum(Joven),
            Alto = sum(Alto),
            A = sum(A),
            B = sum(B),
            C = sum(C),
            Promedio.de.Salario = mean(pro_salario),
            Promedio.de.edad = mean(pro_edad),
            Habeas_data = sum(n_habeas),
            Kit_derecho = sum(Kit_derecho),
            Kit_redimio = sum(Kit_redimio),
            Cuota_derecho = sum(n_cuota_derecho),
            Cuota_redimida = sum(n_cuota_redimida),
            famisanar = sum(n0_faminar),
            ips_colsubsidio = sum(n0_ips_colsubsidio),
            ips_cafam = sum(ips_cafam),
            otra_ips = sum(otra_ips),
            pac_famisanar = sum(pac_famisanar),
            suramericana = sum(suramericana),
            pac_suramericana = sum(pac_suramericana),
            proteccion = sum(proteccion),
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
            Bono_derecho = sum(Bono_derecho),
            Bono_redimido= sum(Bono_redimido),
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

secundarias <- bd_empresas %>% 
  filter(tipo_nit %in% c("secundario")) %>% 
  select(id_empresa,RazonSocial,piramide_1,piramide_2,Afiliados:Cuadrante_B) %>% 
  dplyr::rename(nombre = RazonSocial)%>% 
  mutate(tipo_nit = "secundaria")
str(bd_empresas)

bd_empresas <- rbind(principales, secundarias)
str(bd_empresas)

bd_empresas <- bd_empresas %>% 
  mutate(P_basico=Basico/Afiliados,
         P_medio=Medio/Afiliados,
         P_joven=Joven/Afiliados,
         P_alto=Alto/Afiliados) %>% 
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
str(bd_empresas)


bd_empresas1 <- bd_empresas %>%
  filter(!is.na(cluster)) %>% 
  dplyr::mutate(cluster = ifelse(P_basico+P_medio+P_joven+P_alto < 0.1, NA,
                                 ifelse(is.na(cluster),colnames(bd_empresas[54:57])[apply(bd_empresas[54:57],1,which.max)],cluster))) %>%
  dplyr::mutate(cluster = ifelse(cluster=="P_basico", "1. Basico",
                                 ifelse(cluster=="P_joven", "3. Joven",
                                        ifelse(cluster=="P_medio", "2. Medio",
                                               ifelse(cluster=="P_alto", "4. Alto",cluster)))))


bd_empresas <- rbind(bd_empresas %>% filter(is.na(cluster)),
                     bd_empresas1)

write.csv2(bd_empresas, file = "Data/Info_emp_act_20032019.csv", row.names = FALSE)

### Consulta PAC
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
