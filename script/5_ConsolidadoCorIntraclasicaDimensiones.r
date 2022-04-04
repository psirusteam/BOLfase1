library(tidyverse)
setwd("F:/Documents/CEPAL 2022/Bases_de_datos_Censo_Bolivia_2012/output/5.CorrelacionesIntraclasicas")

df_nbi <- readRDS("df_nbi.rds")
consulta_ocupac <- readRDS("consulta_ocupac.rds")


setwd("F:/Documents/CEPAL 2022/Bases_de_datos_Censo_Bolivia_2012/output/4.UPM_Colapsadas_indicadores")
consulta_ocupacCompleto <- readRDS("consulta_ocupac.rds")



N_viv <- nrow(df_nbi)
datos_propor_dimNBI <- df_nbi %>% summarise(CARENCIA_CALIDADVIVIENDA = sum(CARENCIA_CALIDADVIVIENDA),
                                            HACINAMIENTO = sum(HACINAMIENTO),
                                            CARENCIA_ABASTECAGUA = sum(CARENCIA_ABASTECAGUA),
                                            CARENCIA_SERVSANITARIO = sum(CARENCIA_SERVSANITARIO),
                                            CARENCIA_INSUMOSENERGETICOS = sum(CARENCIA_INSUMOSENERGETICOS),
                                            CARENCIA_ASISESCOLAR = sum(CARENCIA_ASISESCOLAR),
                                            CARENCIA_SALUD = sum(CARENCIA_SALUD),
                                            CARENCIA_PRECARIEDADOCUPACIONAL = sum(CARENCIA_PRECARIEDADOCUPACIONAL),
                                            NBI = sum(NBI)) %>% mutate(
                                              p_CARENCIA_CALIDADVIVIENDA = CARENCIA_CALIDADVIVIENDA / N_viv,
                                              p_HACINAMIENTO = HACINAMIENTO / N_viv,
                                              p_CARENCIA_ABASTECAGUA = CARENCIA_ABASTECAGUA / N_viv,
                                              p_CARENCIA_SERVSANITARIO = CARENCIA_SERVSANITARIO / N_viv,
                                              p_CARENCIA_INSUMOSENERGETICOS = CARENCIA_INSUMOSENERGETICOS / N_viv,
                                              p_CARENCIA_ASISESCOLAR = CARENCIA_ASISESCOLAR / N_viv,
                                              p_CARENCIA_SALUD = sum(CARENCIA_SALUD) / N_viv,
                                              p_CARENCIA_PRECARIEDADOCUPACIONAL = CARENCIA_PRECARIEDADOCUPACIONAL / N_viv,
                                              p_NBI = NBI / N_viv)




N_ocupac <- nrow(consulta_ocupac)
datos_propor_ocup <- consulta_ocupac %>% summarise(PET = sum(PET),
                                                     PEA = sum(PEA),
                                                     Desocupados  = sum(Desocupados),
                                                     Ocupados  = sum(Ocupados)) %>%
  mutate(p_PET = PET / N_ocupac,
         p_PEA = PEA / N_ocupac,
         p_Desocupados = Desocupados / N_ocupac,
         p_Ocupados = Ocupados / N_ocupac)


# Estadísticas
N_viv = nrow(df_nbi) # Se puede guardar
N_pea = nrow(consulta_ocupac) # Se puede guardar

M_viv = df_nbi[["id_upm_inicial"]] %>% unique() %>% length() # Se puede guardar
M_50_viv = df_nbi[["id_upm50"]] %>% unique() %>% length() # Se puede guardar
M_100_viv = df_nbi[["id_upm100"]] %>% unique() %>% length() # Se puede guardar

# M_ocup = consulta_ocupac[["id_upm_inicial"]] %>% unique() %>% length() # Se puede guardar
# M_50_ocup = consulta_ocupac[["id_upm50"]] %>% unique() %>% length() # Se puede guardar
# M_100_ocup = consulta_ocupac[["id_upm100"]] %>% unique() %>% length() # Se puede guardar

#b <- nrow(consulta_ocupac)/length(unique(consulta_ocupac$I_BC_VIV)) # Número de miembros

b <- nrow(consulta_ocupacCompleto)/length(unique(consulta_ocupacCompleto$I_BC_VIV)) 

r_PEA <- sum(consulta_ocupacCompleto$PEA, na.rm = T) / nrow(consulta_ocupacCompleto) # Se puede guardar

lista_estadisticas <- list(N_viv = N_viv, 
                           N_pea = N_pea, r_PEA = r_PEA, b = b,
                           M_viv = M_viv, 
                           M_50_viv = M_50_viv,
                           M_100_viv = M_100_viv)
 
setwd("F:/Documents/CEPAL 2022/Bases_de_datos_Censo_Bolivia_2012/output/5C_EstadisticasDimensiones")
saveRDS(datos_propor_dimNBI, "datos_propor_dimNBI.rds")
saveRDS(datos_propor_ocup, "datos_propor_ocup.rds")
saveRDS(lista_estadisticas, "lista_estadisticas.rds")

