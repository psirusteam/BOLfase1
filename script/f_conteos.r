
# Paquetes necesarios para procesamiento
library(rlang)
library(dplyr)
library(arrow)


f_conteos <- function(datos_arrow, datos_etiquetas, string_variable){
  
  
  consultaA <- val_labels(datos_etiquetas[[string_variable]]) %>% as.table() %>% 
    as.data.frame() %>% `names<-`(c(var_label(datos_etiquetas[[string_variable]]),                                                             string_variable))
  
  consultaB <- datos_arrow %>% group_by(!!sym(string_variable)) %>% 
    summarise(`Número de personas` = n()) %>% 
    collect() %>% 
    left_join(consultaA, by = string_variable) %>%
    select(var_label(datos_etiquetas[[string_variable]]), `Número de personas`)
  return(consultaB)
}


f_conteo_personas <- function(datos_arrow = persona_arrow, datos_etiquetas = etiquetas_personas, string_variable){
  
  
  consultaA <- val_labels(datos_etiquetas[[string_variable]]) %>% as.table() %>% 
    as.data.frame() %>% `names<-`(c(var_label(datos_etiquetas[[string_variable]]),                                                             string_variable))
  
  consultaB <- datos_arrow %>% group_by(!!sym(string_variable)) %>% 
    summarise(`Número de personas` = n()) %>% 
    collect() %>% 
    left_join(consultaA, by = string_variable) %>%
    select(var_label(datos_etiquetas[[string_variable]]), `Número de personas`)
  return(consultaB)
}

f_conteo_hogares <- function(datos_arrow = hogar_arrow, datos_etiquetas = etiquetas_hogares, string_variable){
  
  
  consultaA <- val_labels(datos_etiquetas[[string_variable]]) %>% as.table() %>% 
    as.data.frame() %>% `names<-`(c(var_label(datos_etiquetas[[string_variable]]),                                                             string_variable))
  
  consultaB <- datos_arrow %>% group_by(!!sym(string_variable)) %>% 
    summarise(`Número de hogares` = n()) %>% 
    collect() %>% 
    left_join(consultaA, by = string_variable) %>%
    select(var_label(datos_etiquetas[[string_variable]]), `Número de hogares`)
  return(consultaB)
}


f_conteo_viviendas <- function(datos_arrow = vivienda_arrow, datos_etiquetas = etiquetas_viviendas, string_variable){
  
  
  consultaA <- val_labels(datos_etiquetas[[string_variable]]) %>% as.table() %>% 
    as.data.frame() %>% `names<-`(c(var_label(datos_etiquetas[[string_variable]]),                                                             string_variable))
  
  consultaB <- datos_arrow %>% group_by(!!sym(string_variable)) %>% 
    summarise(`Número de hogares` = n()) %>% 
    collect() %>% 
    left_join(consultaA, by = string_variable) %>%
    select(var_label(datos_etiquetas[[string_variable]]), `Número de hogares`)
  return(consultaB)
}
