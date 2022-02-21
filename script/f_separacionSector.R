

f_separacionSector <- function(datos_sectores, nom_sector, nom_sectorFinal = NULL, nom_tam,  umbral){

LI <- umbral - (umbral / 2)
LS <- umbral + (umbral / 2)
n <- nrow(datos_sectores)

sector <- datos_sectores[[nom_sector]]
tam <- datos_sectores[[nom_tam]]

num_grupos <- rep(NA, n)

for(i in 1:n){
 
# Identificar cuantos grupos se conforman inicialmente en cada secctor en el proceso de subdivisión
  num_grupos[i] <- ifelse(tam[i] >= umbral & (tam[i] %% umbral == 0), tam[i] %/% umbral,
                          ifelse(tam[i] >= umbral & (tam[i] %% umbral > 0), 1 + tam[i] %/% umbral,
                                 1))
  }                       

# Crear la repetición de los sectores de acuerdo al número de grupos
sectores_nuevos <- 1:sum(num_grupos)

tam_nuevos <- rep(NA, length(sectores_nuevos))
cum_num_grupos <- cumsum(num_grupos)
cum_num_grupos <- c(1, cum_num_grupos)

for(i in 1:n){
# Caso menor a 100
  if(tam[i] < umbral) { 
    tam_nuevos[(cum_num_grupos[i+1] - (num_grupos[i] - 1)):cum_num_grupos[i+1]] <- tam[i]
  }
  
  # Caso mayor a 100 
      # Residual es cero
    if(tam[i] >= umbral &  (tam[i] %% umbral ==0)) { 
      tam_nuevos[(cum_num_grupos[i+1] - (num_grupos[i] - 1)):cum_num_grupos[i+1]] <- rep(umbral, num_grupos[i])
    }
  
    # Residual del tamaño del grupo sobre el umbral diferente a cero
    if(tam[i] >= umbral &  (tam[i] %% umbral !=0)) { 
    tam_nuevos[(cum_num_grupos[i+1] - (num_grupos[i] - 1)):cum_num_grupos[i+1]] <-
      c(rep(umbral, num_grupos[i] - 1), tam[i] %% umbral)
  }
}

salida <- data.frame(sector = rep(sector, num_grupos), sectores_nuevos, tam_nuevos, stringsAsFactors = F)
names(salida)[names(salida) == "sector"] <- nom_sector
if(!is.null(nom_sectorFinal)) names(salida)[names(salida) == "sectores_nuevos"] <- nom_sectorFinal

salida

}

 
# ################################# Colapsar sectores ########

 
# prueba <- structure(list(Sector = 1:7, Tam = c(140L, 60L, 160L, 20L, 40L, 
#                                                 420L, 300L)), class = "data.frame", row.names = c(NA, -7L))
#  
# umbral <- 100 # argumento
# 
#  
# res <- f_separacionSector(datos_sectores = prueba, nom_sector = "Sector", nom_sectorFinal = "Sector Final", 
#                           nom_tam = "Tam", umbral = 100)
#  res
# 
# 
# 
# prueba2 <- data.frame(Sector = 1, Tam = 400, stringsAsFactors = F)
# res2 <- f_separacionSector(datos_sectores = prueba2, nom_sector = "Sector", nom_sectorFinal = "Sector Final", 
#                           nom_tam = "Tam", umbral = 100)
# res2
