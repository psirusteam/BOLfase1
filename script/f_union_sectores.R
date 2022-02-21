
f_union_sectores <- function(datos_sectores, nomb_sectorInicial = NULL, 
                             nom_sector_intermedio, nom_tam_sector_intermedio, 
                             nom_sectorFinal, nom_Tam_sectorFinal, 
                             restriccion_sectores = NULL,
                             umbral){
  
  if(!is.null(nomb_sectorInicial)){
    sectorInicial <- datos_sectores[[nomb_sectorInicial]]
  }
  
  sector_intermedio <- datos_sectores[[nom_sector_intermedio]]
  tam <- datos_sectores[[nom_tam_sector_intermedio]]
  
  sector_intermedio_nuevo <-rep(NA, length(sector_intermedio))
  acum <- rep(NA, length(sector_intermedio))
  
  cond <- tam[1] # Revisar si se debe partir el primero
  i = 1
  j = 1
  
  n_datos <- nrow(datos_sectores)
  
  while(i <= n_datos){ 
    
    if(restriccion_sectores == T){
      
      condicion_ContiguidadSectores <- as.logical(as.character(sectorInicial[i]) == 
                                                    as.character(sectorInicial[i+1]))
      
      
      
      while(condicion_ContiguidadSectores  &  tam[i] < (umbral / 2) & cond <= 1.5 * umbral & 
            i + j + 1 <= n_datos){
        
        if(restriccion_sectores & sectorInicial[i] == sectorInicial[i+j]){  
          acum[i] <- tam[i]   
          acum[i+j] <- tam[i+j] + acum[i + (j-1)]  
          
          sector_intermedio_nuevo[i] <- sector_intermedio[i] 
          sector_intermedio_nuevo[i+j] <- sector_intermedio[i] 
          
          cond <- acum[i+j] + tam[i+j + 1] #+ tam[i+j] + tam[i+j + 1]  # probar con este tam[i+j+1] antes acum[i+j] + tam[i+j]
          j <- j + 1
          
        } 
        
        if(sectorInicial[i] != sectorInicial[i+j]){
          cond = 1.5 * umbral + 1
        }
        
      } # Cierra while
      
      i = i + j # Si salío de un while recientemente
      
      
      j = 1 # si no salío del while recientement
      cond <- tam[i]
      
      
    }  # cierra if
    
    
    if(restriccion_sectores == F){
      
      while(tam[i] < (umbral / 2) & cond <= 1.5 * umbral & 
            (tam[i] + tam[i+1]) <= 1.5 * umbral &
            i + j + 1 <= n_datos){
        acum[i] <- tam[i]   
        acum[i+j] <- tam[i+j] + acum[i + (j-1)]  
        
        sector_intermedio_nuevo[i] <- sector_intermedio[i] 
        sector_intermedio_nuevo[i+j] <- sector_intermedio[i] 
        
        cond <- acum[i+j] + tam[i + j + 1] #+ tam[i+j] + tam[i+j + 1]  # probar con este tam[i+j+1] antes acum[i+j] + tam[i+j]
        
        j <- j + 1 
      }
      
      
      i = i + j # Si salío de un while recientemente
      
      j = 1 # si no salío del while recientement
      cond <- tam[i+1] # i+ 1
    }
    
    
  }
  
  
  
  if(!is.null(nomb_sectorInicial)){
    detalleCreacionSectores <- data.frame(sectorInicial, sector_intermedio, tam, 
                                          sector_intermedio_nuevo, acum, stringsAsFactors = F)
  } else{
    detalleCreacionSectores <- data.frame(sector_intermedio, tam, sector_intermedio_nuevo, 
                                          acum, stringsAsFactors = F)
    
  }
  
  
  detalleCreacionSectores$sector_intermedio_nuevo <- ifelse(is.na(detalleCreacionSectores$sector_intermedio_nuevo),
                                                            detalleCreacionSectores$sector_intermedio, 
                                                            detalleCreacionSectores$sector_intermedio_nuevo)
  detalleCreacionSectores$tam_sectores_nuevos <- ifelse(is.na(detalleCreacionSectores$acum), 
                                                        detalleCreacionSectores$tam,  detalleCreacionSectores$acum)
  detalleCreacionSectores$acum <- NULL
  
  if(!is.null(nom_sectorFinal)){
    names(detalleCreacionSectores) <- c( nomb_sectorInicial, nom_sector_intermedio, nom_tam_sector_intermedio, nom_sectorFinal, nom_Tam_sectorFinal )
  }else{
    names(detalleCreacionSectores) <- c(nom_sector_intermedio, nom_tam_sector_intermedio, nom_sectorFinal, 
                                        nom_Tam_sectorFinal)
    
  }
  
  detalleCreacionSectores$Sector_Final <- detalleCreacionSectores[[nom_sectorFinal]]
  detalleCreacionSectores$Tam_SectorFinal <- detalleCreacionSectores[[nom_Tam_sectorFinal]]
  
  detalleCreacionSectores <- detalleCreacionSectores %>% dplyr::group_by(Sector_Final) %>% 
    dplyr::mutate(Tam_SectorFinal = max(Tam_SectorFinal))
  
  detalleCreacionSectores <- as.data.frame(detalleCreacionSectores)
  
  detalleCreacionSectores[[nom_Tam_sectorFinal]] <- detalleCreacionSectores$Tam_SectorFinal  
  
  
  df_creacionSectores <- detalleCreacionSectores %>% dplyr::group_by(Sector_Final) %>%
    dplyr::summarise(Tam_SectorFinal = max(Tam_SectorFinal))
  names(df_creacionSectores) <- c(nom_sectorFinal, nom_Tam_sectorFinal)
  
  df_creacionSectores <- as.data.frame(df_creacionSectores)
  
  detalleCreacionSectores$Sector_Final <- NULL
  detalleCreacionSectores$Tam_SectorFinal <- NULL
  
  salida <- list(df_creacionSectores, detalleCreacionSectores)
  names(salida) <- c("df_creacionSectores", "df_detalleCreacionSectores")
  return(salida)
}


datos_prueba <- data.frame(sector = 1:12, Tam = c(40, 60, 120, 5, 20, 10, 140, 51,
                                                  90, 20, 15, 400),
                           stringsAsFactors = F)


prueba <- f_union_sectores(datos_sectores = datos_prueba,
                           nom_sector_intermedio = "sector",
                           nom_tam_sector_intermedio = "Tam", 
                           nom_sectorFinal = "Sector_Final",
                           nom_Tam_sectorFinal = "Tam_SectorFinal",
                           restriccion_sectores = F,
                 umbral = 50)
# prueba$df_creacionSectores
# prueba$df_detalleCreacionSectores




datos_prueba <- data.frame(sector = letters[1:12], Tam = c(40, 60, 120, 5, 20, 10, 140, 51,
                                                  90, 20, 15, 400),
                           stringsAsFactors = F)


prueba <- f_union_sectores(datos_sectores = datos_prueba,
                           nom_sector_intermedio = "sector",
                           nom_tam_sector_intermedio = "Tam", 
                           nom_sectorFinal = "Sector_Final",
                           nom_Tam_sectorFinal = "Tam_SectorFinal",
                           restriccion_sectores = F,
                           umbral = 50)
 prueba$df_creacionSectores
 
 
