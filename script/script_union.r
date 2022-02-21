

# Prototipo básico

f_unionesUPM <- function(df, id_upm, n_upm, umbral){
  
  vctr_upm <- df[[id_upm]] 
  vctr_size <- df[[n_upm]]


  ####################### Inicialización de objetos #######################
  n_upm <- length(vctr_upm)

  vctr_cumSize <- numeric(n_upm)
  vctr_cumPaste <- numeric(n_upm)
  vctr_seqUnion <- numeric(n_upm)


  ####################### Caso 1 #######################
  vctr_cumSize[1] <- vctr_size[1]
  vctr_cumPaste[1] <- vctr_upm[1]
  vctr_seqUnion[1] <- 1

####################### Ciclo #######################

  for(i in 2:n_upm){
    if(vctr_cumSize[i-1] + vctr_size[i] <= umbral){
      vctr_cumSize[i] <-  vctr_cumSize[i-1] + vctr_size[i]
      vctr_cumPaste[i] <-  paste0(vctr_cumPaste[i-1], "_", vctr_upm[i])
      vctr_seqUnion[i] = vctr_seqUnion[i-1]
    } else {
        vctr_cumSize[i] = vctr_size[i]
        vctr_cumPaste[i] = vctr_upm[i]
        vctr_seqUnion[i] =  vctr_seqUnion[i-1] + 1
   }
 }

  resultado_temp <- df
  resultado_temp$cumSize <- vctr_cumSize
  resultado_temp$cumPaste <- vctr_cumPaste
  resultado_temp$seqUnion <- vctr_seqUnion


  df_resumen_uniones <- resultado_temp %>% group_by(seqUnion) %>% summarise(union_upm = max(cumPaste),
                                            size_upm = max(cumSize))


  df_upmUniones <- resultado_temp %>% group_by(seqUnion) %>%
    mutate(union_upm = max(cumPaste),
           size_upm = max(cumSize)) %>%
    select(-cumSize,  -cumPaste,)

  lst_resultado <- list(df_resumen_uniones, df_upmUniones)
  names(lst_resultado) <- c("df_resumen_uniones", "df_upmUniones")
  return(lst_resultado)
}


# Toy example prototipo básico
df_prueba <- data.frame(upm = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K"),
                        n = c(130, 15, 20, 50, 60, 30, 85, 5,
                              130, 6, 140))

f_unionesUPM(df = df_prueba, id_upm = "upm", n_upm = "n", umbral = 125)
