f_restricciones <- function(df){
  df <- df$microdata[[1]]
  # renombramos las variables que usamos, para mas claridad
  df <- df %>% 
        rename(wage = P21,  ubicacion = AGLOMERADO, est_civil = CH07, edad = CH06)
  
  df <- df %>%
  filter(wage > 0,        # Asalariados
         CH03 == 01,      # Jefes de hogar
         CH04 == 1,       # Varones
         edad >= 25,      # Edad mínima
         edad <= 65)      # Edad maxima 
  
  # calculamos anios de educacion (anios_edu), experiencia potencial (exp_pot)
  anios_prev <- c(0, 0, 6, 6, 9, 12, 12, 18)
  anios_post <- c(0, 6, 9, 12, 12, 15, 18, 22)
  
  df <- df %>%
  mutate(
    # Asignar el número de años en base al nivel de educación y condición de finalización
    anios_edu = ifelse(CH13 == 1, # finalizo ese nivel ? 
                      anios_post[CH12 + 1],  # +1 para ajustar al índice en R
                      anios_prev[CH12 + 1] + as.numeric(CH14)),
    
    # Calcular la experiencia potencial y su cuadrado
    exp_pot = edad - anios_edu - 6,
    exp_pot_2 = exp_pot^2
  )
  
  # Filtrado
  df <- df[complete.cases(df[, c("exp_pot")]), ] # nos quitamos los na's de la variable "exp_pot"
  df <- df[! (df$anios_edu > 100),] # [1]
  return(df)
}

# funcion auxiliar, devuelve la suma de sarialos, o la cantidad de elementos.
faux <- function(wage,bool){
  if(bool){
    return(sum(wage))
  }else{
    return(length(wage))
  }
}

# Estamos haciendo la "particion" por Region 
Descomp <- function(wage,subgrupo){
  if (!is.numeric(wage) || !is.factor(subgrupo)) {
    stop("El wage debe ser numérico y REGION debe ser un factor.")
  }
  # Calcular el índice de Theil para cada grupo manualmente
  Ts <- tapply(wage, subgrupo, Theil)
  
  # Calcular sum(wage) por Region 
  sums <- tapply(wage,subgrupo, function(w) faux(w,TRUE))
  
  # Calcular len(wage) por Region 
  ns <- tapply(wage,subgrupo, function(w) faux(w,FALSE))
  
  mu_n <- sum(sums)     
  n <- sum(ns)
  mu <- mu_n / sum(ns)
  
  within_ineq <- sum(sums*Ts / mu_n) 
  between_ineq <- sum(sums*log((sums/ns) / mu) / mu_n)
  
  return(c(within_ineq,between_ineq))
}
