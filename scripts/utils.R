#variables = c("P21","AGLOMERADO","CH07","CH06","CH03","CH04","CH13","CH12","CH14","NIVEL_ED","PP3E_TOT","CODUSU","NRO_HOGAR")

datos <- function(anio,f_i){
   bases      <- list()
   bases_rest <- list()
   hogares    <- list()
   for(i in 1:f_i){
     b <- get_microdata(year = anio, period = i, type = "individual", 
                        destfile = paste0('Data/EPH_', anio, '_', i, '_individual.zip')) 
     b_f <- f_restricciones(b$microdata[[1]],gender = "ambos",asalariado = FALSE)
     h <- get_microdata(year = anio, period = i, type = "hogar",
                        destfile = paste0('Data/EPH_', anio, '_', i, '_hogar.zip'))
     
     hogares[[i]]    <- h$microdata[[1]]
     bases[[i]]      <- b$microdata[[1]]
     bases_rest[[i]] <- b_f
   }
   return(list(bases = bases, bases_rest = bases_rest, hogares = hogares))
}

# calculamos anios de educacion (anios_edu), experiencia potencial (exp_pot)
anios_prev <- c(0, 0, 6, 6, 9, 12, 12, 18)
anios_post <- c(0, 6, 9, 12, 12, 15, 18, 22)

# Restricciones para data.frame
f_restricciones <- function(df, gender, asalariado){
  # renombramos las variables que usamos, para mas claridad
  
  # CH03  --> jefe de hogar.
  # CH14 --> cual fue el ultimo anio que aprobo.
  # CH12 --> nivel mas alto alcanzado.
  # CH13 --> termino este nivel.
  # genero  ~~ CH04 | 1 -> masculino | 2 -> femenino
  
  if(gender == "mas"){
    df <- df[df$CH04 == 1,]
  }else if(gender == "fem"){
    df <- df[df$CH04 == 2,]
  }else if(gender == "ambos"){
  }else{
    warning("El valor de gender no es valido")
  }
  
  if(asalariado){
    df <- df[df$P21 > 0,]
  }
  
  df <-df %>% 
    rename(ingreso = P21,  ubicacion = AGLOMERADO, est_civil = CH07, edad = CH06, gender = CH04, horas_semanales  = PP3E_TOT) %>%
    
    filter(
           edad >= 25,      # Edad mínima
           edad <= 65,      # Edad maxima
           NIVEL_ED != 7
          ) %>%
               
    mutate(
      # Asignar el número de años en base al nivel de educación y condición de finalización
      anios_edu = ifelse(CH13 == 1, # finalizo ese nivel ? 
                         anios_post[CH12 + 1],  # +1 para ajustar al índice en R
                         anios_prev[CH12 + 1] + as.numeric(CH14)),
      # Calcular la experiencia potencial y su cuadrado
      exp_pot = ifelse( ingreso > 0 & edad - anios_edu - 6 < 0, 
                        1,
                        ifelse( ingreso <= 0 & edad - anios_edu - 6 < 0, 
                                0,
                                edad - anios_edu - 6)
                      ),
      exp_pot_2 = exp_pot ^ 2,
      edad_2 = edad ^ 2,
      horas_mensuales = (365 / 12) * horas_semanales / 7 ,  
      ingreso_hora = ifelse(ingreso > 0, 
                            ingreso / horas_mensuales , 
                            0), 
      log_horas = ifelse(horas_mensuales == 0, 
                         0, 
                         log(horas_mensuales))
    ) %>%
    
    select(
      ingreso, ingreso_hora, exp_pot, exp_pot_2, edad, edad_2, anios_edu, ubicacion, est_civil, NIVEL_ED, CODUSU, NRO_HOGAR, gender, horas_semanales, horas_mensuales, log_horas, PP04C
    )
  
  df <- df[! is.na(df$exp_pot), ] # quitamos los NaN's de la variable "exp_pot"
  df <- df[! (df$anios_edu > 100),] # quitamos los valores de anios educativos mayores a 100, corresponden a personas con educacion especial o no sabe no responde.
  df <- df[!(df$horas_semanales > 168), ] # no pueden trabajar mas del total de horas en la semana.
  df <- df[! is.na(df$horas_semanales), ] 
  return(df)
}

mergear <- function(df_personas_gender, df_hogar, df_personas){
   df_personas <- df_personas %>%
     filter(
       P21 > 0, # asalariado 
       CH03 == 01 # jefe de familia
     ) %>%
     rename(
       ingreso_jf = P21 # ingreso jefe de familia
     ) %>%
     select(
       CODUSU, NRO_HOGAR, ingreso_jf
     )
   # ADECIFR --> No de decil de ingreso total del hogar del aglomerado
   df_hogar <- df_hogar %>%
     rename(
      cant_integrantes = IX_TOT
     ) %>%
     select(
       CODUSU, NRO_HOGAR, cant_integrantes, ADECIFR
     )
   
   res <- left_join(df_hogar, df_personas, by = c('CODUSU','NRO_HOGAR'))
   df_personas_gender <- left_join(df_personas_gender, res, by = c('CODUSU','NRO_HOGAR'))

   df_personas_gender <- df_personas_gender %>%
     mutate(estrato = case_when(
       as.numeric(ADECIFR) %in% 1:3 ~ 1,  # Deciles 1 a 3
       as.numeric(ADECIFR) %in% 4:9 ~ 2,  # Deciles 4 a 9
       as.numeric(ADECIFR) == 10    ~ 3   # Decil 10
     ),
     empleado = ifelse(ingreso > 0, 1, 0)
     )   
   
   df_personas_gender <- df_personas_gender[complete.cases(df_personas_gender[, c("ingreso_jf","estrato")]), ]
   
   rm(res)
   return(df_personas_gender)
}

# funcion auxiliar, devuelve la suma de sarialos, o la cantidad de elementos.
faux <- function(wage,bool){
  if(bool){
    return(sum(wage))
  }else{
    return(length(wage))
  }
}

# Estamos haciendo la "particion" por subgrupos --- Theil ---- 
Descomp_Theil <- function(df1,df2,subgrupo){
  x <- c()
  w <- c()
  b <- c() 
  
  for (i in 1:4){
    d <- df1[[i]][df1[[i]]$ingreso>0,]
    wage <- d$ingreso
    seg <- d[[subgrupo]]
    
    Ts <- tapply(wage, seg, Theil)
    
    # Calcular sum(wage) por ubicacion 
    sums <- tapply(wage,seg, function(w) faux(w,TRUE))
    
    # Calcular len(wage) por ubicacion 
    ns <- tapply(wage,seg, function(w) faux(w,FALSE))
    
    mu_n <- sum(sums)     
    n <- sum(ns)
    mu <- mu_n / n
    
    within_ineq <- sum(sums*Ts / mu_n) 
    between_ineq <- sum(sums*log((sums/ns) / mu) / mu_n)
    Te <- within_ineq + between_ineq
    x <- c(x,Te)
    w <- c(w,within_ineq)
    b <- c(b,between_ineq)
  }
  for(i in 1:2){
    d <- df2[[i]][df2[[i]]$ingreso>0,]
    wage <- d$ingreso
    seg <- d[[subgrupo]]
    
    Ts <- tapply(wage, seg, Theil)
    
    # Calcular sum(wage) por ubicacion 
    sums <- tapply(wage,seg, function(w) faux(w,TRUE))
    
    # Calcular len(wage) por ubicacion 
    ns <- tapply(wage,seg, function(w) faux(w,FALSE))
    
    mu_n <- sum(sums)     
    n <- sum(ns)
    mu <- mu_n / sum(ns)
    
    within_ineq <- sum(sums*Ts / mu_n) 
    between_ineq <- sum(sums*log((sums/ns) / mu) / mu_n)
    Te <- within_ineq + between_ineq
    x <- c(x,Te)
    w <- c(w,within_ineq)
    b <- c(b,between_ineq)
  }
  return(list(x=x,w=w,b=b))
}

# Heckman-Method
Heckman_method <- function(df){
  
  # Modelo probit para la selección
  probit_model <- glm(empleado ~ edad + edad_2 + as.factor(NIVEL_ED) + cant_integrantes + ingreso_jf + as.factor(estrato) + as.factor(est_civil),
                      family = binomial(link = "probit"), data = df)
  
  # Resumen del modelo
  summary(probit_model)
  
  # Predecir el índice lineal z del modelo probit
  df$z_hat <- predict(probit_model, type = "link")
  
  # Calcular phi(z) y Phi(z)
  df$phi_z <- dnorm(df$z_hat)    # Densidad normal estándar
  df$Phi_z <- pnorm(df$z_hat)    # Función acumulada normal estándar
  
  # Calcular lambda (inverso de Mills)
  df$lambda <- df$phi_z / df$Phi_z
  
  return(df)
}

# transformacion que permite una interpretabilidad a la diferencia de medias log(ingreso) 
# porcentaje de la diferencia entre d1 y d2.
porce <- function(d1, d2 = NULL) {
  # D1 y D2 deben estar en orden correcto, para mi analisis seria: hombres -> d1, mujeres -> d2.
  res <- 0 
  if (missing(d2)) {
    res <- exp(d1) - 1
  } else {
    res <- exp(mean(log(d1)) - mean(log(d2))) - 1
  }
  return(res * 100)
}

# Graficos --------------------------------------------------------------------------------------------------------------------------------------------

x <- c("Primario\ncompleto", "Secundario\nincompleto", "Secundario\ncompleto", "Universitario\nincompleto", "Universitario\ncompleto")

# Efecto de NIVEL_ED en lwage --- no linearidad ---- 
graph_nivel_ed <- function(y,pv){
  #y <- coef_interes_reg2[,1]
  #pv <- coef_interes_reg2[,4]
  png("graphs/nivel_ed_nl.png", width = 10 * 72, height = 5 * 72)#  Para guardar grafico 
  # Crear el gráfico base
  plot(1:length(x), y, type = "b", pch = 19, col = "blue", xaxt = "n", 
      xlab = "", ylab = "Coeficiente estimado", 
      xlim = c(0, length(x) + 1),
      main = "Efecto del Nivel Educativo en el Salario")

  # Añadir una línea horizontal en y = 0 para referencia
  abline(h = 0, col = "red", lty = 2)

  # Añadir las etiquetas del eje X inclinadas
  axis(1, at = 1:length(x), labels = FALSE)  # Ocultar etiquetas predeterminadas
  text(1:length(x), par("usr")[3] - 0.05,  # Ajuste para desplazar las etiquetas
      labels = x, srt = 45, adj = 1, xpd = TRUE, cex = 0.8)

  # Filtrar los valores válidos para evitar NA en los p-valores
  valid_indices <- which(!is.na(y))

  # Añadir los p-valores junto a sus respectivos puntos en el gráfico
  text(valid_indices, y[valid_indices], 
      labels = paste("p-valor", format(pv[valid_indices], digits = 2)), 
      pos = 4, cex = 0.8, col = "darkgreen")
  
  dev.off() # para guardar grafico
}

graph_nivel_ed_comparacion <- function(y, pv, y2, pv2){
  #png("graphs/nivel_ed_comp.png", width = 10 * 72, height = 5 * 72)#  Para guardar grafico 
  # Crear el gráfico base con la primera serie (línea azul)
  plot(1:length(x), y, type = "b", pch = 19, col = "blue", xaxt = "n", 
      xlab = "", ylab = "Coeficiente estimado", 
      xlim = c(0, length(x) + 1), ylim = c(min(y,y2) - .01, max(y,y2) + .01),
      main = "Efecto del Nivel Educativo en el Salario")
   
  # Añadir la segunda serie (línea roja)
  lines(1:length(x), y2, type = "b", pch = 17, col = "red")
  
  # Añadir una línea horizontal en y = 0 para referencia
  abline(h = 0, col = "red", lty = 2)
  
  # Añadir las etiquetas del eje X inclinadas
  axis(1, at = 1:length(x), labels = FALSE)  # Ocultar etiquetas predeterminadas
  text(1:length(x), par("usr")[3] - 0.05,  # Ajuste para desplazar las etiquetas
      labels = x, srt = 45, adj = 1, xpd = TRUE, cex = 0.8)
  
  # Filtrar los valores válidos para evitar NA en los p-valores
  valid_indices1 <- which(!is.na(y))
  valid_indices2 <- which(!is.na(y2))
  
  # Añadir los p-valores junto a sus respectivos puntos en el gráfico
  text(valid_indices1, y[valid_indices1], 
      labels = paste("p=", format(pv[valid_indices1], digits = 2)), 
      pos = 4, cex = 0.8, col = "darkgreen")
  
  text(valid_indices2, y2[valid_indices2], 
      labels = paste("p=", format(pv2[valid_indices2], digits = 2)), 
      pos = 4, cex = 0.8, col = "darkred")
  
  # Añadir una leyenda para diferenciar ambas series
  legend("topright", legend = c("Hombres", "Mujeres"), 
      col = c("blue", "red"), pch = c(19, 17), lty = 1, cex = 0.8)

  #dev.off() 
}

# Comparacion boxplot y histogram 
comparacion <- function(df){
  vector1 <-log((df[[1]] %>% filter(gender == 1, ingreso >0))$ingreso)
  vector2 <-log((df[[1]] %>% filter(gender == 2, ingreso >0))$ingreso)
  
  hist(vector1, col=rgb(0, 0, 1, 0.5), xlim=c(min(c(vector1, vector2)), max(c(vector1, vector2))), 
       main="Comparación con histogramas", xlab="log(ingreso)", ylab="Frecuencia", 
       border="blue", breaks=20)
  hist(vector2, col=rgb(1, 0, 0, 0.5), add=T, border="red", breaks=20)
  
  # Agregar leyenda
  legend("topright", legend=c("Hombres", "Mujeres"), fill=c(rgb(0, 0, 1, 0.5), rgb(1, 0, 0, 0.5)))
  
  boxplot(vector1, vector2, 
          names = c("Hombres", "Mujeres"), 
          col = c("skyblue", "salmon"), 
          main = "Comparación con Boxplots", 
          ylab = "log(ingreso)")
}

# Definir los años y períodos
anios <- c("2023-1er","2023-2do","2023-3er","2023-4to","2024-1er","2024-2do")

# Theil 
graph_theil <- function(x, w, b, group = "Género") {
  #png(paste("Graphs/ind_theil_",group,".png"), width = 10 * 72, height = 5 * 72)
  plot(x, type = "o", col = "blue", pch = 16, ylab = "Índice Theil", xlab = "Año - Trimestre",
       main = paste("Theil en el tiempo, agrupando por",group), ylim = c(0, 0.35), xaxt= "n")
  
  # Agregar etiquetas personalizadas en el eje X
  axis(1, at = 1:length(anios), labels = anios)
  
  # Añadir el segundo conjunto de datos con líneas
  lines(w, type = "o", col = "orange", pch = 16)
  
  # Añadir el tercer conjunto de datos con líneas
  lines(b, type = "o", col = "darkgray", pch = 16)
  
  # Mostrar los valores sobre cada punto en el gráfico
  text(1:length(x), x, labels = round(x, 3), pos = 3, cex = 0.8, col = "blue")       # Theil
  text(1:length(w), w, labels = round(w, 3), pos = 3, cex = 0.8, col = "orange")     # Within
  text(1:length(b), b, labels = round(b, 3), pos = 3, cex = 0.8, col = "darkgray")   # Between

  # Añadir leyenda
  legend("topleft", legend = c("Theil", "Within", "Between"), 
         col = c("blue", "orange", "darkgray"), 
         pch = 16, lty = 1, cex = 0.7)
  #dev.off()
}

# Gini
graph_gini <- function(ambos, mas, fem) {
  y_min <- min(c(ambos, mas, fem)) - 0.01
  y_max <- max(c(ambos, mas, fem)) + 0.01
  #png("Graphs/ind_gini.png", width = 10 * 72, height = 5 * 72)
  # Graficar el primer grupo (ambos)
  plot(ambos, type = "o", col = "black", pch = 16, ylab = "Índice Gini", 
       xlab = "Año - Trimestre", main = "Gini en el tiempo", xaxt= "n", ylim = c(y_min, y_max))
  
  # Agregar las líneas de los otros dos grupos
  lines(mas, type = "o", col = "blue", pch = 17)
  lines(fem, type = "o", col = "red", pch = 18)
  
  # Personalizar los ejes x con las etiquetas de los años
  axis(1, at = 1:length(anios), labels = anios)
  
  # Agregar los valores del índice de Gini en cada punto
  text(1:length(ambos), ambos, labels = round(ambos, 3), pos = 3, cex = 0.8, col = "black")
  text(1:length(mas), mas, labels = round(mas, 3), pos = 3, cex = 0.8, col = "blue")
  text(1:length(fem), fem, labels = round(fem, 3), pos = 3, cex = 0.8, col = "red")

  # Agregar una leyenda para los grupos
  legend("topleft", legend = c("Ambos", "Masculino", "Femenino"), 
         col = c("black", "blue", "red"), pch = c(16, 17, 18), lty = 1, cex = 0.7)
  #dev.off()
}


graph_RIF <- function(mas,fem){
  y_min <- min(c(mas, fem)) - 0.0001
  y_max <- max(c(mas, fem)) + 0.0001

  # Graficar el primer grupo (ambos)
  plot(mas, type = "o", col = "blue", pch = 16, ylab = "Estimacion del coeficiente", 
      xlab = "Año - Periodo", main = "Influencia de Años de educación en Gini", xaxt= "n", ylim = c(y_min, y_max))

  # Agregar las líneas de los otros dos grupos
  #lines(mas, type = "o", col = "blue", pch = 17)
  lines(fem, type = "o", col = "red", pch = 18)

  # Personalizar los ejes x con las etiquetas de los años
  axis(1, at = 1:length(anios), labels = anios)

  # Agregar los valores del índice de Gini en cada punto
  #text(1:length(ambos), ambos, labels = round(ambos, 4), pos = 3, cex = 0.8, col = "black")
  text(1:length(mas), mas, labels = round(mas, 4), pos = 3, cex = 0.8, col = "blue")
  text(1:length(fem), fem, labels = round(fem, 4), pos = 3, cex = 0.8, col = "red")

  # Agregar una leyenda para los grupos
  legend("topleft", legend = c("Masculino", "Femenino"), 
        col = c( "blue", "red"), pch = c(16, 17, 18), lty = 1, cex = 0.7)
}

# barplots 

## nivel edu y tam 
NT_aux <- function(df){
  porcent <- c()
  t <- c(1,6,7,9,10,11)
  
  for(i in c(1,3,5)){
    d <- df %>% filter(t[i] <= PP04C,PP04C <= t[i+1])
    porcent <- c(porcent, 
                  c(d %>% filter(1 <= NIVEL_ED, NIVEL_ED <= 3) %>% nrow(),
                    d %>% filter(4 == NIVEL_ED) %>% nrow(),
                    d %>% filter(5 == NIVEL_ED) %>% nrow(), 
                    d %>% filter(6 == NIVEL_ED) %>% nrow() ) / d %>% nrow()
                )
  }
  
  d <- df %>% filter(PP04C == 12) #7-9  10-11  12
  porcent <- c(porcent, 
                c(d %>% filter(1 <= NIVEL_ED, NIVEL_ED <= 3) %>% nrow(),
                  d %>% filter(4 == NIVEL_ED) %>% nrow(),
                  d %>% filter(5 == NIVEL_ED) %>% nrow(), 
                  d %>% filter(6 == NIVEL_ED) %>% nrow() ) / d %>% nrow()
              )
  
  porcent <- porcent * 100
  return(porcent)
}

niveled_tam <- function(df){
  # Datos de ejemplo
  datos <- data.frame(
    nivel_ed = rep(c("Hasta Secundario incompleto", "Secundario completo", "Universitario incompleto", "Universitario completo"), each = 4),
    Sector = rep(c("HASTA 10 PERSONAS", "DE 11 A 100 PERSONAS", "DE 101 A 500 PERSONAS", "MAS DE 500 PERSONAS"), times = 4),
    Porcentajes = NT_aux(df)  
  )

  # Modificar los nombres de las categorías con saltos de línea
  datos$nivel_ed <- gsub(" ", "\n", datos$nivel_ed)

  # Definir el orden deseado de los sectores y categorías
  orden_cant <- c("HASTA 10 PERSONAS", "DE 11 A 100 PERSONAS", "DE 101 A 500 PERSONAS", "MAS DE 500 PERSONAS")
  orden_ed <- c("Hasta\nSecundario\nincompleto", "Secundario\ncompleto", "Universitario\nincompleto", "Universitario\ncompleto")

  # Convertir las columnas Sector y Categoria en factores con el orden especificado
  datos$Sector <- factor(datos$Sector, levels = orden_cant)
  datos$nivel_ed <- factor(datos$nivel_ed, levels = orden_ed)

  # Gráfico de barras agrupadas con colores personalizados
  g<-ggplot(datos, aes(x = nivel_ed, y = Porcentajes, fill = Sector)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +  # Ancho de las barras
    scale_fill_manual(values = c("HASTA 10 PERSONAS" = "#a6bddb",  # Colores personalizados
                                "DE 11 A 100 PERSONAS" = "#74a9cf",
                                "DE 101 A 500 PERSONAS" = "#2b8cbe",
                                "MAS DE 500 PERSONAS" = "#045a8d")) + 
    labs(title = "Nivel educativo por tamaño de la organización",
        x = "",
        y = "",
        fill = "") +
    theme_minimal() +
    theme(
          panel.grid.major = element_blank(), # Elimina las líneas de cuadrícula mayores
          panel.grid.minor = element_blank(), # Elimina las líneas de cuadrícula menores
          axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # Rotar etiquetas del eje X
          plot.title = element_text(hjust = 0.5) # Centra el título
          )
  print(g)
  return(g)
}

## ingreso y nivel edu 

IN_aux <- function(df){
  df <- df %>% filter(ingreso > 0)
  x <- mean(df$ingreso)

  porcent <- c(mean(( df %>% filter(1 <= NIVEL_ED, NIVEL_ED <= 2) )$ingreso),
              mean(( df %>% filter(3 == NIVEL_ED) )$ingreso),
              mean(( df %>% filter(4 == NIVEL_ED) )$ingreso),
              mean(( df %>% filter(5 == NIVEL_ED) )$ingreso),
              mean(( df %>% filter(6 == NIVEL_ED) )$ingreso)) / x 

  porcent <- (porcent - 1) * 100

  return(porcent)
}

IN_graph <- function(df){
  # Crear datos
  datos <- data.frame(
    Categoria = c("Primario completo","Secundario incompleto","Secundario completo","Universitario incompleto","Universitario completo"),
    Valor = IN_aux(df)
  )

  datos$Categoria <- factor(datos$Categoria, levels = c("Primario completo","Secundario incompleto","Secundario completo","Universitario incompleto","Universitario completo"))

  # Crear el barplot con color único sin fill
  g<-ggplot(datos, aes(x = Categoria, y = Valor)) +
    geom_bar(stat = "identity", width = 0.7, fill = "#023858", color = "black") +  # Color único y bordes
    labs(title = "Ingreso según Nivel educativo vs salario medio",
        x = "",
        y = "") +
    theme_minimal() +  # Estilo minimalista
    theme(
          panel.grid.major = element_blank(), # Elimina las líneas de cuadrícula mayores
          panel.grid.minor = element_blank(), # Elimina las líneas de cuadrícula menores
          axis.text.x = element_text(angle = 45, hjust = 1, size = 12), # Rotar etiquetas del eje X
          plot.title = element_text(hjust = 0.5)) # Centra el título  
  print(g)
  return(g)
}

## ingreso y cant 

IC_aux <- function(df){
  porcent <- c()
  df <- df %>% filter(ingreso > 0)
  x <- mean(df$ingreso)

  for(i in 1:12){
    porcent <- c(porcent,
      mean( (df %>% filter(PP04C == i))$ingreso ) / x
    )
  }

  porcent <- (porcent - 1) * 100

  return(porcent)
}

cant <- c("1 PERSONA","2 PERSONAS","3 PERSONAS","4 PERSONAS","5 PERSONAS","6 A 10 PERSONAS","11 A 25 PERSONAS","26 A 40 PERSONAS","41 A 100 PERSONAS","101 A 200 PERSONAS","201 A 500 PERSONAS","MAS DE 500 PERSONAS")
IC_graph <- function(df){

  datos <- data.frame(
    Categoria = cant,
    Valor = IC_aux(df)
  )

  datos$Categoria <- factor(datos$Categoria, levels = cant)

  # Crear el barplot con color único sin fill
  g <- ggplot(datos, aes(x = Categoria, y = Valor)) +
    geom_bar(stat = "identity", width = 0.7, fill = "#023858", color = "black") +
    labs(title = "Ingreso según tamaño de la organización vs salario medio",
         x = "",
         y = "") +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(), # Elimina las líneas de cuadrícula mayores
      panel.grid.minor = element_blank(), # Elimina las líneas de cuadrícula menores
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
      plot.title = element_text(hjust = 0.5) # Centra el título
    )
  print(g)
  return(g)
}
# RIF  --------------------------------------------------------------------------------------------------------------------------------------------

calculate_rif <- function(dmas_23,dfem_23,d_23, dmas_24,dfem_24,d_24){
  coefs_rif_edu_mas <- c()
  coefs_rif_edu_fem <- c()
  coefs_rif_edu <- c()
  #coefs_rif_exp <- c()
  
  for(i in 1:4){
    # calculamos RIF en funcion de lwage.
    rif_gini_mas <- rif(log(dmas_23[[i]]$ingreso[dmas_23[[i]]$ingreso >0]), method = "gini")
    rif_gini_fem <- rif(log(dfem_23[[i]]$ingreso[dfem_23[[i]]$ingreso >0]), method = "gini")
    rif_gini <- rif(log(d_23[[i]]$ingreso[d_23[[i]]$ingreso >0]), method = "gini")
    
    RIFreg_gini_mas <- lm(rif_gini_mas ~ anios_edu + exp_pot + exp_pot_2 + as.factor(PP04C) + log_horas, data = dmas_23[[i]] %>% filter(ingreso >0))
    RIFreg_gini_fem <- lm(rif_gini_fem ~ anios_edu + exp_pot + exp_pot_2 + as.factor(PP04C) + log_horas, data = dfem_23[[i]] %>% filter(ingreso >0))
    RIFreg_gini     <- lm(rif_gini     ~ anios_edu + exp_pot + exp_pot_2 + as.factor(PP04C) + log_horas, data = d_23[[i]] %>% filter(ingreso >0))
    
    edu1 <- RIFreg_gini_mas$coefficients[2]
    edu2 <- RIFreg_gini_fem$coefficients[2]
    edu3 <- RIFreg_gini$coefficients[2]
    #exp <- RIFreg_gini$coefficients[3]
    
    coefs_rif_edu_mas <- c(coefs_rif_edu_mas, edu1)
    coefs_rif_edu_fem <- c(coefs_rif_edu_fem, edu2)
    coefs_rif_edu <- c(coefs_rif_edu, edu3)
    #coefs_rif_exp <- c(coefs_rif_exp, exp)
  }
  
  for(i in 1:2){
    # calculamos RIF en funcion de lwage.
    rif_gini_mas <- rif(log(dmas_24[[i]]$ingreso[dmas_24[[i]]$ingreso >0]), method = "gini")
    rif_gini_fem <- rif(log(dfem_24[[i]]$ingreso[dfem_24[[i]]$ingreso >0]), method = "gini")
    rif_gini <- rif(log(d_24[[i]]$ingreso[d_24[[i]]$ingreso >0]), method = "gini")
    
    RIFreg_gini_mas <- lm(rif_gini_mas ~ anios_edu + exp_pot + exp_pot_2 + as.factor(PP04C) + log_horas, data = dmas_24[[i]] %>% filter(ingreso >0))
    RIFreg_gini_fem <- lm(rif_gini_fem ~ anios_edu + exp_pot + exp_pot_2 + as.factor(PP04C) + log_horas, data = dfem_24[[i]] %>% filter(ingreso >0))
    RIFreg_gini     <- lm(rif_gini     ~ anios_edu + exp_pot + exp_pot_2 + as.factor(PP04C) + log_horas, data = d_24[[i]] %>% filter(ingreso >0))
    
    edu1 <- RIFreg_gini_mas$coefficients[2]
    edu2 <- RIFreg_gini_fem$coefficients[2]
    edu3 <- RIFreg_gini$coefficients[2]
    
    coefs_rif_edu_mas <- c(coefs_rif_edu_mas, edu1)
    coefs_rif_edu_fem <- c(coefs_rif_edu_fem, edu2)
    coefs_rif_edu <- c(coefs_rif_edu, edu3)
  }
  return(list(coefs_rif_edu_mas=coefs_rif_edu_mas,coefs_rif_edu_fem=coefs_rif_edu_fem,coefs_rif_edu=coefs_rif_edu))
}



# Oaxaca-Blinder --------------------------------------------------------------------------------------------------------------------------------------------
Descomp_OaxacaBlinder <- function(base23,hogar23,base24,hogar24){

  aux <- f_restricciones(base23, gender = "fem", asalariado = FALSE)
  aux <- mergear(aux, hogar23, base23)
  fem_23 <- Heckman_method(aux) 
  
  aux <- f_restricciones(base23, gender = "mas", asalariado = FALSE)
  aux <- mergear(aux, hogar23, base23)
  mas_23 <- Heckman_method(aux) 
  
  aux <- f_restricciones(base24, gender = "fem", asalariado = FALSE)
  aux <- mergear(aux, hogar24, base24)
  fem_24 <- Heckman_method(aux) 
  
  aux <- f_restricciones(base24, gender = "mas", asalariado = FALSE)
  aux <- mergear(aux, hogar24, base24)
  mas_24 <- Heckman_method(aux) 
  
  fem_23 <- fem_23 %>% filter(ingreso >0) 
  mas_23 <- mas_23 %>% filter(ingreso >0) 
  
  fem_24 <- fem_24 %>% filter(ingreso >0) 
  mas_24 <- mas_24 %>% filter(ingreso >0) 
  
  fem_23$fem = 1; fem_24$fem = 1 
  mas_23$fem = 0; mas_24$fem = 0 
  data_combined23 <- rbind(fem_23, mas_23) ; data_combined24 <- rbind(fem_24, mas_24) 
  data_combined23$fem <- as.numeric(as.character(data_combined23$fem));  data_combined24$fem <- as.numeric(as.character(data_combined24$fem))
  
  oaxaca_result23 <- oaxaca(
    formula = log(ingreso) ~ anios_edu + exp_pot + exp_pot_2 + as.factor(PP04C) + log_horas + lambda | fem,
    data = data_combined23,
  )
  
  oaxaca_result24 <- oaxaca(
    formula = log(ingreso) ~ anios_edu + exp_pot + exp_pot_2 + as.factor(PP04C) + log_horas + lambda | fem,
    data = data_combined24,
  )
  return(list(oaxaca_result23=oaxaca_result23, oaxaca_result24=oaxaca_result24))
}




