#### INDICES ####
############


# Los índices son medidas utilizadas para resumir o representar la información contenida en un conjunto de variables. 
# Son útiles para reducir la complejidad de los datos y facilitar la interpretación.

# ------------------------------ 
# IDH (Indice de desarrollo humano) 
# ------------------------------ 
# El Índice de Desarrollo Humano (IDH) es una medida resumida del logro promedio en dimensiones clave del desarrollo 
# humano: una vida larga y saludable, un nivel de vida digno y un nivel de vida adecuado. 
# El IDH es la media geométrica de los índices normalizados para cada una de las tres dimensiones:
# - La dimensión de salud se evalúa mediante la esperanza de vida al nacer
# - La dimensión de educación, mediante la media de los años de escolarización para adultos de 25 años o más y 
#   la expectativa de escolarización para niños en edad escolar
# - La dimensión del nivel de vida se mide mediante el ingreso nacional bruto per cápita.

# https://hdr.undp.org/data-center/human-development-index#/indicies/HDI

# Cada dimension o variable se debe expresar en una escala de 0 a 1.
# Se eligen los valores min y max para cada uno:
# Indice de dimension = (valor_real - valor_min)/(valor_max - valor_min)
# IDH se calcula con el promedio simple de los indices de las dimensiones

# ------------------------------ 
# Ejercicio: Indices MUJER
# Maternidad: Mortalidad infantil, embarazo adolescente, tasa global de fecundidad (TGF), Educación preescolar y 
#             %Papanicolaou 
# Calidad de Vida: Prevalencia de estrés, tasa de femicidios, tasa VIF, tasa de victimas de delitos, contaminación (MP10)
# Empoderamiento: Indigencia, Participación laboral, Salario promedio, brecha salarial, nivel de escolaridad, 
#                 porcentaje en puestos de gerentes, directivos y porcentaje en cargos políticos


base <- rio::import("RankingMujer.xlsx")
attach(base)  
head(base,5)

# 1. Necesitamos "escalar" segun IDH (0 a 1) cada variable
# por ejemplo: Papanicolau
## HV: esta es una forma de escalarlo.. se esta haciendo una prueba..
prueba1 <- (Papanicolau-min(Papanicolau)) / (max(Papanicolau) - min(Papanicolau))
summary(prueba1) ## la variable queda escalado entre 0 y 1


#install.packages("datawizard")
library(datawizard) ### HV: A contuación se reescala con la función rescale
prueba2 <- rescale(Papanicolau, to = c(0, 1))
summary(prueba2)

# OJO! No confundir con la función scale
# scale(): estandarización Z -> Transforma los datos para que tengan una media de 0 y una desviación estándar de 1.
# rescale(): escala los datos a un rango específico, por defecto de 0 a 1

base_norm <- rescale(base,to = c(0, 1)) ## HV: rescalar toda la base
attach(base_norm)
summary(base_norm)

# 2. Construimos las variables normalizados  (Indices) para cada dimension

# Indice de Maternidad
#            directas:  Preescolar, Papanicolau, TGFecund
#            inversas: EmbAdoles, TMortInfantil

# (1-variable) --> afectan

base_norm$Maternidad <- (Preescolar+Papanicolau+TGFecund+(1-EmbAdoles)+(1-TMortInfantil))/5

# Indice de Calidad de Vida
#           directas:  Ninguna!
#           inversas: MP10, TVictTot, TEstrés, TFeminic, TVIntraf

base_norm$CVida <- ((1-MP10)+(1-TVictTot)+(1-TEstrés)+(1-TFeminic)+(1-TVIntraf))/5

# Indice de Empoderamiento
#           directa: FuerzaLaboral, SueldoProm, Escolarid, Puestos, Politicos
#           inversas: Pindigencia, BrechaSueld

base_norm$Empod <- (FuerzaLaboral+SueldoProm+Escolarid+Puestos+Politicos+(1-Pindigencia)+(1-`Brecha sueld`))/7

# Resumen por región
attach(base_norm)
cbind(REGION,cbind(Maternidad,CVida,Empod))



# Gráficos "de a par": Maternidad vs CVida

library(ggplot2)
ggplot(base_norm, aes(x = Maternidad, y = CVida, label = REGION)) +
  geom_point(color = "darkred", size = 3) +                  # puntos
  geom_text(vjust = -0.5, color = "blue", size = 3) +        # nombres de región
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "gray50") + # línea vertical referencia
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray50") + # línea horizontal referencia
  scale_x_continuous(limits = c(0.2, 1)) +                   # límites del eje
  scale_y_continuous(limits = c(0.3, 1)) +
  labs(title = "Relación entre Maternidad y Calidad de Vida",x = "Índice de Maternidad",y = "Índice de Calidad de Vida") +
  theme_minimal(base_size = 13)

# Ambos ejes van de 0 (peor) a 1 (mejor).
# Eje X (Maternidad): mide mejores condiciones reproductivas y de atención (más educación preescolar, más papanicolaou, 
# menor mortalidad infantil y embarazo adolescente).
# Eje Y (Calidad de Vida): mide condiciones sociales y ambientales seguras (menos violencia, estrés y contaminación).

# Cuadrante superior-derecho (++): Buen desempeño en ambas dimensiones — "mejores regiones"
# Cuadrante superior-izquierdo (+-): Buena calidad de vida, pero peores indicadores de maternidad.
# Cuadrante inferior-derecho (-+): Buen desempeño en maternidad, pero baja calidad de vida.
# Cuadrante inferior-izquierdo (--): Mal desempeño general - "peores regiones"


# Gráficos "de a par": Maternidad vs Empoderamiento
ggplot(base_norm, aes(x = Maternidad, y = Empod, label = REGION)) +
  geom_point(color = "darkgreen", size = 3) +
  geom_text(vjust = -0.5, color = "blue", size = 3) +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "gray50") +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray50") +
  scale_x_continuous(limits = c(0.2, 1)) +
  scale_y_continuous(limits = c(0, 0.8)) +
  labs(title = "Relación entre Maternidad y Empoderamiento",x = "Índice de Maternidad",y = "Índice de Empoderamiento") +
  theme_minimal(base_size = 13)

# Aquí el eje Y mide autonomía económica y política (empleo, salario, cargos directivos, etc.).


# Indice global
indice <- (Maternidad+CVida+Empod)/3
listado <- cbind(REGION,round(indice*100,1))
colnames(listado) <- c("Region","Ptje")   #asignamos nombres
list <- as.data.frame(listado)            #indicamos que es una "base"
list <- list[order(list$Ptje), ]  # ordenar de menor a mayor
list

ggplot(list, aes(x = factor(Region, levels = Region), y = Ptje)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = Ptje), hjust = -0.2, size = 3.5) + #texto por barra
  coord_flip() +
  labs(title = "Ranking Global del Índice de Desarrollo de la Mujer",x = "Región",y = "Puntaje (0-100)") +
  theme_minimal(base_size = 13)