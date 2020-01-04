setwd("~/Documents/Maestria/2 Cuatrimestre/Data Warehouseing BI/Unidad 4/Data")

hogares <- read.table("hogares.csv", header = TRUE, sep = ",")
entidades <- read.table("entidades.csv", header = TRUE, sep = ",")
gasto <-read.table("gasto_agua.csv", header = TRUE, sep = ",")
integrantes <- read.table("integrantes.csv", header = TRUE, sep = ",")

names_entidades <- read.table("entidades_ids.csv", header = TRUE, sep = ",")
main_table <- read.table("tablaConcentrado.csv", header = TRUE, sep = ",")
entidad_gasto <- read.table("entidad_gasto.csv", header = TRUE, sep = ",")
consumo_hogares <- read.table("hogares_consumo.csv", header = TRUE, sep = ",")

head(entidad_gasto)

summary(main_table$gasto_agua_tot)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
#    0.25   170.00   300.00   467.55   540.00 39000.00    21353 

summary(main_table$tot_integ)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1.000   2.000   3.000   3.604   5.000  22.000 

summary(main_table$hombres)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.000   1.000   2.000   1.764   2.000  11.000

summary(main_table$mujeres)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.000   1.000   2.000   1.841   2.000  12.000 

str(main_table)
# 74647 obs. of  14 variables:

# Replace NA with 0 for correlation analisys
entidad_gasto$gasto_agua_tot[is.na(entidad_gasto$gasto_agua_tot)] <- 0

### Correlacion ###
cor(x = entidad_gasto$gasto_agua_tot, y = entidad_gasto$tot_integ)
# 0.0421 
# MUY BAJA, CASI NULA. NO EXISTE UNA RELACION ENTRE EL GASTO DE AGUA Y LOS INTEGRANTES DE LA FAMILIA

cor(x = consumo_hogares$hogares, y = consumo_hogares$consumo)
#0.9973
# MUY ALTA. EXISTE UNA GRAN RELACION ENTRE EL NUMERO DE HOGARES Y EL CONSUMO.
# ENTRE MAS HOGARES, MAYOR EL CONSUMO

##### Data visualization #####
library(ggplot2)
library(plyr)

# Numero de hogares por numero de integrantes
ggplot(data = na.omit(entidad_gasto)) +
  geom_bar(mapping = aes(x = tot_integ)) +
  labs(
    title = "Cantidad de hogares segun el numero de integrantes",
    x = "Integrantes por familia",
    y = "Cantidad de hogares"
  )

# Relacion entre Entidad Federativa vs Consumo de agua
ggplot(data = na.omit(entidad_gasto)) +
  geom_bar(
    mapping = aes(x = entidad, y = (gasto_agua_tot / 1000), fill = entidad), stat = "identity"
    ) +
  labs(
    title = "Gasto trimestral de agua por Entidad Federativa",
    x = "Entidad Federativa",
    y = "Gasto trimestral (litros x 1000)"
  ) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Relacion entre Gasto y Numero de integrantes en el hogar por Entidad
ggplot(data = na.omit(entidad_gasto)) +
  geom_point(
    mapping = aes(x = tot_integ, y = (gasto_agua_tot), color = entidad)
  ) +
  facet_wrap(~ entidad) +
  labs(
    title = "Gasto trimestral de agua por integrantes de familia por entidad",
    x = "Integrantes",
    y = "Gasto trimestral (litros x 1000)"
  ) 

# Regresion lineal
ggplot(data = na.omit(entidad_gasto), mapping = aes(x = tot_integ, y = gasto_agua_tot)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Gasto trimestral de agua por integrantes de familia",
    x = "Integrantes",
    y = "Gasto trimestral (litros)"
  ) 


ggplot(data = na.omit(consumo_hogares), mapping = aes(x = hogares, y = consumo / 1000)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Gasto trimestral de agua por hogares",
    x = "Hogares",
    y = "Gasto trimestral (litros x 1000)"
  ) 

ggplot(data = na.omit(consumo_hogares)) +
  geom_point(mapping = aes(x = hogares, y = consumo / 1000, size = integrantes)) +
  labs(
    title = "Gasto trimestral de agua por hogares",
    x = "Hogares",
    y = "Gasto trimestral (litros x 1000)"
  ) 
