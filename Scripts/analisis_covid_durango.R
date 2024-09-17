library(readr)

covidData <- read.csv("Data/Datos_Tarea2.csv")


library(dplyr)

covidDurango <- covidData %>% filter (ENTIDAD_UM == 10)

write_csv(covidData, "Exports/COVID19_2020_DURANGO.csv")


# transformación de variables
covidDurango <- covidDurango %>%
  mutate(SEXO = factor(SEXO, levels = c(1,2),
                       labels = c("Mujer", "Hombre")),
         TIPO_PACIENTE = factor(TIPO_PACIENTE, levels = c(1,2),
                                labels = c("Ambulatorio", "Hospitalizado")),
         DIABETES = factor(DIABETES, levels = c(1,2,97,98,99),
                           labels = c("Si", "No", "No aplica", "Se ignora", "No especificado")),
         OBESIDAD = factor(OBESIDAD, levels = c(1,2,97,98,99),
                           labels = c("Si", "No", "No aplica", "Se ignora", "No especificado")),
         UCI = factor(UCI, levels = c(1,2,97,98,99),
                      labels = c("Si", "No", "No aplica", "Se ignora", "No especificado")),
         INTUBADO = factor(INTUBADO, levels = c(1,2,97,98,99),
                           labels = c("Si", "No", "No aplica", "Se ignora", "No especificado")))

# Tabla de Frecuencia de la variable SEXO

tabla_sexo <- table(covidDurango$SEXO)

tabla_sexo_df <- as.data.frame(tabla_sexo)

tabla_sexo_porcentajes <- prop.table(tabla_sexo) * 100



# Diagrama de Barras de la variable TIPO_PACIENTE
tabla_tipo_paciente_df <- as.data.frame(table(covidDurango$TIPO_PACIENTE))

library(ggplot2)

tipo_paciente <- ggplot(tabla_tipo_paciente_df, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  labs(title = "Diagrama de Barras: Tipo de Paciente",
       x = "Tipo de Paciente",
       y = "Frecuencia") +
  theme_minimal()

ggsave(filename = "Exports/diagrama_tipo_paciente.jpg", plot = tipo_paciente, width = 10, height = 6, units = "in")


# Histograma de la variable FECHA_SINTOMAS
library(ggplot2)
covidDurango$FECHA_SINTOMAS <- as.Date(covidDurango$FECHA_SINTOMAS, format = "%Y-%m-%d")

ggplot(covidDurango, aes(x = FECHA_SINTOMAS)) +
  geom_histogram(fill = "#68228B", color = "black", binwidth = 7) +  # Agrupar por semanas
  labs(title = "Histograma de la Fecha de Síntomas por meses",
       x = "Fecha de Síntomas",
       y = "Personas con Sintomas") +
  scale_x_date(date_breaks = "1 month",  # Etiquetas por mes
               date_labels = "%b %Y",    # Mostrar nombre del mes y el año
               limits = as.Date(c("2020-01-01", "2020-12-31"))) +  # Limitar el rango de fechas a 2020
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),  # Centrar el título
        axis.text.x = element_text(angle = 45, hjust = 1))  # Rotar etiquetas en el eje X

# Histograma de la variable FECHA_DEF

covidDurango$FECHA_DEF <- as.Date(covidDurango$FECHA_DEF, format="%Y-%m-%d")
covidDurango_def <- subset(covidDurango, !is.na(FECHA_DEF))

ggplot(covidDurango_def, aes(x = FECHA_DEF)) +
  geom_histogram(fill = "lightcoral", color = "black", binwidth = 7) +  
  labs(title = "Distribución de la Fecha de Defunción",
       x = "Fecha de Defunción",
       y = "Frecuencia") +
  scale_x_date(date_breaks = "1 month",  
               date_labels = "%b %Y") +  
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),  
        axis.text.x = element_text(angle = 45, hjust = 1))

# Medidas de tendencia Central de la variable EDAD

Edad <- covidDurango %>% 
  summarise(Minimo=min(EDAD), 
            Maximo=max(EDAD),
            Media=mean(EDAD),
            Mediana=median(EDAD),
            Q1=quantile(EDAD, probs = 0.25),
            Q3=quantile(EDAD, probs = 0.75),
            P10= quantile(EDAD, probs = 0.10),
            P90= quantile(EDAD, probs = 0.90))

View(Edad)

# Diagrama de barras de la variable DIABETES
library(ggplot2)

tabla_diabetes_df <- as.data.frame(table(covidDurango$DIABETES))

tabla_diabetes <- ggplot(tabla_diabetes_df, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  labs(title = "Diagrama de Barras: Personas con Diabetes",
       x = "Diabetes",
       y = "Frecuencia") +
  theme_minimal()

ggsave(filename = "Exports/diagrama_diabetes.jpg", plot = tabla_diabetes, width = 10, height = 6, units = "in")

# Tabla de Frecuencias de personas con Obesidad

tabla_obesidad <- table(covidDurango$OBESIDAD)
View(tabla_obesidad)


# 