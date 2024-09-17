# Cargar librerías
library(readr)
library(dplyr)
library(ggplot2)

# Leer archivo CSV
archivo <- "Data/Datos_Tarea2.csv"
if (file.exists(archivo)) {
  covidData <- read.csv(archivo)
} else {
  stop("El archivo no existe, por favor verifica la ruta.")
}

# Filtrar datos para Durango
covidDurango <- covidData %>% filter(ENTIDAD_UM == 10)

# Transformación de variables
covidDurango <- covidDurango %>%
  mutate(SEXO = factor(SEXO, levels = c(1, 2),
                       labels = c("Mujer", "Hombre")),
         TIPO_PACIENTE = factor(TIPO_PACIENTE, levels = c(1, 2),
                                labels = c("Ambulatorio", "Hospitalizado")),
         DIABETES = factor(DIABETES, levels = c(1, 2),
                           labels = c("Si", "No")),
         OBESIDAD = factor(OBESIDAD, levels = c(1, 2),
                           labels = c("Si", "No")),
         UCI = factor(UCI, levels = c(1, 2),
                      labels = c("Si", "No")),
         INTUBADO = factor(INTUBADO, levels = c(1, 2),
                           labels = c("Si", "No")))

# Calcular el total dinámicamente
total <- nrow(covidDurango)

# Tabla de Frecuencia de la variable SEXO
Tabla_Sexo <- covidDurango %>%
  group_by(SEXO) %>%
  summarise(Frecuencia = n(),
            Porcentaje = round(Frecuencia / total * 100, 3))

# Añadir fila de total
Tabla_Sexo_Total <- Tabla_Sexo %>%
  bind_rows(summarise(Tabla_Sexo,
                      SEXO = "Total", 
                      Frecuencia = sum(Frecuencia), 
                      Porcentaje = round(sum(Frecuencia) / total * 100, 3)))

# Diagrama de Barras de la variable TIPO_PACIENTE
tabla_tipo_paciente_df <- as.data.frame(table(covidDurango$TIPO_PACIENTE))

tipo_paciente <- ggplot(tabla_tipo_paciente_df, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  labs(title = "Diagrama de Barras: Tipo de Paciente",
       x = "Tipo de Paciente",
       y = "Frecuencia") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave(filename = "Exports/diagrama_tipo_paciente.jpg", plot = tipo_paciente, width = 10, height = 6, units = "in")


# Histograma de la variable FECHA_SINTOMAS
covidDurango$FECHA_SINTOMAS <- as.Date(covidDurango$FECHA_SINTOMAS, format = "%Y-%m-%d")

histograma_fecha_sintomas <- ggplot(covidDurango, aes(x = FECHA_SINTOMAS)) +
  geom_histogram(fill = "#68228B", color = "black", binwidth = 7) + 
  labs(title = "Histograma de la Fecha de Síntomas por meses",
       x = "Fecha de Síntomas",
       y = "Personas con Sintomas") +
  scale_x_date(date_breaks = "1 month", 
               date_labels = "%b %Y",    
               limits = as.Date(c("2020-01-01", "2020-12-31"))) +  
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle = 45, hjust = 1))  

ggsave(filename = "Exports/histograma_fecha_sintomas.jpg", plot = histograma_fecha_sintomas, width = 10, height = 6, units = "in")

# Histograma de la variable FECHA_DEF

covidDurango$FECHA_DEF <- as.Date(covidDurango$FECHA_DEF, format="%Y-%m-%d")
covidDurango_def <- subset(covidDurango, !is.na(FECHA_DEF))

histograma_fecha_def <- ggplot(covidDurango_def, aes(x = FECHA_DEF)) +
  geom_histogram(fill = "lightcoral", color = "black", binwidth = 7) +  
  labs(title = "Distribución de la Fecha de Defunción",
       x = "Fecha de Defunción",
       y = "Frecuencia") +
  scale_x_date(date_breaks = "1 month",  
               date_labels = "%b %Y") +  
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),  
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(filename = "Exports/histograma_fecha_def.jpg", plot = histograma_fecha_def, width = 10, height = 6, units = "in")

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
tabla_diabetes_df <- as.data.frame(table(covidDurango$DIABETES))

tabla_diabetes <- ggplot(tabla_diabetes_df, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  labs(title = "Diagrama de Barras: Personas con Diabetes",
       x = "Diabetes",
       y = "Frecuencia") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave(filename = "Exports/diagrama_diabetes.jpg", plot = tabla_diabetes, width = 10, height = 6, units = "in")

# Tabla de Frecuencias de personas con Obesidad
covidDurango  %>% 
  group_by(OBESIDAD) %>% 
  summarise(Frecuencia = n(),
            Porcentaje = round(Frecuencia / 26503 * 100, 3)) -> Tabla_Obesidad
Tabla_Obesidad_Total <- Tabla_Obesidad %>%
  bind_rows(summarise(Tabla_Obesidad,
                      OBESIDAD = "Total", 
                      Frecuencia = sum(Frecuencia), 
                      Porcentaje = round(sum(Frecuencia) / 26503 * 100, 3)))


# Diagrama de barras de la variable UCI (Unidad de Cuidados Intensivos)

tabla_uci_df <- as.data.frame(table(covidDurango$UCI))

tabla_uci <- ggplot(tabla_uci_df, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  labs(title = "Diagrama de Barras: Personas que fueron a Cuidados Intensivos",
       x = "UCI",
       y = "Frecuencia") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave(filename = "Exports/diagrama_uci.jpg", plot = tabla_uci, width = 10, height = 6, units = "in")


# Diagrama de barras de la variable Intubado

tabla_intubado_df <- as.data.frame(table(covidDurango$INTUBADO))

tabla_intubado <- ggplot(tabla_intubado_df, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  labs(title = "Diagrama de Barras: Personas intubadas",
       x = "Intubados",
       y = "Frecuencia") +
  theme_minimal() +
  theme(legend.position = "none")

# Tabla de doble entrada para SEXO y TIPO_PACIENTE
tabla_sexo_tipo_paciente <- table(covidDurango$SEXO, covidDurango$TIPO_PACIENTE)
total_sexo_tipo_paciente <- sum(tabla_sexo_tipo_paciente)  # Total de casos

# Calcular porcentajes
porcentajes_sexo_tipo_paciente <- prop.table(tabla_sexo_tipo_paciente) * 100  # Porcentajes sobre el total

# Mostrar la tabla de frecuencias absolutas y porcentajes
cat("Tabla de SEXO y TIPO_PACIENTE (frecuencias absolutas):\n")
print(tabla_sexo_tipo_paciente)
cat("\nTabla de SEXO y TIPO_PACIENTE (porcentajes):\n")
print(round(porcentajes_sexo_tipo_paciente, 2))

# Tabla de doble entrada para SEXO y DIABETES
tabla_sexo_diabetes <- table(covidDurango$SEXO, covidDurango$DIABETES)
total_sexo_diabetes <- sum(tabla_sexo_diabetes)  # Total de casos

# Calcular porcentajes
porcentajes_sexo_diabetes <- prop.table(tabla_sexo_diabetes) * 100  # Porcentajes sobre el total

# Mostrar la tabla de frecuencias absolutas y porcentajes
cat("\nTabla de SEXO y DIABETES (frecuencias absolutas):\n")
print(tabla_sexo_diabetes)
cat("\nTabla de SEXO y DIABETES (porcentajes):\n")
print(round(porcentajes_sexo_diabetes, 2))

# Tabla de doble entrada para TIPO_PACIENTE y OBESIDAD
tabla_paciente_obesidad <- table(covidDurango$TIPO_PACIENTE, covidDurango$OBESIDAD)
total_paciente_obesidad <- sum(tabla_paciente_obesidad)  # Total de casos

# Calcular porcentajes
porcentajes_paciente_obesidad <- prop.table(tabla_paciente_obesidad) * 100  # Porcentajes sobre el total

# Mostrar la tabla de frecuencias absolutas y porcentajes
cat("\nTabla de TIPO_PACIENTE y OBESIDAD (frecuencias absolutas):\n")
print(tabla_paciente_obesidad)
cat("\nTabla de TIPO_PACIENTE y OBESIDAD (porcentajes):\n")
print(round(porcentajes_paciente_obesidad, 2))
