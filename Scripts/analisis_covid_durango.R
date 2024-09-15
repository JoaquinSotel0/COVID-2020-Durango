library(readr)

covidData <- read.csv("Data/Datos_Tarea2.csv")


library(dplyr)

covidDurango <- covidData %>% filter (ENTIDAD_UM == 10)

write_csv(covidData, "Exports/COVID19_2020_DURANGO.csv")


# transformación de variables
covidDurango <- covidDurango %>%
  mutate (SEXO = factor(SEXO, levels = c(1,2),
                        labels = c("Mujer", "Hombre")),
          TIPO_PACIENTE = factor(TIPO_PACIENTE, levels = c(1,2,3),
                                 labels = c("Ambulatorio", "Hospitalizado",
                                            "NO ESPECIFICADO")),
          DIABETES = factor(DIABETES, levels = c(1,2),
                            labels = c("Si", "No")))
glimpse(covidDurango)



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

summary(covidDurango$FECHA_SINTOMAS)

ggplot(covidDurango, aes(x = FECHA_SINTOMAS)) +
  geom_histogram(fill = "skyblue", color = "black", binwidth = 7) +  # Agrupar por semanas
  labs(title = "Distribución de la Fecha de Síntomas",
       x = "Fecha de Síntomas",
       y = "Frecuencia") +
  scale_x_date(date_breaks = "1 month",  # Etiquetas por mes
               date_labels = "%b %Y",    # Mostrar nombre del mes y el año
               limits = as.Date(c("2020-01-01", "2020-12-31"))) +  # Limitar el rango de fechas a 2020
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),  # Centrar el título
        axis.text.x = element_text(angle = 45, hjust = 1))  # Rotar etiquetas en el eje X

