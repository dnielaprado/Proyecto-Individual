# Capítulo 0: Preparación de datos

#Carga de paquetes, lectura de datos y paleta utilizada
library(tidyverse)
library(scales)
library(ggrepel)
data <- read_csv("C:/Users/danie/OneDrive/Escritorio/UCR/II 2025/Herramientas/datos.csv")
paleta <- c("#FEEA08", "#203667", "#2957A4", "#9DD5E4", "#9A9899")

#Filtrado, renombramiento y recodificación de variables

#Renombramiento de las variables por nombres descriptivos
data <- data %>%
  rename(
    pais = COUNTRYNEW,
    id_unico = WPID_RANDOM,
    peso_nacional = WGT,
    peso_poblacional = PROJWT,
    fecha_estudio = FIELD_DATE,
    anio_encuesta = YEAR_WAVE,
    conocimiento_ciencia = W1,
    comprension_ciencia = W2,
    ultimo_nivel_aprendizaje_ciencia = W3,
    confianza_hospitales = W4,
    confianza_vecinos = W5A,
    confianza_gobierno = W5B,
    confianza_cientificos = W5C,
    confianza_periodistas = W5D,
    confianza_medicos = W5E,
    confianza_ong = W5F,
    confianza_curanderos = W5G,
    confianza_ciencia = W6,
    confianza_cientificos_info = W7A,
    confianza_cientificos_beneficio_publico = W7B,
    gobierno_valora_ciencia = W7C,
    ciencia_beneficia_pais = W8,
    ciencia_beneficia_personas_como_tu = W9,
    impacto_ciencia_empleo = W10,
    impacto_ciencia_salud = W11A,
    impacto_ciencia_ambiente = W11B,
    ciencia_explica_cuerpo = MH2A,
    ciencia_explica_emociones = MH2B,
    conoce_cambio_climatico = W13,
    entiende_cambio_climatico = W14,
    amenaza_cambio_climatico = W15,
    covid_confia_gobierno = W15_1A,
    covid_confia_familia = W15_1B,
    covid_confia_oms = W15_1C,
    covid_confia_medicos = W15_1D,
    covid_confia_religiosos = W15_1E,
    gobierno_ayuda_todo_mundo = W15_2A,
    gobierno_ayuda_solo_pais = W15_2B,
    importancia_salud_mental = MH1,
    ciencia_trata_cancer = MH3A,
    ciencia_trata_ansiedad = MH3B,
    ciencia_trata_infecciones = MH3C,
    ciencia_trata_obesidad = MH3D,
    gobierno_fondea_cancer = MH4A,
    gobierno_fondea_ansiedad = MH4B,
    hablar_ansiedad_local = MH5,
    familiares_ansiosos = MH6,
    ha_estado_ansioso = MH7A,
    edad_primera_ansiedad = MH7B,
    edad_primera_ansiedad_rango = MH7B_2,
    ha_estado_ansioso_mas_una_vez = MH7C,
    hablo_profesional_salud = MH8A,
    participo_actividad_religiosa = MH8B,
    hablo_amigos_familia = MH8C,
    tomo_medicamento_recetado = MH8D,
    mejoro_estilo_vida = MH8E,
    cambio_situacion_trabajo = MH8F,
    cambio_relaciones_personales = MH8G,
    paso_tiempo_naturaleza = MH8H,
    utilidad_profesional_salud = MH9A,
    utilidad_actividad_religiosa = MH9B,
    utilidad_amigos_familia = MH9C,
    utilidad_medicamento_recetado = MH9D,
    utilidad_mejorar_estilo_vida = MH9E,
    utilidad_cambio_trabajo = MH9F,
    utilidad_cambio_relaciones = MH9G,
    utilidad_paso_tiempo_naturaleza = MH9H,
    uso_redes_sociales = W27,
    frecuencia_redes_sociales = W28,
    ve_info_salud_redes = W29,
    ciencia_vs_religion = W30,
    vida_afectada_covid = WP21757,
    paro_temporal_covid = WP21758,
    perdio_empleo_covid = WP21759,
    redujo_horas_covid = WP21760,
    redujo_ingresos_covid = WP21761,
    acepta_vacuna_covid = WP21768,
    edad = Age,
    grupo_edad_3 = age_var1,
    grupo_edad_4 = age_var2,
    grupo_edad_mental = age_var3,
    genero = Gender,
    nivel_educativo = Education,
    ingreso_hogar_quintil = Household_Income,
    region_global = Global11Regions,
    nivel_ingreso_banco_mundial = wbi,
    percepcion_ingreso = Subjective_Income,
    estado_empleo = EMP_2010
  )

#Nuevo DataFrame con solo las variables a estudio
data_filtrados <- data %>%
  filter(region_global %in% c(8, 9)) %>% 
  select(
    pais,
    region_global,
    edad,
    grupo_edad_3,
    grupo_edad_4,
    grupo_edad_mental,
    genero,
    acepta_vacuna_covid,
    nivel_educativo,
    ingreso_hogar_quintil,
    nivel_ingreso_banco_mundial,
    percepcion_ingreso,
    estado_empleo,
    confianza_hospitales,
    confianza_vecinos,
    confianza_gobierno,
    confianza_cientificos,
    confianza_periodistas,
    confianza_medicos,
    confianza_ong,
    confianza_curanderos,
    confianza_ciencia,
    confianza_cientificos_info,
    confianza_cientificos_beneficio_publico,
    gobierno_valora_ciencia,
    ciencia_beneficia_pais,
    ciencia_beneficia_personas_como_tu,
    covid_confia_gobierno,
    covid_confia_familia,
    covid_confia_oms,
    covid_confia_medicos,
    covid_confia_religiosos,
    gobierno_ayuda_todo_mundo,
    gobierno_ayuda_solo_pais,
    ciencia_vs_religion,
    vida_afectada_covid,
    paro_temporal_covid,
    perdio_empleo_covid,
    redujo_horas_covid,
    redujo_ingresos_covid
  )

#Reemplazo de codificación en los datos de las variables
# Función para recodificar variables de confianza
recode_confianza <- function(x) {
  case_when(
    x == 1 ~ "Mucho",
    x == 2 ~ "Algo",
    x == 3 ~ "Poco",
    x == 4 ~ "Nada",
    x == 99 ~ "NS/NR",
    TRUE ~ NA_character_
  )
}

# Función para recodificar sí/no con NS/NR
recode_si_no <- function(x) {
  case_when(
    x == 1 ~ "Sí",
    x == 2 ~ "No",
    x %in% c(8,9,99) ~ "NS/NR",
    TRUE ~ NA_character_
  )
}

# Recode general para grupos de edad
recode_grupo_edad_3 <- function(x) {
  case_when(
    x == 1 ~ "15-29",
    x == 2 ~ "30-49",
    x == 3 ~ "50+",
    x == 99 ~ "NS/NR",
    TRUE ~ NA_character_
  )
}

recode_grupo_edad_4 <- function(x) {
  case_when(
    x == 1 ~ "15-29",
    x == 2 ~ "30-49",
    x == 3 ~ "50-64",
    x == 4 ~ "65+",
    x == 99 ~ "NS/NR",
    TRUE ~ NA_character_
  )
}

recode_grupo_edad_mental <- function(x) {
  case_when(
    x == 1 ~ "15-24",
    x == 2 ~ "25-34",
    x == 3 ~ "35-49",
    x == 4 ~ "50+",
    x == 99 ~ "NS/NR",
    TRUE ~ NA_character_
  )
}

# Recodificación de otras variables específicas
data_filtrados <- data_filtrados %>%
  mutate(
    region_global = case_when(
      region_global == 1 ~ "Europa Occidental",
      region_global == 2 ~ "Europa Oriental",
      region_global == 3 ~ "Rusia/Cáucaso/Asia Central",
      region_global == 4 ~ "Australia/Nueva Zelanda",
      region_global == 5 ~ "Asia Oriental",
      region_global == 6 ~ "Sudeste Asiático",
      region_global == 7 ~ "Asia del Sur",
      region_global == 8 ~ "América Latina",
      region_global == 9 ~ "Norteamérica",
      region_global == 10 ~ "Medio Oriente/Norte de África",
      region_global == 11 ~ "África Subsahariana",
      TRUE ~ NA_character_
    ),
    gobierno_valora_ciencia = case_when(
      gobierno_valora_ciencia == 1 ~ "Mucho",
      gobierno_valora_ciencia == 2 ~ "Algo",
      gobierno_valora_ciencia == 3 ~ "Poco",
      gobierno_valora_ciencia == 4 ~ "Nada",
      gobierno_valora_ciencia == 99 ~ "NS/NR",
      TRUE ~ NA_character_
    ),
    genero = case_when(
      genero == 1 ~ "Hombre",
      genero == 2 ~ "Mujer",
      TRUE ~ NA_character_
    ),
    nivel_educativo = case_when(
      nivel_educativo == 1 ~ "Primaria o menos",
      nivel_educativo == 2 ~ "Secundaria",
      nivel_educativo == 3 ~ "Terciaria",
      TRUE ~ NA_character_
    ),
    ingreso_hogar_quintil = case_when(
      ingreso_hogar_quintil == 1 ~ "El 20% de ingresos más bajos en su país",
      ingreso_hogar_quintil == 2 ~ "Segundo 20%",
      ingreso_hogar_quintil == 3 ~ "Medio 20%",
      ingreso_hogar_quintil == 4 ~ "Cuarto 20%",
      ingreso_hogar_quintil == 5 ~ "El 20% de ingresos más altos en su país",
      TRUE ~ NA_character_
    ),
    nivel_ingreso_banco_mundial = case_when(
      nivel_ingreso_banco_mundial == 1 ~ "Ingreso bajo",
      nivel_ingreso_banco_mundial == 2 ~ "Ingreso medio-bajo",
      nivel_ingreso_banco_mundial == 3 ~ "Ingreso medio-alto",
      nivel_ingreso_banco_mundial == 4 ~ "Ingreso alto",
      TRUE ~ NA_character_
    ),
    grupo_edad_3 = recode_grupo_edad_3(grupo_edad_3),
    grupo_edad_4 = recode_grupo_edad_4(grupo_edad_4),
    grupo_edad_mental = recode_grupo_edad_mental(grupo_edad_mental),
    acepta_vacuna_covid = case_when(
      acepta_vacuna_covid == 1 ~ "Sí, aceptaría",
      acepta_vacuna_covid == 2 ~ "No, no aceptaría",
      acepta_vacuna_covid %in% c(8, 9, 99) ~ "NS/NR",
      TRUE ~ NA_character_
    ),
    percepcion_ingreso = case_when(
      percepcion_ingreso == 1 ~ "Vive cómodamente",
      percepcion_ingreso == 2 ~ "Sobrevive",
      percepcion_ingreso == 3 ~ "Difícil",
      percepcion_ingreso == 4 ~ "Muy difícil",
      percepcion_ingreso %in% c(8,9) ~ "NS/NR",
      TRUE ~ NA_character_
    ),
    estado_empleo = case_when(
      estado_empleo == 1 ~ "Empleado tiempo completo para un empleador",
      estado_empleo == 2 ~ "Empleado tiempo completo independiente",
      estado_empleo == 3 ~ "Empleado medio tiempo, no quiere tiempo completo",
      estado_empleo == 4 ~ "Desempleado",
      estado_empleo == 5 ~ "Empleado medio tiempo, quiere tiempo completo",
      estado_empleo == 6 ~ "Fuera de la fuerza laboral",
      TRUE ~ NA_character_
    ),
    gobierno_ayuda_todo_mundo = recode_confianza(gobierno_ayuda_todo_mundo),
    gobierno_ayuda_solo_pais = recode_confianza(gobierno_ayuda_solo_pais),
    paro_temporal_covid = recode_si_no(paro_temporal_covid),
    perdio_empleo_covid = recode_si_no(perdio_empleo_covid),
    redujo_horas_covid = recode_si_no(redujo_horas_covid),
    redujo_ingresos_covid = recode_si_no(redujo_ingresos_covid),
    vida_afectada_covid = case_when(
      vida_afectada_covid == 1 ~ "Mucho",
      vida_afectada_covid == 2 ~ "Algo",
      vida_afectada_covid == 3 ~ "Nada",
      vida_afectada_covid %in% c(8,9) ~ "NS/NR",
      TRUE ~ NA_character_
    ),
    ciencia_vs_religion = case_when(
      ciencia_vs_religion == 1 ~ "Ciencia",
      ciencia_vs_religion == 2 ~ "Religión",
      ciencia_vs_religion == 3 ~ "No hay conflicto",
      ciencia_vs_religion == 4 ~ "Depende",
      ciencia_vs_religion == 99 ~ "NS/NR",
      TRUE ~ NA_character_
    ),
    ciencia_beneficia_pais = case_when(
      ciencia_beneficia_pais == 1 ~ "La mayoría",
      ciencia_beneficia_pais == 2 ~ "Algunos",
      ciencia_beneficia_pais == 3 ~ "Muy pocos",
      ciencia_beneficia_pais == 99 ~ "NS/NR",
      TRUE ~ NA_character_
    ),
    ciencia_beneficia_personas_como_tu = case_when(
      ciencia_beneficia_personas_como_tu == 1 ~ "Mucho",
      ciencia_beneficia_personas_como_tu == 2 ~ "Un poco",
      ciencia_beneficia_personas_como_tu == 3 ~ "Nada",
      ciencia_beneficia_personas_como_tu == 99 ~ "NS/NR",
      TRUE ~ NA_character_
    ),
    across(starts_with("confianza_"), recode_confianza),
    across(starts_with("covid_confia_"), recode_confianza)
  )

#Tratamiento de valores faltantes
data_filtrados <- data_filtrados %>%
  mutate(
    ingreso_hogar_quintil = ifelse(is.na(ingreso_hogar_quintil), "Medio 20%", ingreso_hogar_quintil),
    across(
      c(
        nivel_educativo, confianza_gobierno,
        confianza_hospitales, confianza_periodistas, gobierno_valora_ciencia,
        acepta_vacuna_covid, percepcion_ingreso, vida_afectada_covid,
        paro_temporal_covid, perdio_empleo_covid, redujo_horas_covid,
        redujo_ingresos_covid, ciencia_vs_religion, gobierno_ayuda_todo_mundo, 
        gobierno_ayuda_solo_pais 
      ),
      ~ ifelse(is.na(.), "NS/NR", .)
    )
  )
colSums(is.na(data_filtrados))

#Capítulo I: El impacto desigual del Covid-19 en América
# GRÁFICO 1.1: Trabajadores que perdieron su empleo o negocio debido al Covid-19, según el nivel de ingreso del país.
#Porcentaje de personas que respondieron “Sí”. Pregunta: ¿ha perdido su empleo o negocio como resultado de la situación del Covid-19?
#Resumir datos según el nivel de ingreso del país
empleo_resumen <- data_filtrados %>%
  filter(perdio_empleo_covid %in% c("Sí", "No")) %>%
  group_by(nivel_ingreso_banco_mundial) %>%
  summarise(
    total = n(),
    perdio = sum(perdio_empleo_covid == "Sí"),
    porcentaje = (perdio / total) * 100
  )

# Gráfico que muestra el porcentaje de trabajadores que perdieron empleo o negocio por Covid-19
ggplot(empleo_resumen, aes(x = nivel_ingreso_banco_mundial, y = porcentaje, fill = nivel_ingreso_banco_mundial)) +
  geom_col() +
  geom_text(aes(label = paste0(round(porcentaje, 1), "%")), vjust = -0.5, fontface = "bold") +
  scale_fill_manual(values = paleta) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "Trabajadores que perdieron empleo o negocio por Covid-19,\nsegún grupo de ingreso",
    x = "Grupo de ingreso del país",
    y = "Porcentaje de trabajadores"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 14)
  )

### TABLA 1.1: Consecuencias económicas sufridas por trabajadores debido al Covid-19, por país
# Porcentaje de trabajadores que respondieron "sí". Pregunta: "¿Ha experimentado cada una de las siguientes situaciones como resultado del Covid-19?"

## Calcular el porcentaje de trabajadores afectados por cada consecuencia económica en cada país
tabla_por_pais <- data_filtrados %>%
  group_by(pais) %>%
  summarise(
    `Perdió su empleo o negocio` = mean(perdio_empleo_covid == "Sí", na.rm = TRUE) * 100,
    `Paró temporalmente su trabajo o negocio` = mean(paro_temporal_covid == "Sí", na.rm = TRUE) * 100,
    `Trabajó menos horas en su trabajo o negocio` = mean(redujo_horas_covid == "Sí", na.rm = TRUE) * 100,
    `Recibió menos dinero de lo habitual de su empleador o negocio` = mean(redujo_ingresos_covid == "Sí", na.rm = TRUE) * 100
  ) %>%
  arrange(desc(`Perdió su empleo o negocio`)) 

tabla_por_pais

#GRÁFICO 1.2: Trabajadores que perdieron su empleo o negocio debido al Covid-19,según quintil de ingreso dentro del país
#Porcentaje de trabajadores que respondieron "sí".  
#Pregunta: "¿Ha perdido su empleo o negocio como resultado de la situación del Covid-19?"

# Reordenar factores del quintil de ingreso
data_filtrados <- data_filtrados %>%
  mutate(
    ingreso_hogar_quintil = factor(
      ingreso_hogar_quintil,
      levels = c(
        "El 20% de ingresos más bajos en su país",
        "Segundo 20%",
        "Medio 20%",
        "Cuarto 20%",
        "El 20% de ingresos más altos en su país"
      )
    )
  )

#Calcular porcentaje por quintil
tabla_income <- data_filtrados %>%
  group_by(ingreso_hogar_quintil) %>%
  summarise(
    Perdió_empleo_negocio = mean(perdio_empleo_covid == "Sí", na.rm = TRUE) * 100
  )

#Gráfico de barras
ggplot(tabla_income, aes(x = ingreso_hogar_quintil, y = Perdió_empleo_negocio)) +
  geom_bar(stat = "identity", fill = "#FEEA08") +
  geom_text(aes(label = round(Perdió_empleo_negocio, 1)), hjust = -0.1) +
  labs(
    title = "Porcentaje de trabajadores que perdieron su empleo\n o negocio según quintil de ingreso",
    x = "Quintil de ingreso del hogar",
    y = "Porcentaje de trabajadores (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  coord_flip()

# Capítulo II: Apoyo público al gasto global en la prevención y cura de enfermedades
#GRÁFICO 2.1: Opiniones sobre el gasto gubernamental para prevenir y curar enfermedades (resultados para América)
#Porcentaje de personas que respondieron desde “muy de acuerdo” hasta “muy en desacuerdo”.  
# Pregunta: “Para cada afirmación, dígame si está muy de acuerdo, algo de acuerdo, algo en desacuerdo o muy en desacuerdo.”

# Transformar a formato largo
data_long <- data_filtrados %>%
  select(gobierno_ayuda_todo_mundo, gobierno_ayuda_solo_pais) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Pregunta",
    values_to = "Respuesta"
  )

# Calcular porcentajes por respuesta y pregunta
tabla_porcentajes <- data_long %>%
  group_by(Pregunta, Respuesta) %>%
  summarise(Cantidad = n(), .groups = "drop") %>%
  group_by(Pregunta) %>%
  mutate(Porcentaje = Cantidad / sum(Cantidad) * 100)

# Ordenar niveles de respuestas
tabla_porcentajes$Respuesta <- factor(
  tabla_porcentajes$Respuesta,
  levels = c("Mucho", "Algo", "Poco", "Nada", "NS/NR")
)

# Gráfico de barras que muestra la distribución porcentual de niveles de acuerdo para cada afirmación
ggplot(tabla_porcentajes, aes(x = Pregunta, y = Porcentaje, fill = Respuesta)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = paleta) +
  scale_x_discrete(labels = c(
    "gobierno_ayuda_todo_mundo" = "Gastar dinero para ayudar a\notros países a prevenir\ny curar enfermedades",
    "gobierno_ayuda_solo_pais" = "Gastar dinero solo si las\nenfermedades representan\nriesgo en este país"
  )) +
  labs(
    title = "Opinión sobre gasto gubernamental para prevenir y curar enfermedades",
    x = "",
    y = "Porcentaje de personas",
    fill = "Nivel de acuerdo"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 10, hjust = 1),
    axis.text.y = element_text(size = 10),
    legend.position = "bottom"
  )


# GRÁFICO 2.2: Comparación del acuerdo con dos enfoques de gasto gubernamental en salud, por país.
#Porcentaje relativo de personas que respondieron “Mucho” o “Algo”.

# Calcular porcentaje de acuerdo ("Mucho" + "Algo") por país y tipo de ayuda
porcentaje_acuerdo <- data_filtrados %>%
  select(pais, gobierno_ayuda_todo_mundo, gobierno_ayuda_solo_pais) %>%
  pivot_longer(cols = -pais, names_to = "Pregunta", values_to = "Respuesta") %>%
  filter(Respuesta %in% c("Mucho", "Algo")) %>%
  group_by(pais, Pregunta) %>%
  summarise(Cantidad = n(), .groups = "drop") %>%
  group_by(pais) %>%
  mutate(Porcentaje = Cantidad / sum(Cantidad) * 100)

# Separar las preguntas
todo_mundo <- porcentaje_acuerdo %>% filter(Pregunta == "gobierno_ayuda_todo_mundo") %>% select(pais, Porcentaje_todo = Porcentaje)
solo_pais <- porcentaje_acuerdo %>% filter(Pregunta == "gobierno_ayuda_solo_pais") %>% select(pais, Porcentaje_solo = Porcentaje)

# Unir los datos
diferencia_long <- todo_mundo %>%
  inner_join(solo_pais, by = "pais") %>%
  pivot_longer(cols = c(Porcentaje_todo, Porcentaje_solo),
               names_to = "Tipo",
               values_to = "Porcentaje") %>%
  mutate(Tipo = recode(Tipo,
                       "Porcentaje_todo" = "Ayudar a otros países",
                       "Porcentaje_solo" = "Ayudar solo a su país"))

# Crear un índice para dividir en 2 grupos
paises <- unique(diferencia_long$pais)
grupo <- rep(1:2, length.out = length(paises))
grupo_df <- data.frame(pais = paises, Grupo = grupo)

diferencia_long <- diferencia_long %>%
  left_join(grupo_df, by = "pais")

ggplot(diferencia_long, aes(x = reorder(pais, Porcentaje), y = Porcentaje, fill = Tipo)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_text(aes(label = paste0(round(Porcentaje, 1), "%"),
                color = Tipo),
            position = position_dodge(width = 0.8),
            hjust = 1.1,
            size = 3,
            show.legend = FALSE) + 
  scale_color_manual(values = c("Ayudar solo a su país" = "white",
                                "Ayudar a otros países" = "black")) +
  coord_flip() +
  facet_wrap(~ Grupo, scales = "free_y") +
  scale_fill_manual(values = paleta) +
  labs(
    title = "Opinión sobre gasto gubernamental para prevenir y curar enfermedades",
    x = "País",
    y = "Porcentaje de acuerdo",
    fill = "Tipo de gasto"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(size = 9),
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))

# Capítulo III: Confianza y valoración percibida de la ciencia en el contexto del Covid-19

### GRÁFICO 3.1: Percepción sobre la base científica de decisiones relacionadas con el coronavirus
#Porcentaje de personas que dijeron “Mucho”, “Algo”, “Poco” o “Nada”.  
#Pregunta: ¿En general, cuánto cree que cada uno de los siguientes toma decisiones sobre coronavirus basándose en asesoramiento científico?
  
# Selección y transformación de los datos a formato largo
data_fuentes <- data_filtrados %>%
  select(covid_confia_religiosos, covid_confia_familia, covid_confia_gobierno, 
         covid_confia_oms, covid_confia_medicos) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Fuente",
    values_to = "Respuesta"
  )

# Recodificación de respuestas a factor con niveles ordenados
data_fuentes <- data_fuentes %>%
  mutate(
    Respuesta = factor(Respuesta, levels = c("Mucho", "Algo", "Poco", "Nada", "NS/NR"))
  )

# Cálculo de porcentaje de cada respuesta por fuente
tabla_porcentajes <- data_fuentes %>%
  group_by(Fuente, Respuesta) %>%
  summarise(Cantidad = n(), .groups = "drop") %>%
  group_by(Fuente) %>%
  mutate(Porcentaje = Cantidad / sum(Cantidad) * 100)

# Renombrar las fuentes para hacer los nombres más legibles en el gráfico
tabla_porcentajes$Fuente <- recode(tabla_porcentajes$Fuente,
                                   "covid_confia_religiosos" = "Líderes\nreligiosos",
                                   "covid_confia_familia" = "Familia/\nAmigos",
                                   "covid_confia_gobierno" = "Gobierno\nnacional",
                                   "covid_confia_oms" = "OMS",
                                   "covid_confia_medicos" = "Doctores/\nEnfermeras")


ggplot(tabla_porcentajes, aes(x = Fuente, y = Porcentaje, fill = Respuesta)) +
  geom_bar(stat = "identity") +
  geom_text(
    aes(label = ifelse(Porcentaje > 4.7, paste0(round(Porcentaje), "%"), "")),
    position = position_stack(vjust = 0.5),
    color = "black",
    size = 3.5,
    fontface = "bold"
  ) +
  scale_fill_manual(values = paleta) +
  labs(
    title = "Percepción sobre decisiones de coronavirus basadas en ciencia",
    x = "",
    y = "Porcentaje de personas (%)",
    fill = "Cuánto basan sus decisiones en ciencia"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    legend.position = "bottom"
  ) +
  coord_flip()

### TABLA 3.1: Creencia de que distintas fuentes basan sus decisiones sobre coronavirus en asesoramiento científico
#Porcentaje de personas que respondieron “Mucho”.  
#Pregunta: ¿En general, cuánto cree que cada uno de los siguientes toma decisiones sobre coronavirus basándose en asesoramiento científico?
  
# Selección de las variables de interés y transformación a formato largo
tabla_confianza <- data_filtrados %>%
  select(pais, covid_confia_religiosos, covid_confia_familia, covid_confia_gobierno, 
         covid_confia_oms, covid_confia_medicos) %>%
  pivot_longer(
    cols = -pais,
    names_to = "actor",
    values_to = "respuesta"
  ) %>%
  # Calcular porcentaje de personas que respondieron "Mucho" por país y actor
  group_by(pais, actor) %>%
  summarise(
    pct_mucho = mean(respuesta == "Mucho", na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  # Reorganizar los datos a formato ancho para presentar cada actor como columna
  pivot_wider(
    names_from = actor,
    values_from = pct_mucho
  )

# Visualización de la tabla
tabla_confianza

### TABLA 3.2: Creencia de que la OMS basa sus decisiones sobre coronavirus en asesoramiento científico comparada con la creencia sobre los gobiernos nacionales
#Porcentaje de personas que respondieron “Mucho”.  
#Pregunta: ¿En general, cuánto cree que cada uno de los siguientes toma decisiones sobre coronavirus basándose en asesoramiento científico?

# Selección de variables y transformación a formato largo
tabla_oms_gob <- data_filtrados %>%
  select(pais, covid_confia_oms, covid_confia_gobierno) %>%
  pivot_longer(
    cols = c(covid_confia_oms, covid_confia_gobierno),
    names_to = "Fuente",
    values_to = "Respuesta"
  ) %>%
  mutate(Fuente = recode(Fuente,
                         "covid_confia_oms" = "OMS",
                         "covid_confia_gobierno" = "Gobierno nacional"))

# Cálculo de porcentajes de respuestas "Mucho" por país y fuente
tabla_oms_gob <- tabla_oms_gob %>%
  group_by(pais, Fuente) %>%
  summarise(Porcentaje_Mucho = sum(Respuesta == "Mucho") / n() * 100, .groups = "drop") %>%
  pivot_wider(names_from = Fuente, values_from = Porcentaje_Mucho)

# Cálculo de la diferencia OMS - Gobierno y ordenamiento
tabla_oms_gob <- tabla_oms_gob %>%
  mutate(Diferencia = OMS - `Gobierno nacional`) %>%
  arrange(desc(Diferencia))

# Mostrar la tabla final
tabla_oms_gob


### GRÁFICO 3.2: Relación entre la confianza en científicos y la percepción de que la ciencia beneficia al país
#Porcentaje de personas que respondieron 'Mucho' o 'La mayoría'.

# Resumir los datos por país calculando porcentaje de respuestas "Mucho" en confianza y "La mayoría" en beneficio de la ciencia
scatter_ci <- data_filtrados %>%
  group_by(pais) %>%
  summarise(
    Confianza_Cientificos = sum(confianza_cientificos == "Mucho") / n() * 100,
    Ciencia_Beneficia_Pais = sum(ciencia_beneficia_pais == "La mayoría") / n() * 100
  )

# Gráfico de dispersión con línea de regresión y etiquetas de país
ggplot(scatter_ci, aes(x = Confianza_Cientificos, y = Ciencia_Beneficia_Pais)) +
  geom_point(color = "#FEEA08", size = 4, alpha = 0.9) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    linetype = "dashed",
    color = "#9A9899",
    size = 0.6
  ) +
  geom_text_repel(
    aes(label = pais),
    size = 3,
    fontface = "bold",
    segment.color = "#9A9899",
    max.overlaps = 100
  ) +
  scale_x_continuous(labels = percent_format(scale = 1), limits = c(10, 75)) +
  scale_y_continuous(labels = percent_format(scale = 1), limits = c(10, 60)) +
  labs(
    title = "Relación entre confianza en científicos y percepción\n del beneficio de la ciencia",
    x = "Confianza en científicos",
    y = "Percepción de que la ciencia beneficia al país"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14)
  )

### Gráfico 3.3: Relación entre confianza en el gobierno y percepción de que las decisiones sobre Covid-19 se basan en ciencia
#Porcentaje de personas que respondieron “Mucho”  
#Este gráfico muestra cómo, en cada país, la confianza general en el gobierno se asocia con la percepción de que el gobierno toma decisiones sobre el coronavirus basándose en asesoría científica.

# Calcular porcentaje "Mucho" por país
scatter_data <- data_filtrados %>%
  group_by(pais) %>%
  summarise(
    Confianza_Gobierno = sum(confianza_gobierno == "Mucho") / n() * 100,
    Gobierno_Basado_Ciencia = sum(covid_confia_gobierno == "Mucho") / n() * 100
  )

#C rear gráfico de dispersión con la relación entre confianza en el gobierno y valoración de la opinión científica
ggplot(scatter_data, aes(x = Confianza_Gobierno, y = Gobierno_Basado_Ciencia)) +
  geom_point(color = "#FEEA08", size = 4, alpha = 0.9) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    linetype = "dashed",
    color = "#9A9899",
    size = 0.6
  ) +
  geom_text_repel(
    aes(label = pais),
    size = 3,
    fontface = "bold",
    segment.color = "#9A9899",
    max.overlaps = 100
  ) +
  scale_x_continuous(labels = percent_format(scale = 1), limits = c(0, 43)) +
  scale_y_continuous(labels = percent_format(scale = 1), limits = c(10, 66)) +
  labs(
    title = "Relación entre confianza en el gobierno y decisiones basadas en ciencia",
    x = "Confianza en el gobierno",
    y = "Decisiones del gobierno basadas en ciencia"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title.y = element_text(margin = margin(r = 15))
  )

### Tabla 3.3: Países y territorios donde las personas eran más propensas a decir que los líderes de su gobierno no valoran la opinión de los científicos
#Porcentaje de personas que dijeron “mucho”/“algo” en comparación con “poco”/“nada”.  
#Pregunta: En general, ¿cuánto cree que los líderes del gobierno nacional valoran la opinión y la experiencia de los científicos?
  
# Calcular porcentajes por país
tabla_3_3 <- data_filtrados %>%
  group_by(pais) %>%
  summarise(
    total = n(),
    A_lot_Some = sum(gobierno_valora_ciencia %in% c("Mucho", "Algo"), na.rm = TRUE)/total*100,
    Not_much_Not_at_all = sum(gobierno_valora_ciencia %in% c("Poco", "Nada"), na.rm = TRUE)/total*100
  ) %>%
  mutate(Diferencia = round(A_lot_Some - Not_much_Not_at_all, 0)) %>%
  arrange(Diferencia) 

# Formatear tabla final
tabla_3_3 <- tabla_3_3 %>%
  mutate(
    `Mucho/Algo` = round(A_lot_Some),
    `Poco/Nada` = round(Not_much_Not_at_all),
    `Diferencia (pts)` = Diferencia
  ) %>%
  select(pais, `Mucho/Algo`, `Poco/Nada`, `Diferencia (pts)`)

tabla_3_3                                                  
