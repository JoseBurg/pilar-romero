library(dplyr)
library(performance)
library(readxl)
library(tidyr)
library(broom)

enfct_2019 <- read_excel("datos/base ENCFT 2019 44.xlsx")
enfct_2020 <- read_excel("./datos/BASE ENCFT 2020 44.xlsx")
enfct_2021 <- read_excel("./datos/BASE ENCFT 2021 44.xlsx")
enfct_2022 <- read_excel("./datos/BASE ENCFT 2022 44.xlsx")
enfct_2023 <- read_excel("./datos/BASE ENCFT 2023 44.xlsx")
enfct_2024 <- read_excel("./datos/BASE ENCFT 2024 44.xlsx")

source('./funciones.r', chdir = TRUE)


datos_base <- list(
  enfct_2019,
  enfct_2020,
  enfct_2021,
  enfct_2022,
  enfct_2023,
  enfct_2024
)


datos_base <- purrr::map(datos_base, ~ provincia_to_region(.x) |>
  transformar_variables() |>
  variables_dummy())

datos_base <- purrr::map(
  datos_base,
  ~.x |> 
    select(migrante, no_es_beneficiario, all_of(variables_modelo6)) |> 
    mutate(
      across(
        everything(),
          as.numeric
        )
      )
  )



names(datos_base) <- 2019:2024



# Variables dependientes para cada modelos
variables_independientes <- list(
    variables_modelo1,
    variables_modelo2,
    variables_modelo3,
    variables_modelo4,
    variables_modelo5,
    variables_modelo6
)

# --- Nombramos las listas para que los resultados sean más claros ---
names(variables_independientes) <- paste0("modelo_", 1:6)


# --- Bucle anidado para correr TODOS los modelos en TODOS los años ---
modelos_logit_todos_los_anios <- list()

# Bucle 1: Itera sobre cada año
for (nombre_anio in names(datos_base)) {
  
  # Bucle 2: Itera sobre cada especificación de modelo
  for (nombre_modelo in names(variables_independientes)) {
    
    # Obtenemos los datos y variables para esta combinación
    datos_actuales <- datos_base[[nombre_anio]]
    vars_actuales <- variables_independientes[[nombre_modelo]]
    
    # Creamos la fórmula
    formulas <- paste("migrante ~", paste(vars_actuales, collapse = " + "))

    # Corremos el modelo
    modelo_logit <- glm(as.formula(formulas), family = binomial(link = "logit"), data = datos_actuales)
    
    # Creamos un nombre único, ej: "2019_modelo_1"
    nombre_resultado <- paste(nombre_anio, nombre_modelo, sep = "_")
    
    # Guardamos el modelo en la nueva lista
    modelos_logit_todos_los_anios[[nombre_resultado]] <- modelo_logit
    
    print(paste("Guardado:", nombre_resultado))
  }
}


# --- Ahora, extraemos los resultados de la lista completa ---
# El mismo código de antes funciona perfectamente en la nueva lista
resultados_completos <- purrr::map(modelos_logit_todos_los_anios, broom::tidy)


modelos_logit_por_year <- bind_rows(resultados_completos, .id = "modelo") |>
  mutate(year = stringr::str_extract(modelo, "^[0-9]{4}"),
         especificacion = stringr::str_extract(modelo, "modelo_[0-9]+"),
         .before = everything()
         ) |>
  select(-modelo)

tabla_final <- modelos_logit_por_year |>
  
  # 1. Añadimos una columna para las "estrellas" de significancia basadas en el p-value
  mutate(
    significancia = case_when(
      p.value < 0.01 ~ "***",
      p.value < 0.05 ~ "**",
      p.value < 0.10 ~ "*",
      TRUE ~ ""  # Si no es significativo, no ponemos nada
    )
  ) |>
  
  # 2. Formateamos el coeficiente a 3 decimales y lo unimos con las estrellas
  mutate(
    valor_formateado = paste0(sprintf("%.4f",estimate), significancia)
  ) |>
  
  # 3. Seleccionamos solo las columnas que necesitamos para pivotar
  select(term, year, especificacion, valor_formateado)

tabla_final |> 
  
  # 4. ¡La magia! Pivotamos la tabla para poner los modelos en las columnas
  pivot_wider(
    names_from = c(year, especificacion), # Nombres de las nuevas columnas
    values_from = valor_formateado,      # Valores que irán en las celdas
    id_cols = term,                      # Columna que se quedará como filas
    values_fill = ""                     # Rellena celdas vacías con nada en vez de NA
  )

# Mostramos la tabla final
print(tabla_final)

# Para una vista más elegante en RStudio o al exportar
kableExtra::kbl(tabla_final) |>
  kableExtra::kable_styling(bootstrap_options = "striped", full_width = F)


modelos_logit_por_year |>
  
  # 1. Añadimos una columna para las "estrellas" de significancia basadas en el p-value
  mutate(
    significancia = case_when(
      p.value < 0.01 ~ "***",
      p.value < 0.05 ~ "**",
      p.value < 0.10 ~ "*",
      TRUE ~ ""  # Si no es significativo, no ponemos nada
    )
  ) |>
  
  # 2. Formateamos el coeficiente a 3 decimales y lo unimos con las estrellas
  mutate(
    valor_formateado = paste0(sprintf("%.4f", estimate), significancia)
  ) |>
  
  # 3. Seleccionamos solo las columnas que necesitamos para pivotar
  select(term, year, especificacion, valor_formateado) |> 
  pivot_wider(
    id_cols = c(term, year),
    names_from = especificacion,
    values_from = valor_formateado,
    values_fill = "") |>
    mutate(
      # creando un factor para que se organice la variable term:
      term = factor(
        term,
        levels = c(
          "(Intercept)",
          "sexo_femenino",
          "casado_union",
          "jefe_hogar",
          "edad_0_14",
          "edad_15_29",
          "edad_30_44",
          "edad_45_59",
          "region_gran_santo_domingo",
          "region_cibao_norte",
          "region_sur",
          "region_este",
          "educacion_primaria",
          "educacion_secundaria_tecnica",
          "educacion_universitario_postgrado",
          "empresa_rnc",
          "traslado_trabajo",
          "traslado_familiar",
          "traslado_estudios",
          "traslado_salud",
          "traslado_trabajo_emp"
        )
      ),
      term = recode(
        term,
        "(Intercept)" = "Intercepto",
        "sexo_femenino" = "Sexo Femenino",
        "casado_union" = "Casado(a) o Unión Libre",
        "jefe_hogar" = "Jefe de Hogar",
        "edad_0_14" = "Edad 0-14",
        "edad_15_29" = "Edad 15-29",
        "edad_30_44" = "Edad 30-44",
        "edad_45_59" = "Edad 45-59",
        "region_gran_santo_domingo" = "Región Gran Santo Domingo",
        "region_cibao_norte" = "Región Cibao o Norte",
        "region_sur" = "Región Sur",
        "region_este" = "Región Este",
        "educacion_primaria" = "Educación Primaria",
        "educacion_secundaria_tecnica" = "Educación Secundaria Técnica",
        "educacion_universitario_postgrado" = "Educación Universitario o Postgrado",
        "empresa_rnc" = "Empresa Inscrita RNC",
        "traslado_trabajo" = "Traslado por Trabajo",
        "traslado_familiar" = "Traslado por Razón Familiar",
        "traslado_estudios" = "Traslado por Estudios",
        "traslado_salud" = "Traslado por Salud",
        "traslado_trabajo_emp" = "Traslado de Trabajo"
      )
    ) |> arrange(term)
