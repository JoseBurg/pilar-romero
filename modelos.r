library(dplyr)
library(broom)
library(purrr)
library(tidyr)

source("funciones.r")

datos_base <- readRDS("datos/base_datos.RDS")

estimacion <- function(
  data,
  model = "logit",
  variables = variables_determinantes
) {
  modelos <- purrr::map(
    data,
    \(x) {
      glm(
        as.formula(
          paste(
            "migrante ~ ",
            paste(
              variables,
              collapse = " + "
            )
          )
        ),
        family = binomial(link = model),
        data = x
      )
    }
  )

  purrr::map(
    modelos,
    broom::tidy
  ) |>
    dplyr::bind_rows(.id = "year") |>
    dplyr::mutate(
      type = model,
      .before = year
    )
}

every_models <- datos_base |> 
  estimacion() |> 
  bind_rows(
    datos_base |> 
  estimacion(model = "probit"))

# Presentar los resultados ------------------------------------------------
every_models |>
  mutate(
    significancia = case_when(
      p.value < 0.01 ~ "***",
      p.value < 0.05 ~ "**",
      p.value < 0.10 ~ "*",
      TRUE ~ "")) |>
  mutate(
    valor_formateado = paste0(
      sprintf("%.2f", estimate), 
      significancia)) |>
  select(type, year, term, valor_formateado) |> 
  pivot_wider(
    names_from = c(year, type), # Nombres de las nuevas columnas
    values_from = valor_formateado,      # Valores que irán en las celdas
    id_cols = term,                      # Columna que se quedará como filas
    values_fill = ""                     # Rellena celdas vacías con nada en vez de NA
  ) |> 
  mutate(
    term = recode(term, # Mejorar nombres:
                   "(Intercept)" = "Constante",
                   "sexo_femenino" = "Sexo femenino", 
                   "casado_union"= "Casado/Unión",
                   "jefe_hogar"= "Jefe de hogar",
                   "educacion_primaria"= "Educación primaria",
                   "educacion_secundaria_tecnica"= "Educación secundaria/técnica",
                   "educacion_universitario_postgrado"= "Educación universitario/postgrado",
                   "edad_0_14"= "Edad 0-14",
                   "edad_15_29"= "Edad 15-29",
                   "edad_30_44"= "Edad 30-44",
                   "edad_45_59"= "Edad 45-59",
                   "region_cibao_norte"= "Región Cibao Norte",
                   "region_sur"= "Región Sur",
                   "region_este"= "Región Este"

    ))
