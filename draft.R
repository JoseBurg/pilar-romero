library(broom)
library(tidyverse)
library(modelr)




# Cargar datos: -----------------------------------------------------------

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

variables_determinantes <- c("sexo_femenino", "casado_union", "jefe_hogar",
                             "educacion_primaria", "educacion_secundaria_tecnica", 
                             "educacion_universitario_postgrado",
                             "region_cibao_norte", "region_sur", "region_este")


datos_base <- purrr::map(
  datos_base,
  ~.x |> 
    select(migrante, all_of(variables_determinantes)) |> 
    mutate(
      across(
        everything(),
          as.numeric
        )
      )
  )

names(datos_base) <- 2019:2024

datos_aninados <- bind_rows(datos_base, .id = "year") |> 
  group_by(year) |> 
  nest()


modelo <- function(datos, especificacion = variables_determinantes, type = NULL){
  formula <- paste("migrante ~", paste(especificacion, collapse = " + "))
  
  if(is.null(type)) {
      lm(as.formula(formula), data = datos)
  } else {
      glm(as.formula(formula), family = binomial(link = type), data = datos)
    }
}



datos_aninados |> 
  mutate(
    modelo_lineal = map(data, ~ modelo(.x)),
    modelo_probit = map(data, ~ modelo(.x, type = "probit")),
    modelo_logit = map(data, ~  modelo(.x, type = "logit"))
    ) |> 
  mutate(
    residual_lineal = map2(data, modelo_lineal, add_residuals)
    ) |> 
  unnest(residual_lineal) |> 
  as.data.frame()
  
  


