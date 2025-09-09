library(readxl)
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
    mutate(
      across(
        everything(),
          as.numeric
        )
      )
  )



names(datos_base) <- 2019:2024


saveRDS(datos_base, "base_datos.RDS")
