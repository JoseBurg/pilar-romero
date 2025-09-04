# Funcion para llevar de provincia a region
provincia_to_region <- function(data) {
  data %>%
    janitor::clean_names() %>%
    dplyr::mutate(region = dplyr::case_when(
      des_provincia %in% c("MONTE CRISTI", "PUERTO PLATA", "ESPAILLAT",
                           "MARÍA TRINIDAD SÁNCHEZ", "HERMANAS MIRABAL",
                           "DUARTE", "SAMANÁ", "LA VEGA", "MONSEÑOR NOUEL",
                           "SÁNCHEZ RAMÍREZ", "SANTIAGO", "VALVERDE") ~ "Cibao o  Norte",
      des_provincia %in% c("EL SEIBO", "HATO MAYOR", "LA ALTAGRACIA",
                           "LA ROMANA", "SAN PEDRO DE MACORÍS", "MONTE PLATA") ~ "Este",
      des_provincia %in% c("AZUA", "BAORUCO", "BARAHONA", "INDEPENDENCIA",
                           "PEDERNALES", "SAN CRISTÓBAL", "SAN JOSÉ DE OCOA",
                           "SAN JUAN", "PERAVIA") ~ "Sur",
      des_provincia %in% c("DISTRITO NACIONAL", "SANTO DOMINGO") ~ "Gran Santo Domingo",
      TRUE ~ "otras"
    ))
}

# Funcion transformar variables para los modelos:
transformar_variables <- function(data) {
    data |>
     dplyr::mutate(
       # Transformar variables según sea necesario
       # Ejemplo: crear variable dummy para sexo
       ano = as.numeric(ano),
       edad = as.numeric(edad),
       migrante = ifelse(is.na(razon_traslado), 0, 1),
       sexo_femenino = ifelse(sexo == "F", 1, 0),
       # Crear variable dummy para estado civil
       casado_union = ifelse(estado_civil %in% c("Casado(a)", "Unión libre"), 1, 0),
       # variable dependiente
       no_es_beneficiario = as.character(no_es_beneficiario),
       no_es_beneficiario = ifelse(stringr::str_detect(no_es_beneficiario, "Si|1"), 1, 0),
       jefe_hogar = relacion_con_jefe_del_hogar,
    #    empresa_rnc = ifelse(empresa_inscrita_rnc == "Si", 1, 0),
       jefe_hogar = as.character(jefe_hogar),
       jefe_hogar = ifelse(stringr::str_detect(no_es_beneficiario, "jefe de hogar|1"), 1, 0),
       empresa_rnc = dplyr::case_when(
         empresa_inscrita_rnc == "Si" ~ 1,
         empresa_inscrita_rnc == "No" ~ 0,
         TRUE ~ NA
       ),
       razon_traslado = dplyr::case_when(
         stringr::str_detect(razon_traslado, "Buscar Trabajo") ~ "Trabajo",
        #  stringr::str_detect(razon_traslado, "Traslado de Trabajo") ~ "Traslado_trabajo",
        is.na(razon_traslado) ~ "No_aplica",
        TRUE ~ razon_traslado
       ),
       educacion_cat = dplyr::case_when(
        stringr::str_detect(nivel_ultimo_ano_aprobado, "Ninguno") ~ "Ninguno",
        stringr::str_detect(nivel_ultimo_ano_aprobado, "Primario") ~ "Primaria",
        stringr::str_detect(nivel_ultimo_ano_aprobado, "Secundario|Secundario Tecnico") ~ "Secundaria_Tecnica",
        stringr::str_detect(nivel_ultimo_ano_aprobado, "Universitario|Postgrado|Maestría|Doctorado|Maestria") ~ "Universitario_Postgrado",
        TRUE ~ "No_aplica"
    ))
}

variables_dummy <- function(data) {
    data |>
     dplyr::mutate(
        # variable grupo edad
       edad_range = dplyr::case_when(
         edad >= 0 & edad <= 14 ~ "0-14",
         edad >= 15 & edad <= 29 ~ "15-29",
         edad >= 30 & edad <= 44 ~ "30-44",
         edad >= 45 & edad <= 59 ~ "45-59",
         edad >= 60 ~ "60+"
       ),
       # Variables que vamos a  cambiar pero efientizado con case_when:
       edad_0_14 = ifelse(edad_range== "0-14", 1, 0),
       edad_15_29 = ifelse(edad_range== "15-29", 1, 0),
       edad_30_44 = ifelse(edad_range== "30-44", 1, 0),
       edad_45_59 = ifelse(edad_range== "45-59", 1, 0),
       edad_60_mas = ifelse(edad_range== "60+", 1, 0),

       # Dummies para TODAS las regiones
       region_gran_santo_domingo = ifelse(region == "Gran Santo Domingo", 1, 0),
       region_cibao_norte = ifelse(region == "Cibao o  Norte", 1, 0),
       region_sur = ifelse(region == "Sur", 1, 0),
       region_este = ifelse(region == "Este", 1, 0),

       # Dummies para TODOS los niveles de educación
       educacion_ninguno = ifelse(educacion_cat == "Ninguno", 1, 0),
       educacion_primaria = ifelse(educacion_cat == "Primaria", 1, 0),
       educacion_secundaria_tecnica = ifelse(educacion_cat == "Secundaria_Tecnica", 1, 0),
       educacion_universitario_postgrado = ifelse(educacion_cat == "Universitario_Postgrado", 1, 0),

       # Dummies para razón de traslado (mantenemos No_aplica como referencia para evitar colinealidad)
       traslado_no_aplica = ifelse(razon_traslado == "No_aplica", 1, 0),
       traslado_trabajo = ifelse(razon_traslado == "Trabajo", 1, 0),
       traslado_familiar = ifelse(razon_traslado == "Razon familiar", 1, 0),
       traslado_estudios = ifelse(razon_traslado == "Estudios", 1, 0),
       traslado_salud = ifelse(razon_traslado == "Salud", 1, 0),
       traslado_trabajo_emp = ifelse(razon_traslado == "Traslado de trabajo", 1, 0)
     )
}

variables_modelo1 <- c("sexo_femenino")
variables_modelo2 <- c("sexo_femenino", "casado_union", "jefe_hogar")
variables_modelo3 <- c("sexo_femenino", "casado_union", "jefe_hogar",
                       "edad_0_14","edad_15_29", "edad_30_44", "edad_45_59")
variables_modelo4 <- c("sexo_femenino", "casado_union", "jefe_hogar",
                       "edad_0_14","edad_15_29", "edad_30_44", "edad_45_59",
                       "region_gran_santo_domingo", "region_cibao_norte", "region_sur", "region_este",
                       "educacion_primaria", "educacion_secundaria_tecnica", "educacion_universitario_postgrado")
variables_modelo5 <- c("sexo_femenino", "casado_union", "jefe_hogar",
                       "edad_0_14","edad_15_29", "edad_30_44", "edad_45_59",
                       "region_gran_santo_domingo", "region_cibao_norte", "region_sur", "region_este",
                       "educacion_primaria", "educacion_secundaria_tecnica", "educacion_universitario_postgrado",
                       "empresa_rnc")
variables_modelo6 <- c("sexo_femenino", "casado_union", "jefe_hogar",
                        "edad_0_14", "edad_15_29", "edad_30_44", "edad_45_59",
                         "region_gran_santo_domingo", "region_cibao_norte", "region_sur", "region_este",
                         "educacion_primaria", "educacion_secundaria_tecnica", "educacion_universitario_postgrado",
                         "empresa_rnc"
                       # , "traslado_trabajo", "traslado_familiar",
                       #   "traslado_estudios", "traslado_salud", "traslado_trabajo_emp"
                       )

variables_determinantes <- c("sexo_femenino", "casado_union", "jefe_hogar",
                             "educacion_primaria", "educacion_secundaria_tecnica", 
                             "educacion_universitario_postgrado",
                             "region_cibao_norte", "region_sur", "region_este")

