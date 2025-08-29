library(dplyr)
library(readxl)

enfct_2019 <- read_excel("datos/base ENCFT 2019 44.xlsx")
enfct_2020 <- read_excel("./datos/BASE ENCFT 2020 44.xlsx")
enfct_2021 <- read_excel("./datos/BASE ENCFT 2021 44.xlsx")
enfct_2022 <- read_excel("./datos/BASE ENCFT 2022 44.xlsx")
enfct_2023 <- read_excel("./datos/BASE ENCFT 2023 44.xlsx")
enfct_2024 <- read_excel("./datos/BASE ENCFT 2024 44.xlsx")

# Merge usando rbind() para  Combinar todas las filas

base_completa <- rbind(
  base_ENCFT_2019_44,
  BASE_ENCFT_2020_44,
  BASE_ENCFT_2021_44,
  BASE_ENCFT_2022_44,
  BASE_ENCFT_2023_44,
  BASE_ENCFT_2024_44
)

base_completa <- rbind(
  enfct_2019,
  enfct_2020,
  enfct_2021,
  enfct_2022,
  enfct_2023,
  enfct_2024
)

# CLASIFICACIÓN DE REGIONES
# Crear variable de región basada en RESIDENCIA ACTUAL (DES_PROVINCIA)
base_completa <- base_completa %>%
  mutate(
    REGION = case_when(
      # CIBAO o NORTE
      DES_PROVINCIA %in% c("MONTE CRISTI", "PUERTO PLATA", "ESPAILLAT", 
                           "MARÍA TRINIDAD SÁNCHEZ", "HERMANAS MIRABAL",
                           "DUARTE", "SAMANÁ", "LA VEGA", "MONSEÑOR NOUEL",
                           "SÁNCHEZ RAMÍREZ", "SANTIAGO", "VALVERDE") ~ "Cibao o  Norte",
      
      # ESTE (incluyendo Monte Plata) 
      DES_PROVINCIA %in% c("EL SEIBO", "HATO MAYOR", "LA ALTAGRACIA",
                           "LA ROMANA", "SAN PEDRO DE MACORÍS", "MONTE PLATA") ~ "Este",
      
      # SUR
      DES_PROVINCIA %in% c("AZUA", "BAORUCO", "BARAHONA", "INDEPENDENCIA",
                           "PEDERNALES", "SAN CRISTÓBAL", "SAN JOSÉ DE OCOA",
                           "SAN JUAN", "PERAVIA") ~ "Sur",
      
      
      # GRAN SANTO DOMINGO (solo DN y Santo Domingo)
      DES_PROVINCIA %in% c("DISTRITO NACIONAL", "SANTO DOMINGO") ~ "Gran Santo Domingo",
      
      TRUE ~ "otras"
    )
    # ,             La variable de provincia de nacimiento ya esta configurada por región ----------------
    # 
    # # También crear región de ORIGEN para análisis de flujos
    # REGION_ORIGEN = case_when(
    #   # CIBAO NORTE
    #   PROVINCIA_NACIMIENTO %in% c("MONTE CRISTI", "PUERTO PLATA", "ESPAILLAT", 
    #                               "MARÍA TRINIDAD SÁNCHEZ", "HERMANAS MIRABAL",
    #                               "DUARTE", "SAMANÁ", "LA VEGA", "MONSEÑOR NOUEL",
    #                               "SÁNCHEZ RAMÍREZ", "SANTIAGO", "VALVERDE") ~ "Cibao Norte",
    #   
    #   # ESTE (incluyendo Monte Plata) 
    #   PROVINCIA_NACIMIENTO %in% c("EL SEIBO", "HATO MAYOR", "LA ALTAGRACIA",
    #                               "LA ROMANA", "SAN PEDRO DE MACORÍS", "MONTE PLATA") ~ "Este",
    #   
    #   # SUR
    #   PROVINCIA_NACIMIENTO %in% c("AZUA", "BAORUCO", "BARAHONA", "INDEPENDENCIA",
    #                               "PEDERNALES", "SAN CRISTÓBAL", "SAN JOSÉ DE OCOA",
    #                               "SAN JUAN", "PERAVIA") ~ "Sur",
    #   
    #   # GRAN SANTO DOMINGO (solo DN y Santo Domingo)
    #   PROVINCIA_NACIMIENTO %in% c("DISTRITO NACIONAL", "SANTO DOMINGO") ~ "Gran Santo Domingo",
    #   
    #   TRUE ~ "otras"
    # )
  )




# Evitar conflictos de funciones
select <- dplyr::select
filter <- dplyr::filter
mutate <- dplyr::mutate

# Función para verificar la base de datos
verificar_base_datos <- function() {
  if(exists("base_completa")) {
    cat("✓ base_completa encontrada - Dimensiones:", nrow(base_completa), "x", ncol(base_completa), "\n")
    
    # Verificar variables clave
    vars_clave <- c("SEXO", "EDAD", "ESTADO_CIVIL", "NO_ES_BENEFICIARIO",
                    "Relacion con jefe del hogar", "REGION", "EMPRESA_INSCRITA_RNC",
                    "RAZON_TRASLADO", "NIVEL_ULTIMO_ANO_APROBADO", "ANO")
    vars_encontradas <- vars_clave[vars_clave %in% names(base_completa)]
    vars_faltantes <- vars_clave[!vars_clave %in% names(base_completa)]
    
    cat("Variables encontradas:", paste(vars_encontradas, collapse = ", "), "\n")
    if(length(vars_faltantes) > 0) {
      cat("Variables faltantes:", paste(vars_faltantes, collapse = ", "), "\n")
    }
    
    # Mostrar años disponibles
    if("ANO" %in% names(base_completa)) {
      anos_disponibles <- sort(unique(base_completa$ANO))
      cat("Años disponibles:", paste(anos_disponibles, collapse = ", "), "\n")
    }
    
    return(TRUE)
  } else {
    cat("✗ base_completa NO ENCONTRADA\n")
    cat("Por favor, asegúrese de que la base de datos esté cargada en el environment\n")
    return(FALSE)
  }
}

# Función para preparar datos de un año específico
preparar_datos_año <- function(data, year) {
  
  cat("\nPreparando datos para año", year, "...\n")
  
  # Filtrar por año si la columna existe
  if("ANO" %in% names(data)) {
    data_year <- data %>% filter(ANO == year)
  } else {
    data_year <- data
    cat("Advertencia: No se encontró la columna ANO, procesando todos los datos\n")
  }
  
  cat("Registros para año", year, ":", nrow(data_year), "\n")
  
  data_clean <- data_year %>%
    mutate(
      # Sexo dummy (1 = Femenino, 0 = Masculino)
      sexo_femenino = case_when(
        SEXO == "F" ~ 1,
        SEXO == "M" ~ 0,
        is.na(SEXO) ~ 0,  # Asignar masculino por defecto
        TRUE ~ 0
      ),
      
      # Crear dummy para estado civil (1 = casado/unión libre, 0 = otros)
      casado_union = case_when(
        ESTADO_CIVIL %in% c("Casado(a)", "Unión libre", "Casado", "Union libre") ~ 1,
        ESTADO_CIVIL %in% c("Soltero(a)", "Divorciado(a)", "Viudo(a)", "Soltero", "Divorciado", "Viudo", "Separado(a)", "Separado") ~ 0,
        is.na(ESTADO_CIVIL) ~ 0,  # Asignar 0 a NA
        TRUE ~ 0
      ),
      
      # Crear grupos de edad MODIFICADO - Incluye 15-29 años
      grupo_edad = case_when(
        EDAD >= 0 & EDAD <= 14 ~ "0-14",
        EDAD >= 15 & EDAD <= 29 ~ "15-29",
        EDAD >= 30 & EDAD <= 44 ~ "30-44",
        EDAD >= 45 & EDAD <= 59 ~ "45-59",
        EDAD >= 60 ~ "60+",
        is.na(EDAD) ~ "15-29",  # Asignar grupo modal a NA
        TRUE ~ "15-29"
      ),
      
      # Variable dependiente MODIFICADA: NO_ES_BENEFICIARIO
      no_es_beneficiario = case_when(
        NO_ES_BENEFICIARIO == "No" ~ 0,  # Es beneficiario
        NO_ES_BENEFICIARIO == "Si" ~ 1,  # NO es beneficiario
        is.na(NO_ES_BENEFICIARIO) ~ 0,   # Asignar 0 a NA
        TRUE ~ 0
      ),
      
      # Relación con jefe del hogar
      jefe_hogar = case_when(
        `Relacion con jefe del hogar` == "1" |
          `Relacion con jefe del hogar` == 1 ~ 1,  # Es jefe
        `Relacion con jefe del hogar` == "0" |
          `Relacion con jefe del hogar` == 0 ~ 0,  # No es jefe
        is.na(`Relacion con jefe del hogar`) ~ 0,  # Asignar 0 a NA
        TRUE ~ 0
      ),
      
      # Región MODIFICADA - Según tus categorías específicas
      region_cat = case_when(
        REGION == "Gran Santo Domingo" ~ "Gran_Santo_Domingo",
        REGION == "Cibao Norte" | REGION == "Cibao" | REGION == "Norte" ~ "Cibao_Norte",
        REGION == "Sur" ~ "Sur",
        REGION == "Este" ~ "Este",
        is.na(REGION) ~ "Gran_Santo_Domingo",  # Asignar región modal a NA
        TRUE ~ "Gran_Santo_Domingo"
      ),
      
      # Empresa inscrita en RNC
      empresa_rnc = case_when(
        EMPRESA_INSCRITA_RNC == "Si" ~ 1,
        EMPRESA_INSCRITA_RNC == "No" ~ 0,
        is.na(EMPRESA_INSCRITA_RNC) ~ 0,  # Asignar 0 a NA
        TRUE ~ 0
      ),
      
      # Razón de traslado
      razon_traslado_cat = case_when(
        RAZON_TRASLADO == "Buscar Trabajo" |
          RAZON_TRASLADO == "Buscar trabajo" ~ "Trabajo",
        RAZON_TRASLADO == "Razon familiar" |
          RAZON_TRASLADO == "Razón familiar" ~ "Familiar",
        RAZON_TRASLADO == "Estudios" ~ "Estudios",
        RAZON_TRASLADO == "Salud" ~ "Salud",
        RAZON_TRASLADO == "Traslado de trabajo" ~ "Traslado_trabajo",
        is.na(RAZON_TRASLADO) ~ "No_aplica",  # Asignar categoría por defecto a NA
        TRUE ~ "No_aplica"
      ),
      
      # Nivel educativo MODIFICADO - Tres categorías
      educacion_cat = case_when(
        NIVEL_ULTIMO_ANO_APROBADO == "Ninguno" ~ "Ninguno",
        NIVEL_ULTIMO_ANO_APROBADO == "Primario" ~ "Primaria",
        NIVEL_ULTIMO_ANO_APROBADO %in% c("Secundario", "Secundario Tecnico") ~ "Secundaria_Tecnica",
        NIVEL_ULTIMO_ANO_APROBADO %in% c("Universitario", "Postgrado", "Maestría", "Doctorado", "Maestria") ~ "Universitario_Postgrado",
        is.na(NIVEL_ULTIMO_ANO_APROBADO) ~ "Ninguno",  # Asignar ninguno a NA
        TRUE ~ "Ninguno"
      )
    )
  
  # Filtrar casos con datos esenciales (solo verificar variables críticas)
  data_clean <- data_clean %>%
    filter(
      !is.na(no_es_beneficiario),
      !is.na(sexo_femenino)
    )
  
  cat("Filas válidas para año", year, ":", nrow(data_clean), "\n")
  
  return(data_clean)
}

# Función para crear variables dummy MODIFICADA - TODAS LAS CATEGORÍAS INCLUIDAS
crear_dummies <- function(data) {
  data %>%
    mutate(
      # Dummies para TODOS los grupos de edad
      edad_0_14 = ifelse(grupo_edad == "0-14", 1, 0),
      edad_15_29 = ifelse(grupo_edad == "15-29", 1, 0),
      edad_30_44 = ifelse(grupo_edad == "30-44", 1, 0),
      edad_45_59 = ifelse(grupo_edad == "45-59", 1, 0),
      edad_60_mas = ifelse(grupo_edad == "60+", 1, 0),
      
      # Dummies para TODAS las regiones
      region_gran_santo_domingo = ifelse(region_cat == "Gran_Santo_Domingo", 1, 0),
      region_cibao_norte = ifelse(region_cat == "Cibao_Norte", 1, 0),
      region_sur = ifelse(region_cat == "Sur", 1, 0),
      region_este = ifelse(region_cat == "Este", 1, 0),
      
      # Dummies para TODOS los niveles de educación
      educacion_ninguno = ifelse(educacion_cat == "Ninguno", 1, 0),
      educacion_primaria = ifelse(educacion_cat == "Primaria", 1, 0),
      educacion_secundaria_tecnica = ifelse(educacion_cat == "Secundaria_Tecnica", 1, 0),
      educacion_universitario_postgrado = ifelse(educacion_cat == "Universitario_Postgrado", 1, 0),
      
      # Dummies para razón de traslado (mantenemos No_aplica como referencia para evitar colinealidad)
      traslado_no_aplica = ifelse(razon_traslado_cat == "No_aplica", 1, 0),
      traslado_trabajo = ifelse(razon_traslado_cat == "Trabajo", 1, 0),
      traslado_familiar = ifelse(razon_traslado_cat == "Familiar", 1, 0),
      traslado_estudios = ifelse(razon_traslado_cat == "Estudios", 1, 0),
      traslado_salud = ifelse(razon_traslado_cat == "Salud", 1, 0),
      traslado_trabajo_emp = ifelse(razon_traslado_cat == "Traslado_trabajo", 1, 0)
    )
}

# Función para ajustar modelos jerárquicos (6 modelos) MODIFICADA
ajustar_modelos_año <- function(data, year) {
  
  cat("Ajustando modelos para año", year, "...\n")
  
  # Función auxiliar para manejar errores en modelos
  ajustar_modelo_seguro <- function(formula, family = NULL, data) {
    tryCatch({
      if(is.null(family)) {
        lm(formula, data = data)
      } else {
        glm(formula, family = family, data = data)
      }
    }, error = function(e) {
      cat("Error en modelo:", deparse(formula), "\n")
      cat("Error:", e$message, "\n")
      return(NULL)
    })
  }
  
  # VARIABLE DEPENDIENTE CAMBIADA A no_es_beneficiario
  
  # Modelo 1: Solo sexo
  modelo1_logit <- ajustar_modelo_seguro(no_es_beneficiario ~ sexo_femenino,
                                         binomial(link = "logit"), data)
  modelo1_probit <- ajustar_modelo_seguro(no_es_beneficiario ~ sexo_femenino,
                                          binomial(link = "probit"), data)
  modelo1_lpm <- ajustar_modelo_seguro(no_es_beneficiario ~ sexo_femenino, data = data)
  
  # Modelo 2: + Estado civil + Jefe de hogar
  modelo2_logit <- ajustar_modelo_seguro(no_es_beneficiario ~ sexo_femenino + casado_union + jefe_hogar,
                                         binomial(link = "logit"), data)
  modelo2_probit <- ajustar_modelo_seguro(no_es_beneficiario ~ sexo_femenino + casado_union + jefe_hogar,
                                          binomial(link = "probit"), data)
  modelo2_lpm <- ajustar_modelo_seguro(no_es_beneficiario ~ sexo_femenino + casado_union + jefe_hogar, data = data)
  
  # Modelo 3: + Edad (TODAS las categorías de edad)
  modelo3_logit <- ajustar_modelo_seguro(no_es_beneficiario ~ sexo_femenino + casado_union + jefe_hogar +
                                           edad_15_29 + edad_30_44 + edad_45_59 + edad_60_mas,
                                         binomial(link = "logit"), data)
  modelo3_probit <- ajustar_modelo_seguro(no_es_beneficiario ~ sexo_femenino + casado_union + jefe_hogar +
                                            edad_15_29 + edad_30_44 + edad_45_59 + edad_60_mas,
                                          binomial(link = "probit"), data)
  modelo3_lpm <- ajustar_modelo_seguro(no_es_beneficiario ~ sexo_femenino + casado_union + jefe_hogar +
                                         edad_15_29 + edad_30_44 + edad_45_59 + edad_60_mas, data = data)
  
  # Modelo 4: + Región + Educación (TODAS las categorías)
  modelo4_logit <- ajustar_modelo_seguro(no_es_beneficiario ~ sexo_femenino + casado_union + jefe_hogar +
                                           edad_15_29 + edad_30_44 + edad_45_59 + edad_60_mas +
                                           region_gran_santo_domingo + region_cibao_norte + region_sur + region_este +
                                           educacion_primaria + educacion_secundaria_tecnica + educacion_universitario_postgrado,
                                         binomial(link = "logit"), data)
  modelo4_probit <- ajustar_modelo_seguro(no_es_beneficiario ~ sexo_femenino + casado_union + jefe_hogar +
                                            edad_15_29 + edad_30_44 + edad_45_59 + edad_60_mas +
                                            region_gran_santo_domingo + region_cibao_norte + region_sur + region_este +
                                            educacion_primaria + educacion_secundaria_tecnica + educacion_universitario_postgrado,
                                          binomial(link = "probit"), data)
  modelo4_lpm <- ajustar_modelo_seguro(no_es_beneficiario ~ sexo_femenino + casado_union + jefe_hogar +
                                         edad_15_29 + edad_30_44 + edad_45_59 + edad_60_mas +
                                         region_gran_santo_domingo + region_cibao_norte + region_sur + region_este +
                                         educacion_primaria + educacion_secundaria_tecnica + educacion_universitario_postgrado, data = data)
  
  # Modelo 5: + Empresa RNC
  modelo5_logit <- ajustar_modelo_seguro(no_es_beneficiario ~ sexo_femenino + casado_union + jefe_hogar +
                                           edad_15_29 + edad_30_44 + edad_45_59 + edad_60_mas +
                                           region_gran_santo_domingo + region_cibao_norte + region_sur + region_este +
                                           educacion_primaria + educacion_secundaria_tecnica + educacion_universitario_postgrado +
                                           empresa_rnc,
                                         binomial(link = "logit"), data)
  modelo5_probit <- ajustar_modelo_seguro(no_es_beneficiario ~ sexo_femenino + casado_union + jefe_hogar +
                                            edad_15_29 + edad_30_44 + edad_45_59 + edad_60_mas +
                                            region_gran_santo_domingo + region_cibao_norte + region_sur + region_este +
                                            educacion_primaria + educacion_secundaria_tecnica + educacion_universitario_postgrado +
                                            empresa_rnc,
                                          binomial(link = "probit"), data)
  modelo5_lpm <- ajustar_modelo_seguro(no_es_beneficiario ~ sexo_femenino + casado_union + jefe_hogar +
                                         edad_15_29 + edad_30_44 + edad_45_59 + edad_60_mas +
                                         region_gran_santo_domingo + region_cibao_norte + region_sur + region_este +
                                         educacion_primaria + educacion_secundaria_tecnica + educacion_universitario_postgrado +
                                         empresa_rnc, data = data)
  
  # Modelo 6: Modelo completo + Razones de traslado (mantenemos traslado como antes para evitar colinealidad perfecta)
  modelo6_logit <- ajustar_modelo_seguro(no_es_beneficiario ~ sexo_femenino + casado_union + jefe_hogar +
                                           edad_15_29 + edad_30_44 + edad_45_59 + edad_60_mas +
                                           region_gran_santo_domingo + region_cibao_norte + region_sur + region_este +
                                           educacion_primaria + educacion_secundaria_tecnica + educacion_universitario_postgrado +
                                           empresa_rnc + traslado_trabajo + traslado_familiar +
                                           traslado_estudios + traslado_salud + traslado_trabajo_emp,
                                         binomial(link = "logit"), data)
  modelo6_probit <- ajustar_modelo_seguro(no_es_beneficiario ~ sexo_femenino + casado_union + jefe_hogar +
                                            edad_15_29 + edad_30_44 + edad_45_59 + edad_60_mas +
                                            region_gran_santo_domingo + region_cibao_norte + region_sur + region_este +
                                            educacion_primaria + educacion_secundaria_tecnica + educacion_universitario_postgrado +
                                            empresa_rnc + traslado_trabajo + traslado_familiar +
                                            traslado_estudios + traslado_salud + traslado_trabajo_emp,
                                          binomial(link = "probit"), data)
  modelo6_lpm <- ajustar_modelo_seguro(no_es_beneficiario ~ sexo_femenino + casado_union + jefe_hogar +
                                         edad_15_29 + edad_30_44 + edad_45_59 + edad_60_mas +
                                         region_gran_santo_domingo + region_cibao_norte + region_sur + region_este +
                                         educacion_primaria + educacion_secundaria_tecnica + educacion_universitario_postgrado +
                                         empresa_rnc + traslado_trabajo + traslado_familiar +
                                         traslado_estudios + traslado_salud + traslado_trabajo_emp, data = data)
  
  return(list(
    logit = list(modelo1_logit, modelo2_logit, modelo3_logit, modelo4_logit, modelo5_logit, modelo6_logit),
    probit = list(modelo1_probit, modelo2_probit, modelo3_probit, modelo4_probit, modelo5_probit, modelo6_probit),
    lpm = list(modelo1_lpm, modelo2_lpm, modelo3_lpm, modelo4_lpm, modelo5_lpm, modelo6_lpm)
  ))
}

# Función para crear tabla de resultados (6 modelos)
crear_tabla_resultados_año <- function(modelos, year, tipo_modelo = "logit") {
  
  models_list <- modelos[[tipo_modelo]]
  
  # Filtrar modelos que no son NULL
  models_validos <- models_list[!sapply(models_list, is.null)]
  
  if(length(models_validos) == 0) {
    cat("No hay modelos válidos para", tipo_modelo, "en año", year, "\n")
    return(NULL)
  }
  
  # Extraer coeficientes
  coef_table <- map_dfr(seq_along(models_list), ~{
    model <- models_list[[.x]]
    if(!is.null(model)) {
      tidy_model <- tidy(model)
      tidy_model$modelo <- paste0("Modelo ", .x)
      return(tidy_model)
    } else {
      return(data.frame())
    }
  })
  
  if(nrow(coef_table) == 0) {
    return(NULL)
  }
  
  # Preparar tabla con formato
  tabla_wide <- coef_table %>%
    dplyr::select(term, estimate, p.value, modelo) %>%
    mutate(
      coef_format = case_when(
        p.value < 0.001 ~ paste0(sprintf("%.3f", estimate), "***"),
        p.value < 0.01 ~ paste0(sprintf("%.3f", estimate), "**"),
        p.value < 0.05 ~ paste0(sprintf("%.3f", estimate), "*"),
        TRUE ~ sprintf("%.3f", estimate)
      )
    ) %>%
    dplyr::select(term, coef_format, modelo) %>%
    tidyr::pivot_wider(names_from = modelo, values_from = coef_format,
                       values_fill = "")
  
  # Renombrar variables para mejor presentación - TODAS LAS CATEGORÍAS
  if("term" %in% names(tabla_wide)) {
    tabla_wide <- tabla_wide %>%
      mutate(
        term = case_when(
          term == "(Intercept)" ~ "Constante",
          term == "sexo_femenino" ~ "Sexo (Femenino = 1)",
          term == "casado_union" ~ "Estado Civil (Casado/Unión = 1)",
          term == "jefe_hogar" ~ "Jefe de Hogar (1 = Sí)",
          # TODAS las categorías de edad
          term == "edad_0_14" ~ "Edad 0-14 años",
          term == "edad_15_29" ~ "Edad 15-29 años",
          term == "edad_30_44" ~ "Edad 30-44 años",
          term == "edad_45_59" ~ "Edad 45-59 años",
          term == "edad_60_mas" ~ "Edad 60+ años",
          # TODAS las regiones
          term == "region_gran_santo_domingo" ~ "Región Gran Santo Domingo",
          term == "region_cibao_norte" ~ "Región Cibao Norte",
          term == "region_sur" ~ "Región Sur",
          term == "region_este" ~ "Región Este",
          # TODOS los niveles educativos
          term == "educacion_ninguno" ~ "Educación: Ninguno",
          term == "educacion_primaria" ~ "Educación: Primaria",
          term == "educacion_secundaria_tecnica" ~ "Educación: Secundaria/Técnica",
          term == "educacion_universitario_postgrado" ~ "Educación: Universitaria/Postgrado",
          # Variables de empresa y traslado
          term == "empresa_rnc" ~ "Empresa RNC (Sí = 1)",
          term == "traslado_no_aplica" ~ "Traslado: No aplica",
          term == "traslado_trabajo" ~ "Traslado: Buscar trabajo",
          term == "traslado_familiar" ~ "Traslado: Razón familiar",
          term == "traslado_estudios" ~ "Traslado: Estudios",
          term == "traslado_salud" ~ "Traslado: Salud",
          term == "traslado_trabajo_emp" ~ "Traslado: Traslado trabajo",
          TRUE ~ term
        )
      )
  }
  
  # Calcular estadísticas
  stats_table <- map_dfr(seq_along(models_list), ~{
    model <- models_list[[.x]]
    if(!is.null(model)) {
      n_obs <- nobs(model)
      
      if(tipo_modelo %in% c("logit", "probit")) {
        pseudo_r2 <- 1 - (model$deviance / model$null.deviance)
        aic_val <- AIC(model)
        bic_val <- BIC(model)
      } else {
        pseudo_r2 <- summary(model)$r.squared
        aic_val <- AIC(model)
        bic_val <- BIC(model)
      }
      
      data.frame(
        modelo = paste0("Modelo ", .x),
        n_obs = n_obs,
        pseudo_r2 = sprintf("%.4f", pseudo_r2),
        aic = sprintf("%.1f", aic_val),
        bic = sprintf("%.1f", bic_val)
      )
    } else {
      data.frame()
    }
  })
  
  return(list(coeficientes = tabla_wide, estadisticas = stats_table))
}

# Función NUEVA para imprimir TODAS las tablas automáticamente
imprimir_todas_las_tablas <- function() {
  
  if(length(resultados_por_año) == 0) {
    cat("No hay resultados disponibles para mostrar\n")
    return(NULL)
  }
  
  # Tipos de modelo disponibles
  tipos_modelo <- c("logit", "probit", "lpm")
  nombres_modelo <- c("LOGÍSTICO", "PROBIT", "LINEAL DE PROBABILIDAD (LPM)")
  
  for(year_key in names(resultados_por_año)) {
    year <- as.numeric(year_key)
    
    cat("\n")
    cat(paste(rep("█", 100), collapse = ""), "\n")
    cat("                           ANÁLISIS COMPLETO - AÑO", year, "\n")
    cat("                     VARIABLE DEPENDIENTE: NO ES BENEFICIARIO", "\n")
    cat(paste(rep("█", 100), collapse = ""), "\n\n")
    
    for(i in seq_along(tipos_modelo)) {
      tipo_modelo <- tipos_modelo[i]
      nombre_modelo <- nombres_modelo[i]
      
      if(!tipo_modelo %in% names(resultados_por_año[[year_key]])) {
        next
      }
      
      resultado <- resultados_por_año[[year_key]][[tipo_modelo]]
      
      if(is.null(resultado)) {
        cat("No hay resultados válidos para", nombre_modelo, "en año", year, "\n\n")
        next
      }
      
      # Título del modelo
      cat(paste(rep("═", 90), collapse = ""), "\n")
      cat("                        MODELOS", nombre_modelo, "- AÑO", year, "\n")
      cat(paste(rep("═", 90), collapse = ""), "\n\n")
      
      # Tabla de coeficientes
      header_name <- paste("Modelos", nombre_modelo, year)
      col_names <- c("Variable", "Modelo 1", "Modelo 2", "Modelo 3", "Modelo 4", "Modelo 5", "Modelo 6")
      caption_text <- paste("Coeficientes - Modelos", nombre_modelo, "- Año", year)
      
      tabla_coef <- resultado$coeficientes %>%
        kable(format = "html",
              caption = caption_text,
              col.names = col_names,
              escape = FALSE) %>%
        kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                      full_width = TRUE, position = "center", font_size = 11) %>%
        add_header_above(setNames(c(1, 6), c(" ", header_name))) %>%
        column_spec(1, bold = TRUE, width = "4cm") %>%
        row_spec(0, bold = TRUE, color = "white", background = "#2c3e50") %>%
        footnote(general = "Niveles de significancia: *** p<0.001, ** p<0.01, * p<0.05",
                 general_title = "Nota: ")
      
      print(tabla_coef)
      
      cat("\n")
      
      # Tabla de estadísticas
      caption_stats <- paste("Estadísticas de Ajuste - Modelos", nombre_modelo, "- Año", year)
      col_names_stats <- c("Modelo", "N observaciones", "Pseudo R²", "AIC", "BIC")
      
      tabla_stats <- resultado$estadisticas %>%
        kable(format = "html",
              caption = caption_stats,
              col.names = col_names_stats,
              escape = FALSE) %>%
        kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                      full_width = FALSE, position = "center", font_size = 11) %>%
        column_spec(1, bold = TRUE) %>%
        row_spec(0, bold = TRUE, color = "white", background = "#27ae60")
      
      print(tabla_stats)
      
      cat("\n\n")
    }
    
    cat(paste(rep("▓", 100), collapse = ""), "\n")
    cat("                        FIN ANÁLISIS AÑO", year, "\n")
    cat(paste(rep("▓", 100), collapse = ""), "\n\n")
  }
}

# Función para imprimir tablas individuales por año (mantener función original)
imprimir_tabla_año <- function(year, tipo_modelo) {
  
  year_key <- as.character(year)
  
  if(!year_key %in% names(resultados_por_año)) {
    cat("No hay resultados para el año:", year, "\n")
    cat("Años disponibles:", paste(names(resultados_por_año), collapse = ", "), "\n")
    return(NULL)
  }
  
  if(!tipo_modelo %in% names(resultados_por_año[[year_key]])) {
    cat("Tipo de modelo no disponible:", tipo_modelo, "\n")
    return(NULL)
  }
  
  resultado <- resultados_por_año[[year_key]][[tipo_modelo]]
  
  if(is.null(resultado)) {
    cat("No hay resultados válidos para", tipo_modelo, "en año", year, "\n")
    return(NULL)
  }
  
  # Título principal
  cat("\n")
  cat(paste(rep("=", 80), collapse = ""), "\n")
  cat("MODELOS DE REGRESIÓN - VARIABLE DEPENDIENTE: NO ES BENEFICIARIO - AÑO", year, "\n")
  cat(paste(rep("=", 80), collapse = ""), "\n\n")
  
  # Tabla de coeficientes
  header_name <- paste("Modelos", toupper(tipo_modelo), year)
  col_names <- c("Variable", "Modelo 1", "Modelo 2", "Modelo 3", "Modelo 4", "Modelo 5", "Modelo 6")
  caption_text <- paste("Coeficientes - Modelos", toupper(tipo_modelo), "- Año", year)
  
  tabla_coef <- resultado$coeficientes %>%
    kable(format = "html",
          caption = caption_text,
          col.names = col_names,
          escape = FALSE) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                  full_width = TRUE, position = "center", font_size = 12) %>%
    add_header_above(setNames(c(1, 6), c(" ", header_name))) %>%
    column_spec(1, bold = TRUE, width = "3cm") %>%
    row_spec(0, bold = TRUE, color = "white", background = "#2c3e50") %>%
    footnote(general = "Niveles de significancia: *** p<0.001, ** p<0.01, * p<0.05",
             general_title = "Nota: ")
  
  print(tabla_coef)
  
  cat("\n")
  
  # Tabla de estadísticas
  caption_stats <- paste("Estadísticas de Ajuste - Modelos", toupper(tipo_modelo), "- Año", year)
  col_names_stats <- c("Modelo", "N observaciones", "Pseudo R²", "AIC", "BIC")
  
  tabla_stats <- resultado$estadisticas %>%
    kable(format = "html",
          caption = caption_stats,
          col.names = col_names_stats,
          escape = FALSE) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                  full_width = FALSE, position = "center", font_size = 12) %>%
    column_spec(1, bold = TRUE) %>%
    row_spec(0, bold = TRUE, color = "white", background = "#27ae60")
  
  print(tabla_stats)
  
  cat("\n")
}

# EJECUCIÓN PRINCIPAL CON IMPRESIÓN AUTOMÁTICA DE TODAS LAS TABLAS
cat("=== INICIANDO ANÁLISIS CON BASE_COMPLETA ===\n")

# Verificar base de datos
if(!verificar_base_datos()) {
  stop("No se puede proceder sin la base de datos 'base_completa'")
}

# Obtener años disponibles
if("ANO" %in% names(base_completa)) {
  anos_disponibles <- sort(unique(base_completa$ANO))
  cat("\n=== AÑOS ENCONTRADOS ===\n")
  cat("Procesando años:", paste(anos_disponibles, collapse = ", "), "\n")
} else {
  anos_disponibles <- c(2019)  # Año por defecto si no existe columna ANO
  cat("\nAdvertencia: No se encontró columna ANO, procesando como año 2019\n")
}

# Procesar cada año
resultados_por_año <- list()

for(year in anos_disponibles) {
  cat("\n=== PROCESANDO AÑO", year, "===\n")
  
  # Preparar datos
  datos_prep <- preparar_datos_año(base_completa, year)
  
  if(nrow(datos_prep) > 0) {
    datos_finales <- crear_dummies(datos_prep)
    
    # Ajustar modelos
    modelos <- ajustar_modelos_año(datos_finales, year)
    
    # Crear tablas
    resultados_por_año[[as.character(year)]] <- list(
      logit = crear_tabla_resultados_año(modelos, year, "logit"),
      probit = crear_tabla_resultados_año(modelos, year, "probit"),
      lpm = crear_tabla_resultados_año(modelos, year, "lpm")
    )
    
    cat("✓ Completado procesamiento para año", year, "\n")
  } else {
    cat("✗ No hay datos válidos para año", year, "\n")
  }
}

# Mostrar resumen
cat("\n=== RESUMEN FINAL ===\n")
cat("Años procesados exitosamente:", paste(names(resultados_por_año), collapse = ", "), "\n")

cat("\n=== NOTAS METODOLÓGICAS ===\n")
cat("• EDAD: Se omite edad 0-14 del modelo para evitar colinealidad perfecta\n")
cat("• REGIÓN: Se omite educación 'Ninguno' del modelo para evitar colinealidad perfecta\n")
cat("• EDUCACIÓN: Todas las categorías incluidas en los datos\n")
cat("• TRASLADO: Se omite 'No aplica' para evitar colinealidad perfecta\n")
cat("• Variable dependiente: NO_ES_BENEFICIARIO (1 = No es beneficiario, 0 = Es beneficiario)\n")
cat("• TODAS las categorías aparecen en las tablas de resultados\n\n")

# *** IMPRESIÓN AUTOMÁTICA DE TODAS LAS TABLAS ***
cat("=== INICIANDO IMPRESIÓN AUTOMÁTICA DE TODAS LAS TABLAS ===\n")
imprimir_todas_las_tablas()

cat("\n=== ANÁLISIS COMPLETADO ===\n")
cat("✓ Se han impreso automáticamente todas las tablas de todos los años\n")
cat("✓ Para imprimir tablas individuales, use: imprimir_tabla_año(AÑO, 'TIPO_MODELO')\n")
cat("✓ Para reimprimir todas las tablas, use: imprimir_todas_las_tablas()\n\n")

# Mostrar instrucciones adicionales
cat("=== FUNCIONES DISPONIBLES ===\n")
cat("1. imprimir_todas_las_tablas()          # Imprime todas las tablas de todos los años\n")
cat("2. imprimir_tabla_año(AÑO, 'MODELO')    # Imprime tabla específica\n")
for(year in names(resultados_por_año)) {
  cat("   - imprimir_tabla_año(", year, ", 'logit')\n", sep="")
  cat("   - imprimir_tabla_año(", year, ", 'probit')\n", sep="")
  cat("   - imprimir_tabla_año(", year, ", 'lpm')\n", sep="")
}

cat("\n¡ANÁLISIS COMPLETADO CON ÉXITO!\n")


# Replicando los modelos uno a uno ----------------------------------------

# Filtrar datos para 2019
data_2019 <- base_completa %>%
  filter(ANO == 2019)

# Preparar datos
data_2019_prep <- preparar_datos_año(data_2019, 2019)
data_2019_final <- crear_dummies(data_2019_prep)

variables_independientes <- lis

# Ajustar modelos uno a uno
model1_logit <- glm(no_es_beneficiario ~ sexo_femenino,
                    family = binomial(link = "logit"), data = data_2019_final)
summary(model1_logit)

model1_probit <- glm(no_es_beneficiario ~ sexo_femenino,
                     family = binomial(link = "probit"), data = data_2019_final)
summary(model1_probit)


coe


