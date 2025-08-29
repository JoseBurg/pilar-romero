library(broom)
library(tidyverse)
library(modelr)



datos_aninados <- bind_rows(datos_base, .id = "year") |> 
  group_by(year) |> 
  nest()


modelo_logit <- function(datos, especificacion){
  formula <- paste("no_es_beneficiario ~", paste(especificacion, collapse = " + "))
  
  glm(as.formula(formula), family = binomial(link = "logit"), data = datos)

}


map(datos_aninados$data, ~ modelo_logit(.x, especificacion = variables_modelo6))




# Revisa la relación entre 'jefe_hogar' y tu variable dependiente
table(
  Jefe_de_hogar = datos_base[[1]]$jefe_hogar, 
  No_es_Beneficiario = datos_base[[1]]$no_es_beneficiario
)

# Revisa la relación entre 'casado_union' y tu variable dependiente
table(
  Casado_o_Union = datos_base[[1]]$casado_union, 
  No_es_Beneficiario = datos_base[[1]]$no_es_beneficiario
)

# Usa una de tus bases de datos, por ejemplo, la primera de la lista.

# Revisa la variable 'jefe_hogar'
table(
  Jefe_Hogar = datos_base[[1]]$jefe_hogar, 
  Resultado = datos_base[[1]]$no_es_beneficiario
)

# Revisa la variable 'casado_union'
table(
  Casado_Union = datos_base[[1]]$casado_union, 
  Resultado = datos_base[[1]]$no_es_beneficiario
)
