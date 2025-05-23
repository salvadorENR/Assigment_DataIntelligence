# Instalar paquetes si es necesario
# install.packages("tidyverse")
# install.packages("writexl")

library(tidyverse)
library(writexl)

# 1. Cargar los datos con codificación UTF-8
df <- read_csv2("Candidatos_Muestra.csv", locale = locale(encoding = "UTF-8"))

# 2. Eliminar duplicados por COD_CE
escuelas_unicas <- df %>% 
  distinct(COD_CE, .keep_all = TRUE) %>%
  mutate(escuela_id = row_number())  # Identificador único

# 3. Clasificar las escuelas
escuelas_unicas <- escuelas_unicas %>%
  mutate(
    clasificacion = case_when(
      grepl("Centro Escolar", NOMBRE_CE, ignore.case = TRUE) & DOCENTES <= 5 ~ "UNI_TRI",
      grepl("Parvularia", NOMBRE_CE, ignore.case = TRUE) ~ "Parvularia_y_Especiales",
      grepl("Centro Escolar", NOMBRE_CE, ignore.case = TRUE) & DOCENTES > 5 ~ "Centro_Escolar",
      grepl("Instituto", NOMBRE_CE, ignore.case = TRUE) ~ "Institutos",
      grepl("Complejo Educativo", NOMBRE_CE, ignore.case = TRUE) ~ "Complejos",
      TRUE ~ "NO_CLASIFICADA"
    )
  )

# 4. Tabla de muestra requerida
requerido <- tribble(
  ~NOMBRE_DEPTO, ~clasificacion, ~n,
  "AHUACHAPÁN", "UNI_TRI", 15, "AHUACHAPÁN", "Parvularia_y_Especiales", 1, "AHUACHAPÁN", "Centro_Escolar", 8,
  "AHUACHAPÁN", "Institutos", 1, "AHUACHAPÁN", "Complejos", 3,
  "CHALATENANGO", "UNI_TRI", 25, "CHALATENANGO", "Parvularia_y_Especiales", 2, "CHALATENANGO", "Centro_Escolar", 10,
  "CHALATENANGO", "Institutos", 3, "CHALATENANGO", "Complejos", 1,
  "USULUTÁN", "UNI_TRI", 22, "USULUTÁN", "Parvularia_y_Especiales", 2, "USULUTÁN", "Centro_Escolar", 13,
  "USULUTÁN", "Institutos", 2, "USULUTÁN", "Complejos", 4,
  "LA LIBERTAD", "UNI_TRI", 18, "LA LIBERTAD", "Parvularia_y_Especiales", 3, "LA LIBERTAD", "Centro_Escolar", 17,
  "LA LIBERTAD", "Institutos", 2, "LA LIBERTAD", "Complejos", 4,
  "SONSONATE", "UNI_TRI", 14, "SONSONATE", "Parvularia_y_Especiales", 1, "SONSONATE", "Centro_Escolar", 10,
  "SONSONATE", "Institutos", 1, "SONSONATE", "Complejos", 4,
  "SAN MIGUEL", "UNI_TRI", 23, "SAN MIGUEL", "Parvularia_y_Especiales", 2, "SAN MIGUEL", "Centro_Escolar", 17,
  "SAN MIGUEL", "Institutos", 2, "SAN MIGUEL", "Complejos", 3,
  "SAN SALVADOR", "UNI_TRI", 9, "SAN SALVADOR", "Parvularia_y_Especiales", 7, "SAN SALVADOR", "Centro_Escolar", 29,
  "SAN SALVADOR", "Institutos", 3, "SAN SALVADOR", "Complejos", 9,
  "LA PAZ", "UNI_TRI", 14, "LA PAZ", "Parvularia_y_Especiales", 2, "LA PAZ", "Centro_Escolar", 10,
  "LA PAZ", "Institutos", 2, "LA PAZ", "Complejos", 4,
  "SANTA ANA", "UNI_TRI", 24, "SANTA ANA", "Parvularia_y_Especiales", 2, "SANTA ANA", "Centro_Escolar", 14,
  "SANTA ANA", "Institutos", 1, "SANTA ANA", "Complejos", 4,
  "MORAZÁN", "UNI_TRI", 20, "MORAZÁN", "Parvularia_y_Especiales", 2, "MORAZÁN", "Centro_Escolar", 9,
  "MORAZÁN", "Institutos", 1, "MORAZÁN", "Complejos", 2,
  "CABAÑAS", "UNI_TRI", 16, "CABAÑAS", "Parvularia_y_Especiales", 1, "CABAÑAS", "Centro_Escolar", 7,
  "CABAÑAS", "Institutos", 1, "CABAÑAS", "Complejos", 1,
  "CUSCATLÁN", "UNI_TRI", 8, "CUSCATLÁN", "Parvularia_y_Especiales", 1, "CUSCATLÁN", "Centro_Escolar", 8,
  "CUSCATLÁN", "Institutos", 1, "CUSCATLÁN", "Complejos", 2,
  "LA UNIÓN", "UNI_TRI", 25, "LA UNIÓN", "Parvularia_y_Especiales", 1, "LA UNIÓN", "Centro_Escolar", 9,
  "LA UNIÓN", "Institutos", 1, "LA UNIÓN", "Complejos", 3,
  "SAN VICENTE", "UNI_TRI", 11, "SAN VICENTE", "Parvularia_y_Especiales", 1, "SAN VICENTE", "Centro_Escolar", 8,
  "SAN VICENTE", "Institutos", 1, "SAN VICENTE", "Complejos", 2
)

# 5. Verificar cobertura previa
verificar_subrepresentacion <- function(escuelas, requerido) {
  escuelas %>%
    filter(clasificacion != "NO_CLASIFICADA") %>%
    count(NOMBRE_DEPTO, clasificacion) %>%
    right_join(requerido, by = c("NOMBRE_DEPTO", "clasificacion")) %>%
    mutate(
      n_disponibles = replace_na(n.x, 0),
      n_requeridos = n.y,
      diferencia = n_requeridos - n_disponibles,
      subrepresentado = diferencia > 0
    ) %>%
    select(NOMBRE_DEPTO, clasificacion, n_disponibles, n_requeridos, diferencia, subrepresentado)
}

analisis_cobertura <- verificar_subrepresentacion(escuelas_unicas, requerido)
subrepresentados <- analisis_cobertura %>% filter(subrepresentado)

# 6. Selección muestral
set.seed(123)
escuelas_unicas <- escuelas_unicas %>% mutate(seleccionada = FALSE)

muestra_final <- requerido %>%
  group_by(NOMBRE_DEPTO, clasificacion) %>%
  group_modify(~ {
    depto <- .y$NOMBRE_DEPTO
    clase <- .y$clasificacion
    n <- .x$n[1]
    
    disponibles <- escuelas_unicas %>%
      filter(NOMBRE_DEPTO == depto, clasificacion == clase)
    
    if (nrow(disponibles) == 0) {
      return(tibble())
    } else if (nrow(disponibles) >= n) {
      seleccionadas <- slice_sample(disponibles, n = n)
      escuelas_unicas <<- escuelas_unicas %>%
        mutate(seleccionada = ifelse(escuela_id %in% seleccionadas$escuela_id, TRUE, seleccionada))
      seleccionadas %>% select(-NOMBRE_DEPTO, -clasificacion)
    } else {
      escuelas_unicas <<- escuelas_unicas %>%
        mutate(seleccionada = ifelse(escuela_id %in% disponibles$escuela_id, TRUE, seleccionada))
      disponibles %>% select(-NOMBRE_DEPTO, -clasificacion)
    }
  }) %>%
  ungroup()

# 7. Escuelas excluidas (con clasificación)
escuelas_excluidas <- escuelas_unicas %>%
  filter(!seleccionada) %>%
  select(COD_CE, NOMBRE_CE, NOMBRE_DEPTO, clasificacion, SECTOR, DOCENTES)

# 8. Verificación posterior
analisis_post_muestreo <- verificar_subrepresentacion(muestra_final, requerido) %>%
  left_join(analisis_cobertura %>% select(NOMBRE_DEPTO, clasificacion, n_disponibles),
            by = c("NOMBRE_DEPTO", "clasificacion"))

# 9. Resultado final con clasificación incluida
muestra_final <- muestra_final %>%
  select(NOMBRE_DEPTO, COD_DISTRITO, COD_CE, NOMBRE_CE, clasificacion)

# 10. Resumen por departamento y categoría
tabla_base <- muestra_final %>%
  count(NOMBRE_DEPTO, clasificacion) %>%
  pivot_wider(names_from = clasificacion, values_from = n, values_fill = 0)

resumen_muestra <- tabla_base %>%
  mutate(Total = rowSums(across(where(is.numeric)))) %>%
  bind_rows(
    tabla_base %>%
      summarise(across(where(is.numeric), sum)) %>%
      mutate(NOMBRE_DEPTO = "TOTALES") %>%
      relocate(NOMBRE_DEPTO)
  )

# 11. Exportar resultados a archivo Excel
write_xlsx(list(
  Muestra_Final = muestra_final,
  Escuelas_Excluidas = escuelas_excluidas,
  Analisis_Cobertura = analisis_cobertura,
  Resumen_Muestra = resumen_muestra,
  Subrepresentacion_Post = analisis_post_muestreo
), "Muestra_Escuelas_Publicas_Completa.xlsx")

# 12. Mensajes resumen
cat("\nDepartamentos/clasificaciones subrepresentados:\n")
print(subrepresentados)

cat("\nVerificación de cobertura:\n")
cat("- Total escuelas disponibles:", nrow(escuelas_unicas), "\n")
cat("- Total escuelas en muestra:", nrow(muestra_final), "\n")
cat("- Total escuelas excluidas:", nrow(escuelas_excluidas), "\n")
