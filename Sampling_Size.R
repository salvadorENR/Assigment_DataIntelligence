# Load required libraries
library(dplyr)
library(purrr)

# 1. Define the population data
schools_df <- data.frame(
  Departamento = c("Ahuachapán", "Chalatenango", "Usulután", "La Libertad",
                   "Sonsonate", "San Miguel", "San Salvador", "La Paz",
                   "Santa Ana", "Morazán", "Cabañas", "Cuscatlán", 
                   "La Unión", "San Vicente"),
  Escuelas_Publicas = c(280, 413, 449, 437, 310, 468, 583, 308, 
                        455, 331, 266, 208, 378, 235),
  stringsAsFactors = FALSE
)

# 2. Define total sample size (10% of 4,921 schools)
total_sample <- 492

# 3. Define weights from the updated image for categories x1-x5
weights_from_image <- data.frame(
  Departamento = c("Ahuachapán", "Chalatenango", "Usulután", "La Libertad",
                   "Sonsonate", "San Miguel", "San Salvador", "La Paz",
                   "Santa Ana", "Morazán", "Cabañas", "Cuscatlán", 
                   "La Unión", "San Vicente"),
  x1 = c(0.17, 0.55, 0.31, 0.18, 0.17, 0.28, 0.06, 0.22, 0.36, 0.44, 0.45, 0.21, 0.43, 0.35),
  x2 = c(0.03, 0.05, 0.04, 0.06, 0.04, 0.03, 0.14, 0.05, 0.05, 0.05, 0.03, 0.06, 0.02, 0.05),
  x3 = c(0.65, 0.30, 0.51, 0.64, 0.63, 0.58, 0.60, 0.56, 0.49, 0.41, 0.43, 0.60, 0.46, 0.46),
  x4 = c(0.04, 0.07, 0.05, 0.04, 0.03, 0.04, 0.05, 0.05, 0.01, 0.04, 0.03, 0.04, 0.02, 0.05),
  x5 = c(0.12, 0.03, 0.09, 0.08, 0.13, 0.06, 0.15, 0.11, 0.09, 0.06, 0.05, 0.09, 0.07, 0.09),
  stringsAsFactors = FALSE
)

# 4. Calculate proportional quotas for departments
schools_df <- schools_df %>%
  mutate(
    Muestra_total = round((Escuelas_Publicas / sum(Escuelas_Publicas)) * total_sample),
    Muestra_total = pmax(Muestra_total, 1)
  )

# Adjust rounding errors
diff <- total_sample - sum(schools_df$Muestra_total)
schools_df$Muestra_total[which.max(schools_df$Escuelas_Publicas)] <- 
  schools_df$Muestra_total[which.max(schools_df$Escuelas_Publicas)] + diff

# 5. Merge and calculate category quotas
schools_final <- schools_df %>%
  left_join(weights_from_image, by = "Departamento") %>%
  mutate(
    across(x1:x5, ~ round(.x * Muestra_total), .names = "Muestra_{col}")
  )

# 6. Fix rounding errors per department
schools_final <- schools_final %>%
  rowwise() %>%
  mutate(
    sum_diff = Muestra_total - sum(c_across(starts_with("Muestra_x"))),
    # Apply the difference to the category with largest weight (x3 in most cases)
    Muestra_x3 = Muestra_x3 + sum_diff
  ) %>%
  ungroup()

# 7. View final allocation with all columns visible
final_allocation <- schools_final %>%
  select(Departamento, Escuelas_Publicas, Muestra_total, 
         x1, x2, x3, x4, x5,
         Muestra_x1, Muestra_x2, Muestra_x3, Muestra_x4, Muestra_x5)

# Print with all columns visible
options(tibble.width = Inf)
print(final_allocation, n = Inf)

# 8. Verify totals
cat("\nVerification:\n")
cat("Total sample size:", sum(final_allocation$Muestra_total), "\n")
cat("Category totals:\n")
cat("x1 (UNI_TRI):", sum(final_allocation$Muestra_x1), "\n")
cat("x2 (Parvularia):", sum(final_allocation$Muestra_x2), "\n")
cat("x3 (Centro_Escolar):", sum(final_allocation$Muestra_x3), "\n")
cat("x4 (institutos):", sum(final_allocation$Muestra_x4), "\n")
cat("x5 (complejos):", sum(final_allocation$Muestra_x5), "\n")