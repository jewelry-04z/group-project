df <- read.csv("https://redivis.com/api/v1/tables/datapages.item_response_warehouse_2:v33_0.zaehl2023_hexaco/rows?format=csv")

library(tidyr)

# convert the data frame from long format to wide format
# so that each row represents one participant
df_wide <- df %>%
  pivot_wider(
    id_cols = c(id, cov_gender, cov_age, cov_degree),
    names_from = item,
    values_from = resp
  )

# reorder all item columns from F01 to F60 for a cleaner dataset
cov_cols <- c("id", "cov_gender", "cov_age", "cov_degree")

item_cols <- names(df_wide)[grepl("^F\\d+", names(df_wide))]
item_cols <- item_cols[order(as.numeric(gsub("[^0-9]", "", item_cols)))]

df_wide <- df_wide[, c(cov_cols, item_cols)]

# identify and reverse-score the reverse items
reverse_items <- c(
  # Honesty-Humility
  "F12r", "F24r", "F30r", "F42r", "F48r", "F60r",
  # Emotionality
  "F35r", "F41r", "F53r", "F59r",
  # Extraversion
  "F10r", "F28r", "F46r", "F52r",
  # Agreeableness
  "F09r", "F15r", "F21r", "F57r",
  # Conscientiousness
  "F14r", "F20r", "F26r", "F32r", "F44r", "F56r",
  # Openness
  "F01r", "F31r", "F49r"
)

reverse_items <- intersect(reverse_items, names(df_wide))

for (item in reverse_items){
  df_wide[[item]] <- 6 - df_wide[[item]]
}

# calculate scores for each HEXACO dimension
H_items <- c("F06", "F30r", "F54", "F12r", "F36", "F60r",
             "F18", "F42r", "F24r", "F48r")

E_items <- c("F05", "F29", "F53r", "F11", "F35r",
             "F17", "F41r", "F23", "F47", "F59r")

X_items <- c("F04", "F28r", "F52r", "F10r", "F34", "F58",
             "F16", "F40", "F22", "F46r")

A_items <- c("F03", "F27", "F09r", "F33", "F51",
             "F15r", "F39", "F57r", "F21r", "F45")

C_items <- c("F02", "F26r", "F08", "F32r", "F14r", "F38", "F50",
             "F20r", "F44r", "F56r")

O_items <- c("F01r", "F25", "F07", "F31r", "F13", "F37",
             "F49r", "F19", "F43", "F55")

df_wide$H <- rowMeans(df_wide[, H_items], na.rm = TRUE)
df_wide$E <- rowMeans(df_wide[, E_items], na.rm = TRUE)
df_wide$X <- rowMeans(df_wide[, X_items], na.rm = TRUE)
df_wide$A <- rowMeans(df_wide[, A_items], na.rm = TRUE)
df_wide$C <- rowMeans(df_wide[, C_items], na.rm = TRUE)
df_wide$O <- rowMeans(df_wide[, O_items], na.rm = TRUE)
