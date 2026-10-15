# this file contains:....(not finished haha)
df <- read.csv("processed_data.csv")

# Summary table: mean and SD for HEXACO and TWQ
vars <- c("honesty_humility", "emotionality", "extraversion", 
          "agreeableness", "conscientiousness", "openness",
          "communication", "coordination", "balance",
          "support", "commitment", "cohesion")

summary_table <- data.frame(
  variable = vars,
  mean = sapply(df[, vars], mean, na.rm = TRUE),
  sd = sapply(df[, vars], sd, na.rm = TRUE)
)

summary_table$type <- c(rep("HEXACO", 6), rep("TWQ", 6))
summary_table$mean <- round(summary_table$mean, 2)
summary_table$sd <- round(summary_table$sd, 2)

# distributions of HEXACO and TWQ
df %>%
  pivot_longer(cols = all_of(vars),
               names_to = "variable",
               values_to = "value") %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 15, fill = "#28778D", color = "white") +
  facet_wrap(~ variable, scales = "free") +
  theme_minimal() +
  labs(title = "Distributions of HEXACO and TWQ scores")

# correlation analysis for every dimension
cor_vars <- c("honesty_humility", "emotionality", "extraversion",
              "agreeableness", "conscientiousness", "openness",
              "communication", "coordination", "balance",
              "support", "commitment", "cohesion")

cor_mat <- cor(df[, cor_vars], use = "pairwise.complete.obs")
round(cor_mat, 2)

# correlation analysis for TWQ_OVERALL
hexaco_vars <- c("honesty_humility", "emotionality", "extraversion",
                 "agreeableness", "conscientiousness", "openness")

cor_hexaco_overall <- cor(df[, hexaco_vars],
                          df$twq_overall,
                          use = "pairwise.complete.obs")

round(cor_hexaco_overall, 2)

# correlation heatmap
library(reshape2)

melt(cor_mat) %>%
  ggplot(aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), size = 3) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                       midpoint = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "", y = "", fill = "r")








# an example of plotting
# I chose "agreeableness" (HEXACO) and "communication" (TWQ) as an example,
# because they showed the strongest correlation.
library(ggplot2)

ggplot(df, aes(x = agreeableness, y = communication)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(
    x = "Agreebleness",
    y = "Communication",
    title = "Agreebleness vs Communication"
  ) +
  theme_classic()

# also here is the example of regression.
# regression: does agreeableness predict communication?
model <- lm(communication ~ agreeableness, data = df)
summary(model)
