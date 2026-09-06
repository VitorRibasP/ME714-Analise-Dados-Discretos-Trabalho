library(tidyverse)
library(gam)
library(MASS)
library(DHARMa)
library(kableExtra)

banco <- read.csv("filmes.csv")

########################################################
################### AJUSTES INICIAIS ###################
########################################################

#Remover informações faltantes
banco <- banco %>% filter(budget != 0 & revenue != 0)

#Observações únicas
banco <- unique(banco)

# SELEÇÃO ----------------------------------------------------------------------

#Selecionar apenas o primeiro gênero
banco$genres_adap <- sub("\\['([^']+)'.*", "\\1", banco$genres)
sort(table(banco$genres_adap), decreasing = TRUE)
sort(prop.table(table(banco$genres_adap)), decreasing = TRUE)

#Idioma original 
sort(table(banco$original_language), decreasing = TRUE)
sort(prop.table(table(banco$original_language)), decreasing = TRUE)

# Selecionar apenas o ano
banco$year <- substr(banco$release_date, 1, 4)
sort(table(banco$year), decreasing = TRUE)
sort(prop.table(table(banco$year)), decreasing = TRUE)

# Selecionar apenas o mês
banco$month <- substr(banco$release_date, 6, 7)
sort(table(banco$month ), decreasing = TRUE)
sort(prop.table(table(banco$month )), decreasing = TRUE)

# AGRUPAMENTOS -----------------------------------------------------------------

#Agrupar gênero
genero <- sort(table(banco$genres_adap), decreasing = TRUE)
genero <- names(genero[genero>100])
banco$genres_adap <- ifelse(banco$genres_adap %in% genero, banco$genres_adap, "Outros")

#Agrupar idioma
idioma <- sort(table(banco$original_language), decreasing = TRUE)
idioma <- names(idioma[idioma>100])
banco$original_language <- ifelse(banco$original_language %in% idioma, banco$original_language, "Outros")

#Estúdios
banco$production_companies_adap <- sub("\\['([^']+)'.*", "\\1", banco$production_companies)
comp <- sort(table(banco$production_companies_adap), decreasing = TRUE)
comp <- names(comp[comp>100])
banco$production_companies_adap <- ifelse(banco$production_companies_adap %in% comp, banco$production_companies_adap, "Outros")

#Remover variáveis 
banco <- banco %>% dplyr::select(-c("id", "title", "production_countries", "genres", "release_date", "production_companies", "production_countries", "cast", "director", "popularity", "vote_count"))
colnames(banco)                          

# TIPO DE VARIAVEL -------------------------------------------------------------

apply(banco, 2, class)
banco$budget <- as.numeric(banco$budget)
banco$revenue <- as.numeric(banco$revenue)
banco$runtime <- as.numeric(banco$runtime)
banco$vote_average <- as.numeric (banco$vote_average)
banco$original_language <- as.factor(banco$original_language)
banco$genres_adap <- as.factor(banco$genres_adap)
banco$year <- as.numeric(banco$year)
banco$month <- as.factor(banco$month)
banco$production_companies_adap <- as.factor(banco$production_companies_adap)

banco <- rename(banco, prod_comp = production_companies_adap)

########################################################
################## ANALISE DESCRITIVA ##################
########################################################

summary(banco %>% dplyr::select(budget, revenue, runtime, vote_average, year))

sapply(banco %>% dplyr::select(budget, revenue, runtime, vote_average, year), sd)

tema_padrao <- theme_bw() +
  theme(
    axis.title.x = element_text(size = 10, face = "bold", colour = "black"),
    axis.title.y = element_text(size = 10, face = "bold", colour = "black"),
    axis.text.x = element_text(size = 9, face = "bold", colour = "black"),
    axis.text.y = element_text(size = 9, face = "bold", colour = "black")
  )

tema_boxplot <- tema_padrao + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(banco, aes(x = revenue / 1e6)) +
  geom_histogram(fill = "cornflowerblue", color = "black", bins = 30) +
  labs(x = "Receita (em Milhões de US$)", y = "Frequência") +
  tema_padrao

ggplot(banco, aes(x = budget / 1e6, y = revenue / 1e6)) +
  geom_point(alpha = 0.6, color = "cornflowerblue") +
  labs(x = "Orçamento (em Milhões de US$)", y = "Receita (em Milhões de US$)") +
  tema_padrao

ggplot(banco, aes(x = runtime, y = revenue / 1e6)) +
  geom_point(alpha = 0.6, color = "cornflowerblue") +
  labs(x = "Duração (em minutos)", y = "Receita (em Milhões de US$)") +
  tema_padrao

ggplot(banco, aes(x = vote_average, y = revenue / 1e6)) +
  geom_point(alpha = 0.6, color = "cornflowerblue") +
  labs(x = "Nota Média (0 a 10)", y = "Receita (em Milhões de US$)") +
  tema_padrao

ggplot(banco, aes(x = year, y = revenue / 1e6)) +
  geom_point(alpha = 0.6, color = "cornflowerblue") +
  labs(x = "Ano de Lançamento", y = "Receita (em Milhões de US$)") +
  tema_padrao

ggplot(banco, aes(x = fct_reorder(genres_adap, revenue, .fun = median, .desc = TRUE), y = revenue / 1e6)) +
  geom_boxplot(fill = "cornflowerblue", color = "black") +
  labs(x = "Gênero Principal", y = "Receita (em Milhões de US$)") +
  tema_boxplot

ggplot(banco, aes(x = fct_reorder(prod_comp, revenue, .fun = median, .desc = TRUE), y = revenue / 1e6)) +
  geom_boxplot(fill = "cornflowerblue", color = "black") +
  labs(x = "Produtora Principal", y = "Receita (em Milhões de US$)") +
  tema_boxplot

ggplot(banco, aes(x = month, y = revenue / 1e6)) +
  geom_boxplot(fill = "cornflowerblue", color = "black") +
  labs(x = "Mês de Lançamento", y = "Receita (em Milhões de US$)") +
  tema_padrao

ggplot(banco, aes(x = fct_reorder(original_language, revenue, .fun = median, .desc = TRUE), y = revenue / 1e6)) +
  geom_boxplot(fill = "cornflowerblue", color = "black") +
  labs(x = "Idioma Original", y = "Receita (em Milhões de US$)") +
  tema_boxplot


########################################################
################### MODELO COMPLETO ####################
########################################################

# MODELAGEM --------------------------------------------------------------------

set.seed(123)
modelo <- gam(revenue ~ s(budget) + s(runtime) + s(vote_average) + 
                s(year) + month + original_language + genres_adap +
                prod_comp,
              family = negative.binomial(theta = 1), data = banco)
#Usar resultados dos testes para coeficientes funcionais
summary(modelo)
#plot(modelo)


kbl(
  summary(modelo)$anova,
  format = "latex",
  booktabs = FALSE,
  align = c("l", "c", "c", "c", "c"),
  caption = "Coeficientes funcionais",
  escape = FALSE
) %>%
  kable_styling(
    latex_options = c("hold_position"),
    position = "center"
  )


res <- simulateResiduals(modelo, n = 5000)
# plot(res)

# TESTE DE WALD (para coeficientes paramétricos) -------------------------------
coef <- coef(modelo)
V <- vcov(modelo)
erro_padrao <- sqrt(diag(V))


z <- coef / erro_padrao

p_valor <- 2 * (1 - pnorm(abs(z)))

resultado <- data.frame(coeficiente = coef,
                        erro_padrao = erro_padrao,
                        Z = z,
                        p_valor = p_valor)


resultado <- resultado %>% mutate(significância = case_when(
  `p_valor` <= 0.001 ~ "***",
  0.001 < `p_valor` & `p_valor` <= 0.01 ~ "**",
  0.01 < `p_valor` & `p_valor` <= 0.05 ~ "*",
  0.05 < `p_valor` & `p_valor` <= 0.1 ~ ".",
  TRUE ~ ""
))

resultado_4 <- resultado %>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))

kbl(
  resultado_4,
  format = "latex",
  booktabs = FALSE,
  align = c("l", "c", "c", "c", "c", "c"),
  caption = "Coeficientes funcionais",
  escape = FALSE
) %>%
  kable_styling(
    latex_options = c("hold_position"),
    position = "center"
  )

## PLOT ------------------------------------------------------------------------

#BUDGET

grid_budget <- data.frame(
  budget = seq(min(banco$budget),max(banco$budget),length.out = 500),
  runtime = mean(banco$runtime),
  vote_average = mean(banco$vote_average),
  year = mean(banco$year),
  month = levels(banco$month)[1],
  original_language = levels(banco$original_language)[1],
  genres_adap = levels(banco$genres_adap)[1],
  prod_comp = levels(banco$prod_comp)[1])

pred <- predict(modelo, newdata = grid_budget, type = "terms", se.fit = TRUE)

dados_plot <- tibble(
  budget = grid_budget$budget,
  efeito = pred[, "s(budget)"]
)

ggplot(dados_plot, aes(x = budget, y = efeito)) +
  geom_line(size = 1.4) +
  theme_bw() +
  labs(x = "budget", y = "f(budget)") +
  theme(
    axis.title.x = element_text(size = 16, face = "bold", colour = "black"),
    axis.title.y = element_text(size = 16, face = "bold",colour = "black"),
    axis.text.x = element_text(size = 14,face = "bold", colour = "black"),
    axis.text.y = element_text(size = 14, face = "bold", colour = "black")
  )

# RUNTIME
grid_runtime <- data.frame(budget = mean(banco$budget),
                           
                           runtime = seq(min(banco$runtime), max(banco$runtime), length.out = 500),
                           vote_average = mean(banco$vote_average),
                           year = mean(banco$year),
                           month = levels(banco$month)[1],
                           original_language = levels(banco$original_language)[1],
                           genres_adap = levels(banco$genres_adap)[1],
                           prod_comp = levels(banco$prod_comp)[1])

pred <- predict(modelo, newdata = grid_runtime, type = "terms")

dados_plot <- tibble(
  runtime = grid_runtime$runtime,
  efeito = pred[, "s(runtime)"])

ggplot(dados_plot, aes(runtime, efeito)) +
  geom_line(size = 1.4) +
  theme_bw() +
  labs(x = "runtime", y = "f(runtime)") +
  theme(
    axis.title.x = element_text(size = 16, face = "bold", colour = "black"),
    axis.title.y = element_text(size = 16, face = "bold",colour = "black"),
    axis.text.x = element_text(size = 14,face = "bold", colour = "black"),
    axis.text.y = element_text(size = 14, face = "bold", colour = "black")
  )

#YEAR

grid_year <- data.frame(
  budget = mean(banco$budget),
  runtime = mean(banco$runtime),
  vote_average = mean(banco$vote_average),
  
  year = seq(
    min(banco$year),
    max(banco$year),
    length.out = 500
  ),
  
  month = levels(banco$month)[1],
  original_language = levels(banco$original_language)[1],
  genres_adap = levels(banco$genres_adap)[1],
  prod_comp = levels(banco$prod_comp)[1]
)

pred <- predict(
  modelo,
  newdata = grid_year,
  type = "terms"
)

dados_plot <- tibble(
  year = grid_year$year,
  efeito = pred[, "s(year)"]
)

ggplot(dados_plot, aes(year, efeito)) +
  geom_line(size = 1.4) +
  theme_bw() +
  labs(x = "year", y = "f(year)") +
  theme(
    axis.title.x = element_text(size = 16, face = "bold", colour = "black"),
    axis.title.y = element_text(size = 16, face = "bold",colour = "black"),
    axis.text.x = element_text(size = 14,face = "bold", colour = "black"),
    axis.text.y = element_text(size = 14, face = "bold", colour = "black")
  )

#VOTE_AVERAGE
grid_vote <- data.frame(
  budget = mean(banco$budget),
  runtime = mean(banco$runtime),
  vote_average = seq(min(banco$vote_average),max(banco$vote_average),length.out = 500),
  year = mean(banco$year),
  month = levels(banco$month)[1],
  original_language = levels(banco$original_language)[1],
  genres_adap = levels(banco$genres_adap)[1],
  prod_comp = levels(banco$prod_comp)[1])

pred <- predict(modelo, newdata = grid_vote, type = "terms")

dados_plot <- tibble(
  vote_average = grid_vote$vote_average,
  efeito = pred[, "s(vote_average)"]
)

ggplot(dados_plot, aes(vote_average, efeito)) +
  geom_line(size = 1.4) +
  theme_bw() +
  labs(x = "vote_average", y = "f(vote_average)") +
  theme(
    axis.title.x = element_text(size = 16, face = "bold", colour = "black"),
    axis.title.y = element_text(size = 16, face = "bold",colour = "black"),
    axis.text.x = element_text(size = 14,face = "bold", colour = "black"),
    axis.text.y = element_text(size = 14, face = "bold", colour = "black")
  )


#Resíduos

ggplot(
  data.frame(residuo = res$scaledResiduals),
  aes(x = residuo)
) +
  geom_histogram(bins = 20, fill = "cornflowerblue", color = "black") +
  labs(
    x = "Resíduo DHARMa",
    y = "Frequência"
  ) +
  theme_bw() +
  theme(
    axis.title.x = element_text(size = 16, face = "bold", colour = "black"),
    axis.title.y = element_text(size = 16, face = "bold",colour = "black"),
    axis.text.x = element_text(size = 14,face = "bold", colour = "black"),
    axis.text.y = element_text(size = 14, face = "bold", colour = "black")
  )

#qqplot
n <- length(res$scaledResiduals)

qq_df <- tibble(
  teorico = ppoints(n),
  observado = sort(res$scaledResiduals)
)

ggplot(qq_df, aes(x = teorico, y = observado)) +
  geom_point(size = 1) +
  geom_abline(
    intercept = 0,
    slope = 1,
    linetype = 2
  ) +
  labs(
    x = "Quantis teóricos U(0,1)",
    y = "Quantis observados") +
  theme_bw() +
  theme(
    axis.title.x = element_text(size = 16, face = "bold", colour = "black"),
    axis.title.y = element_text(size = 16, face = "bold",colour = "black"),
    axis.text.x = element_text(size = 14,face = "bold", colour = "black"),
    axis.text.y = element_text(size = 14, face = "bold", colour = "black")
  )

########################################################
################### MODELO REDUZIDO ####################
########################################################

# MODELAGEM --------------------------------------------------------------------

set.seed(123)
modelo <- gam(revenue ~ s(budget) + s(runtime) + s(year) + 
                vote_average + month + original_language + 
                genres_adap + prod_comp,
              family = negative.binomial(theta = 1), data = banco)
#Usar resultados dos testes para coeficientes funcionais
summary(modelo)
#plot(modelo)


kbl(
  summary(modelo)$anova,
  format = "latex",
  booktabs = FALSE,
  align = c("l", "c", "c", "c", "c"),
  caption = "Coeficientes funcionais",
  escape = FALSE
) %>%
  kable_styling(
    latex_options = c("hold_position"),
    position = "center"
  )


res <- simulateResiduals(modelo, n = 5000)
# plot(res)

# TESTE DE WALD (para coeficientes paramétricos) -------------------------------
coef <- coef(modelo)
V <- vcov(modelo)
erro_padrao <- sqrt(diag(V))


z <- coef / erro_padrao

p_valor <- 2 * (1 - pnorm(abs(z)))

resultado <- data.frame(coeficiente = coef,
                        erro_padrao = erro_padrao,
                        Z = z,
                        p_valor = p_valor)


resultado <- resultado %>% mutate(significância = case_when(
  `p_valor` <= 0.001 ~ "***",
  0.001 < `p_valor` & `p_valor` <= 0.01 ~ "**",
  0.01 < `p_valor` & `p_valor` <= 0.05 ~ "*",
  0.05 < `p_valor` & `p_valor` <= 0.1 ~ ".",
  TRUE ~ ""
))

resultado_4 <- resultado %>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))

kbl(
  resultado_4,
  format = "latex",
  booktabs = FALSE,
  align = c("l", "c", "c", "c", "c", "c"),
  caption = "Coeficientes funcionais",
  escape = FALSE
) %>%
  kable_styling(
    latex_options = c("hold_position"),
    position = "center"
  )

## PLOT ------------------------------------------------------------------------

#BUDGET

grid_budget <- data.frame(
  budget = seq(min(banco$budget),max(banco$budget),length.out = 500),
  runtime = mean(banco$runtime),
  vote_average = mean(banco$vote_average),
  year = mean(banco$year),
  month = levels(banco$month)[1],
  original_language = levels(banco$original_language)[1],
  genres_adap = levels(banco$genres_adap)[1],
  prod_comp = levels(banco$prod_comp)[1])

pred <- predict(modelo, newdata = grid_budget, type = "terms", se.fit = TRUE)

dados_plot <- tibble(
  budget = grid_budget$budget,
  efeito = pred[, "s(budget)"]
)

ggplot(dados_plot, aes(x = budget, y = efeito)) +
  geom_line(size = 1.4) +
  theme_bw() +
  labs(x = "budget", y = "f(budget)") +
  theme(
    axis.title.x = element_text(size = 16, face = "bold", colour = "black"),
    axis.title.y = element_text(size = 16, face = "bold",colour = "black"),
    axis.text.x = element_text(size = 14,face = "bold", colour = "black"),
    axis.text.y = element_text(size = 14, face = "bold", colour = "black")
  )

# RUNTIME
grid_runtime <- data.frame(budget = mean(banco$budget),
                           
                           runtime = seq(min(banco$runtime), max(banco$runtime), length.out = 500),
                           vote_average = mean(banco$vote_average),
                           year = mean(banco$year),
                           month = levels(banco$month)[1],
                           original_language = levels(banco$original_language)[1],
                           genres_adap = levels(banco$genres_adap)[1],
                           prod_comp = levels(banco$prod_comp)[1])

pred <- predict(modelo, newdata = grid_runtime, type = "terms")

dados_plot <- tibble(
  runtime = grid_runtime$runtime,
  efeito = pred[, "s(runtime)"])

ggplot(dados_plot, aes(runtime, efeito)) +
  geom_line(size = 1.4) +
  theme_bw() +
  labs(x = "runtime", y = "f(runtime)") +
  theme(
    axis.title.x = element_text(size = 16, face = "bold", colour = "black"),
    axis.title.y = element_text(size = 16, face = "bold",colour = "black"),
    axis.text.x = element_text(size = 14,face = "bold", colour = "black"),
    axis.text.y = element_text(size = 14, face = "bold", colour = "black")
  )

#YEAR

grid_year <- data.frame(
  budget = mean(banco$budget),
  runtime = mean(banco$runtime),
  vote_average = mean(banco$vote_average),
  
  year = seq(
    min(banco$year),
    max(banco$year),
    length.out = 500
  ),
  
  month = levels(banco$month)[1],
  original_language = levels(banco$original_language)[1],
  genres_adap = levels(banco$genres_adap)[1],
  prod_comp = levels(banco$prod_comp)[1]
)

pred <- predict(
  modelo,
  newdata = grid_year,
  type = "terms"
)

dados_plot <- tibble(
  year = grid_year$year,
  efeito = pred[, "s(year)"]
)

ggplot(dados_plot, aes(year, efeito)) +
  geom_line(size = 1.4) +
  theme_bw() +
  labs(x = "year", y = "f(year)") +
  theme(
    axis.title.x = element_text(size = 16, face = "bold", colour = "black"),
    axis.title.y = element_text(size = 16, face = "bold",colour = "black"),
    axis.text.x = element_text(size = 14,face = "bold", colour = "black"),
    axis.text.y = element_text(size = 14, face = "bold", colour = "black")
  )


#resíduos

ggplot(
  data.frame(residuo = res$scaledResiduals),
  aes(x = residuo)
) +
  geom_histogram(bins = 20, fill = "cornflowerblue", color = "black") +
  labs(
    x = "Resíduo DHARMa",
    y = "Frequência"
  ) +
  theme_bw() +
  theme(
    axis.title.x = element_text(size = 16, face = "bold", colour = "black"),
    axis.title.y = element_text(size = 16, face = "bold",colour = "black"),
    axis.text.x = element_text(size = 14,face = "bold", colour = "black"),
    axis.text.y = element_text(size = 14, face = "bold", colour = "black")
  )

#qqplot
n <- length(res$scaledResiduals)

qq_df <- tibble(
  teorico = ppoints(n),
  observado = sort(res$scaledResiduals)
)

ggplot(qq_df, aes(x = teorico, y = observado)) +
  geom_point(size = 1) +
  geom_abline(
    intercept = 0,
    slope = 1,
    linetype = 2
  ) +
  labs(
    x = "Quantis teóricos U(0,1)",
    y = "Quantis observados") +
  theme_bw() +
  theme(
    axis.title.x = element_text(size = 16, face = "bold", colour = "black"),
    axis.title.y = element_text(size = 16, face = "bold",colour = "black"),
    axis.text.x = element_text(size = 14,face = "bold", colour = "black"),
    axis.text.y = element_text(size = 14, face = "bold", colour = "black")
  )




