# ==============================================================================
# Script da Atividade 2: Meta-análise do Experimento da Biblioteca Omar Catunda
# Disciplina: MATD28 - Planejamento de Experimentos (UFBA)
# Alunos: Luca Barboza e Vitor Freitas (Grupo 4)
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(metafor)
})

cat(">>> Carregando dados dos grupos...\n")
caminho_dados <- file.path("dados", "dados_consolidados.csv")
if (!file.exists(caminho_dados)) {
  stop("Arquivo de dados consolidados não encontrado: ", caminho_dados)
}

dados_todos <- read.csv(caminho_dados)

grupos <- unique(dados_todos$grupo)
resultados <- list()

cat(">>> Ajustando ANOVA e calculando métricas de efeito por grupo...\n")
for (g in grupos) {
  df_g <- dados_todos %>% filter(grupo == g)
  mod <- aov(paginas ~ prateleira, data = df_g)
  tab <- summary(mod)[[1]]
  
  ss_entre <- tab[["Sum Sq"]][1]
  ss_res   <- tab[["Sum Sq"]][2]
  ss_total <- sum(tab[["Sum Sq"]])
  f_val    <- tab[["F value"]][1]
  p_val    <- tab[["Pr(>F)"]][1]
  
  # 1. Eta-quadrado
  eta2 <- ss_entre / ss_total
  
  # 2. d de Cohen e Variância
  n <- nrow(df_g) # n = 24
  d <- 2 * sqrt(eta2 / (1 - eta2))
  var_d <- (4 / n) * (1 + (d^2 / 8))
  se_d  <- sqrt(var_d)
  
  # Intervalo de confiança individual de 95%
  ci_lb <- d - 1.96 * se_d
  ci_ub <- d + 1.96 * se_d
  
  resultados[[g]] <- data.frame(
    grupo = g,
    n = n,
    ss_entre = ss_entre,
    ss_res = ss_res,
    ss_total = ss_total,
    F_val = f_val,
    p = p_val,
    eta2 = eta2,
    d = d,
    var_d = var_d,
    se_d = se_d,
    ci_lb = ci_lb,
    ci_ub = ci_ub
  )
}

tabela_consolidada <- do.call(rbind, resultados)

cat("\n================ TABELA CONSOLIDADA ================\n")
print(tabela_consolidada[, c("grupo", "n", "eta2", "p", "d", "var_d", "ci_lb", "ci_ub")])

# 3. Ajuste do Modelo de Efeitos Aleatórios (REML)
cat("\n>>> Ajustando Modelo de Efeitos Aleatórios (REML) via metafor...\n")
meta <- rma(yi = d, vi = var_d, data = tabela_consolidada, method = "REML")
print(summary(meta))

# 4. Avaliação da Heterogeneidade
cat("\n================ HETEROGENEIDADE ================\n")
cat("Teste Q de Cochran (QE) =", round(meta$QE, 4), "\n")
cat("p-valor de Q (QEp)      =", format.pval(meta$QEp, digits = 5), "\n")
cat("I² (heterogeneidade)    =", round(meta$I2, 2), "%\n")
cat("tau² (variância entre)  =", round(meta$tau2, 4), "\n")
cat("tau                     =", round(sqrt(meta$tau2), 4), "\n")

# 5. Geração do Forest Plot em PNG
arquivo_plot <- "forest_plot.png"
cat("\n>>> Gerando gráfico Forest Plot:", arquivo_plot, "...\n")
png(arquivo_plot, width = 960, height = 540, res = 130)
par(mar = c(5, 4, 3, 2), family = "sans")

# Rótulos customizados para os grupos
nomes_estudos <- c(
  "Grupo 1 (G1)",
  "Grupo 2 (G2)",
  "Grupo 3 (G3)",
  "Grupo 4 (G4)"
)

forest(
  meta,
  slab = nomes_estudos,
  xlab = "Tamanho de Efeito (d de Cohen)",
  header = c("Grupo Amostral", "d de Cohen [IC 95%]"),
  main = "Efeito da Prateleira no Número de Páginas",
  annotate = TRUE,
  digits = c(2L, 3L),
  col = "#1e40af",
  border = "#1e40af"
)
dev.off()

cat(">>> Concluído com sucesso! Forest Plot salvo em:", arquivo_plot, "\n")
