# Função auxiliar para facilitar a visualização

dumpMatrix <- function(m, f) {
  colunas <- colnames(m)
  linhas <- rownames(m)
  for(l in 1:length(linhas)){
    for(c in 1:length(colunas)){
      f(linhas[l], colunas[c], m[l,c])
    }
  }
}

# Modelagem de equações estruturais de mínimos quadrados parciais (PLS-SEM) usando R
# Livro https://library.oapen.org/handle/20.500.12657/51463
# DOI 10.1007/978-3-030-80519-7
# Script https://sem-in-r.github.io/seminr/

library(seminr)

# 2.2 Load and inspect the data
data <- read.csv(file = "pls.csv", header = TRUE, sep = ",")

# 2.3.2 Create a measurement model UTAUT2 - Figure 1
mm <- constructs(
  composite("ED", c("ED1", "ED2", "ED3", "ED4")),
  composite("EE", c("EE1", "EE2", "EE3", "EE4")),
  composite("IS", c("IS1", "IS2", "IS3")),

# Mod 1  composite("CF", c("CF1", "CF2", "CF3", "CF4")),
# Mod 2  composite("CF", c("CF2", "CF3")),

  composite("MH", c("MH1", "MH2", "MH3")),
  composite("PR", c("PR1", "PR2", "PR3")),

# Mod 1  composite("HA", c("HA1", "HA2", "HA3", "HA4")),
# Mod 3  composite("HA", c("HA1", "HA3", "HA4")),

  composite("IC", c("IC1", "IC2", "IC3")),
  composite("USO", c("USO")),

  # moderadoras  
  composite("Idade", single_item("Idade")),
  composite("Genero", single_item("Genero")),
  composite("Experiencia", single_item("Experiencia")),
 
  interaction_term(iv = "ED", moderator = "Idade", method = two_stage),
  interaction_term(iv = "ED", moderator = "Genero", method = two_stage),
  
  interaction_term(iv = "EE", moderator = "Idade", method = two_stage),
  interaction_term(iv = "EE", moderator = "Genero", method = two_stage),
  interaction_term(iv = "EE", moderator = "Experiencia", method = two_stage),
  
  interaction_term(iv = "IS", moderator = "Idade", method = two_stage),
  interaction_term(iv = "IS", moderator = "Genero", method = two_stage),
  interaction_term(iv = "IS", moderator = "Experiencia", method = two_stage),
  
# Mod 2  interaction_term(iv = "CF", moderator = "Idade", method = two_stage),
# Mod 2  interaction_term(iv = "CF", moderator = "Genero", method = two_stage),
# Mod 2  interaction_term(iv = "CF", moderator = "Experiencia", method = two_stage),

  interaction_term(iv = "MH", moderator = "Idade", method = two_stage),
  interaction_term(iv = "MH", moderator = "Genero", method = two_stage),
  interaction_term(iv = "MH", moderator = "Experiencia", method = two_stage),
  
  interaction_term(iv = "PR", moderator = "Idade", method = two_stage),
  interaction_term(iv = "PR", moderator = "Genero", method = two_stage),

# Mod3   interaction_term(iv = "HA", moderator = "Idade", method = two_stage),
# Mod3   interaction_term(iv = "HA", moderator = "Genero", method = two_stage),
# Mod3   interaction_term(iv = "HA", moderator = "Experiencia", method = two_stage),

  interaction_term(iv = "IC", moderator = "Experiencia", method = two_stage)
)

sm <- relationships(
# Mod 2  paths(from = c("ED", "EE", "IS", "CF", "MH", "PR", "HA"), to = c("IC")),
# Mod 3  paths(from = c("ED", "EE", "IS", "MH", "PR", "HA"), to = c("IC")),
  paths(from = c("ED", "EE", "IS", "MH", "PR"), to = c("IC")),
  
# Mod 2  paths(from = c("IC", "CF", "HA"), to = c("USO")),
# Mod 3  paths(from = c("IC", "HA"), to = c("USO")),
  paths(from = c("IC"), to = c("USO")),

  paths(from = c("Idade", "Genero", "Experiencia"), to = c("IC", "USO")),

  # UTAUT
  paths(from = c("ED*Idade", "ED*Genero"), to = c("IC")),
  paths(from = c("EE*Idade", "EE*Genero", "EE*Experiencia"), to = c("IC")),
  paths(from = c("IS*Idade", "IS*Genero", "IS*Experiencia"), to = c("IC")),
# Mod 2  paths(from = c("CF*Idade", "IS*Experiencia"), to = c("USO")),
  paths(from = c("IS*Experiencia"), to = c("USO")),

  # UTAUT2
# Mod 2  paths(from = c("CF*Idade", "CF*Genero"), to = c("IC")),
  paths(from = c("MH*Idade", "MH*Genero", "MH*Experiencia"), to = c("IC")),
  paths(from = c("PR*Idade", "PR*Genero"), to = c("IC")),
# Mod 3  paths(from = c("HA*Idade", "HA*Genero", "HA*Experiencia"), to = c("IC", "USO")),
  
  paths(from = c("IC*Experiencia"), to = c("USO"))
)

pls_model <- estimate_pls(
  data = data,
  measurement_model = mm,
  structural_model  = sm,
  inner_weights = path_weighting,
  missing = mean_replacement,
  missing_value = "999"
)

# 2.3.5 Resumindo o modelo
summary <- summary(pls_model)

# verificação de convergencia
summary$iterations

# [1] 6

# 3.1 Confiabilidade do indicador
# - Cargas do indicador acima de 0,708 são recomendadas, pois correspondem a uma 
# variância explicada (confiabilidade do indicador) de pelo menos 50%.
# - Indicadores com cargas entre 0,40 e 0,70 devem ser considerados para remoção.
# - Indicadores com cargas muito baixas (abaixo de 0,40) devem ser removidos.

dumpMatrix(summary$loadings, function(l, c, v){
  if (v != 0){
    if (v < 0.4){
      cat(l, 'x', c, round(v, 3), '(Remover)', "\n")
    }else if(v < 0.708){
      cat(l, 'x', c, round(v, 3), '(Ponderar)', "\n")
    }
  } 
})

# Mod 1
# CF1 x CF 0.363 (Remover) 
# CF4 x CF 0.528 (Ponderar) 
# HA2 x HA 0.082 (Remover) 

# Cargas fatoriais
summary$loadings

# 3.2 Confiabilidade da consistência interna
# Dos vários indicadores de confiabilidade da consistência interna, o alfa de Cronbach 
# é o limite inferior (Trizano-Hermosilla & Alvarado, 2016), a confiabilidade composta
# ρc (Jöreskog, 1971) é o limite superior para confiabilidade da consistência interna. 
# O coeficiente de confiabilidade exato (ou consistente) ρA geralmente fica entre esses 
# limites e pode servir como uma boa representação da confiabilidade da consistência interna 
# de um construto (Dijkstra, 2010, 2014; Dijkstra & Henseler, 2015).
# Um item é aceitável para inclusão no modelo se a confiabilidade de sua consistência interna 
# assumir valores específicos:
#  - Valor recomendado de 0,80 a 0,90.
#  - Valor mínimo de 0,70 (ou 0,60 em pesquisa exploratória).
#  - Valor máximo de 0,95 para evitar redundância de indicadores, o que comprometeria a 
# validade de conteúdo (Diamantopoulos et al., 2012).

# A validade convergente é a medida em que o construto converge para explicar a variância dos seus 
# indicadores. A variância média extraída (AVE) é a média das cargas quadradas de um indicador de construto. 
# O AVE mínimo aceitável é 0,50 ou superior (Hair et al., 2021).

# Resumo: Alpha, rhoC, and rhoA should exceed 0.7 while AVE should exceed 0.5 vide página 77

summary$reliability
plot(summary$reliability)

# Mod 2
#                alpha  rhoC   AVE  rhoA
# CF             0.656 0.844 0.732 0.797

# 3.4 Validade discriminante

# não usar o critério de Fornell-Larcker

# Usar a razão heterotraço-monotraço (HTMT) das correlações para avaliar a validade discriminante (Henseler et al., 2015). 
# O HTMT é o valor médio das correlações dos indicadores entre construtos (ou seja, as correlações 
# heterotraço-heterométodo) em relação à média (geométrica) das correlações médias para os indicadores 
# que medem o mesmo construto (ou seja, as correlações monotraço-heterométodo).
# Problemas de validade discriminante estão presentes quando os valores HTMT:
# - excedem 0,90 para construtos conceitualmente muito semelhantes.
# - excedem 0,85 para construtos conceitualmente mais distintos.

dumpMatrix(summary$validity$htmt, function(l, c, v){
  if (!is.na(v)){
    if (v > 0.9){
      cat(l, 'x', c, round(v, 3), '(Alto se construtos semelhantes)', "\n")
    }else if(v > 0.85){
      cat(l, 'x', c, round(v, 3), '(Alto se construtos diferentes)', "\n")
    } 
  }
})

# Mod 3
# HA x ED 0.864 (Alto se construtos diferentes) 
# HA x MH 0.908 (Alto se construtos semelhantes) 
# IC x HA 0.993 (Alto se construtos semelhantes) 


# 5.1 Validação de colinearidade aplicada apenas a construtos formativos
# Os valores VIF > 5 problema, > 3 possivel problema < 3 ok
summary$vif_antecedents$USO

# 5.3 Poder explicativo
# Para considerar o poder explicativo do modelo analisamos o R2 dos 
# construtos endógenos e o tamanho do efeito f2 dos construtos preditores.
summary$paths

#                    IC    USO
# R^2             0.627  0.209
# AdjR^2          0.593  0.190
# ED              0.406      .
# EE              0.113      .
# IS              0.144      .
# MH              0.273      .
# PR              0.047      .
# IC                  .  0.447
# ED*Idade       -0.146      .
# ED*Genero       0.035      .
# EE*Idade       -0.044      .
# EE*Genero       0.043      .
# EE*Experiencia  0.025      .
# IS*Idade        0.036      .
# IS*Genero      -0.062      .
# IS*Experiencia -0.013 -0.077
# MH*Idade        0.147      .
# MH*Genero      -0.041      .
# MH*Experiencia  0.025      .
# PR*Idade        0.001      .
# PR*Genero      -0.041      .
# IC*Experiencia      . -0.005


# Processo de Reamostragem

# 2.3.6 Inicializando o modelo
boot <- bootstrap_model(seminr_model = pls_model,
                        nboot = 5000,
                        cores = 2,
                        seed = 123)

# sumarização da reamostragem
sum_boot <- summary(boot)

# 5.2 Significado e relevância das relações do modelo estrutural

sum_boot$bootstrapped_paths

# apenas significativos
subset(sum_boot$bootstrapped_paths, !(0 > sum_boot$bootstrapped_paths[,5] & 0 < sum_boot$bootstrapped_paths[,6]))
