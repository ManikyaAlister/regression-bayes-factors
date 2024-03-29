rm(list = ls())
library(here)
library(bayestestR)

#' Function for calculating R2 change in R
#' 
#' @param alternative_model lm() object for the alternative model
#' @param null_model lm() object for the null model
R2_change = function(alternative_model, null_model){
  R2_change <- summary(alternative_model)$r.square - summary(null_model)$r.square
  R2_change
}

null_model = function(outcome, data) {
  output <- lm(eval(parse(text=outcome)) ~ age + sex + education, data = data)
  output
}

# Predictor 1: strength ---------------------------------------------------

##load strength data 
load(here("data/clean/data_strength.Rdata"))

# Affective TOM (as = affect & strength) 
## Data 
data_as <- data_strength[!data_strength[,"affective_tom"] %in% NA,] 
## Null model
m0_as <- null_model("affective_tom", data_as)
## Alt. model
m_as <- lm(affective_tom ~ age + sex + education + strength, data = data_as)
### R2 change (original = 0.01)
R2_as <- R2_change(m_as, m0_as)# 0.01
### Bayes factor
bf_as <- bf_models(m0_as, m_as) 

# Cognitive ToM (cs = cognitive & strength)
## Data
data_cs <- data_strength[!data_strength[,"cognitive_tom"] %in% NA,] 
## Null model
m0_cs <- null_model("cognitive_tom", data_cs)
## Alt. model
m_cs <- lm(cognitive_tom ~ age + sex + education + strength, data = data_cs)
## R2 change (original = 0.02)
R2_cs <- R2_change(m_cs, m0_cs) # 0.02
## Bayes factor
bf_cs <- bf_models(m0_cs, m_cs)  

# Social Perception (ss = social perception and strength)
## Data
data_ss <- data_strength[!data_strength[,"social_perception"] %in% NA,] 
## Null model
m0_ss <- null_model("social_perception", data_ss)
## Alt. model
m_ss <- lm(social_perception ~ age + sex + education + strength, data = data_ss)
## R2 change (original = 0.00)
R2_ss <- R2_change(m_ss, m0_ss) # 0.00
## Bayes Factor
bf_ss <- bf_models(m0_ss,m_ss)

## Control task  (cos = control & strength)
## Data
data_cos <- data_strength[!data_strength[,"control"] %in% NA,] 
## Null model
m0_cos <- null_model("control", data_cos)
## Alt. model
m_cos <- lm(control ~ age + sex + education + strength, data = data_cos)
## R2 change (original = 0.00)
R2_cos <- R2_change(m_cos, m0_cos) # 0.00
## Bayes Factor
bf_cos <- bf_models(m0_cos,m_cos)

# New outcomes as per reviewer comments (May 11th 2023)

# Trail A 
data_trA <- data_strength[!data_strength[,"trail_A"] %in% NA,]
##  Null model
m0_trA <- null_model("trail_A", data_trA)
## Alt. model
m_trA <- lm(trail_A ~ age + sex + education + strength, data = data_strength)
## R2 change (original = 0.00)
R2_trA <- R2_change(m_trA, m0_trA) # 0.00
## Bayes Factor
bf_trA <- bf_models(m0_trA,m_trA)


# Trail B 
data_trB <- data_strength[!data_strength[,"trail_B"] %in% NA,]
##  Null model
m0_trB <- null_model("trail_B", data_trB)
## Alt. model
m_trB <- lm(trail_B ~ age + sex + education + strength, data = data_strength)
## R2 change (original = 0.00)
R2_trB <- R2_change(m_trB, m0_trB) # 0.08
## Bayes Factor
bf_trB <- bf_models(m0_trB,m_trB)


# Digit Symbol
data_ds <- data_strength[!data_strength[,"digit_symbol"] %in% NA,]
##  Null model
m0_ds <- null_model("digit_symbol", data_ds)
## Alt. model
m_ds <- lm(digit_symbol ~ age + sex + education + strength, data = data_strength)
## R2 change (original = 0.00)
R2_ds <- R2_change(m_ds, m0_ds) # 0.07
## Bayes Factor
bf_ds <- bf_models(m0_ds,m_ds)


