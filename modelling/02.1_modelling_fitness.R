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

null_model <- function(outcome, data) {
  output <- lm(eval(parse(text=outcome)) ~ age + sex + education, data = data)
  output
}

#  Predictor 2: fitness ---------------------------------------------------

# load fitness data
load(here("data/clean/data_fitness.Rdata"))

# Affective TOM (af = affect & fitness) 
## data: Remove participants who are missing for the predictor so null and alternate 
## models are fit to the same data (only need this for the null model because lm does 
## this automatically for the alt models)
data_af <- data_fitness[!data_fitness[,"affective_tom"] %in% NA,] 
##  Null model
m0_af <- null_model("affective_tom", data_af)
## Alt. model
m_af <- lm(affective_tom ~ age + sex + education + cardio, data = data_fitness) 
## R2 change (original = 0.01)
R2_af <- R2_change(m_af, m0_af)# 0.01
## Bayes factor
bf_af <- bf_models(m0_af, m_af) 


# Cognitive tom (cs = cognitive & fitness)
## Null data
data_cf <- data_fitness[!data_fitness[,"cognitive_tom"] %in% NA,]
##  Null model
m0_cf <- null_model("cognitive_tom", data_cf)
## Alt. model
m_cf <- lm(cognitive_tom ~ age + sex + education + cardio, data = data_fitness)
## R2 change (original = 0.08)
R2_cf <- R2_change(m_cf, m0_cf) # 0.08
## Bayes factor
bf_cf <- bf_models(m0_cf, m_cf)  

# Social Perception (sf = social perception and fitness)
data_sf <- data_fitness[!data_fitness[,"social_perception"] %in% NA,]
##  Null model
m0_sf <- null_model("social_perception", data_sf)
## Alt. model
m_sf <- lm(social_perception ~ age + sex + education + cardio, data = data_fitness)
## R2 change (original = 0.01)
R2_sf <- R2_change(m_sf, m0_sf) # 0.01
## Bayes Factor
bf_sf <- bf_models(m0_sf,m_sf)

# Control task  (cof = control & fitness)
data_cof <- data_fitness[!data_fitness[,"control"] %in% NA,]
##  Null model
m0_cof <- null_model("control", data_cof)
## Alt. model
m_cof <- lm(control ~ age + sex + education + cardio, data = data_fitness)
## R2 change (original = 0.00)
R2_cof <- R2_change(m_cof, m0_cof) # 0.00
## Bayes Factor
bf_cof <- bf_models(m0_cof,m_cof)

# New outcomes as per reviewer comments (May 11th 2023)

# Trail A 
data_trA <- data_fitness[!data_fitness[,"trail_A"] %in% NA,]
##  Null model
m0_trA <- null_model("trail_A", data_trA)
## Alt. model
m_trA <- lm(trail_A ~ age + sex + education + cardio, data = data_fitness)
## R2 change (original = 0.00)
R2_trA <- R2_change(m_trA, m0_trA) # 0.01
## Bayes Factor
bf_trA <- bf_models(m0_trA,m_trA)


# Trail B 
data_trB <- data_fitness[!data_fitness[,"trail_B"] %in% NA,]
##  Null model
m0_trB <- null_model("trail_B", data_trB)
## Alt. model
m_trB <- lm(trail_B ~ age + sex + education + cardio, data = data_fitness)
## R2 change (original = 0.00)
R2_trB <- R2_change(m_trB, m0_trB) # 0.01
## Bayes Factor
bf_trB <- bf_models(m0_trB,m_trB)


# Digit Symbol
data_ds <- data_fitness[!data_fitness[,"digit_symbol"] %in% NA,]
##  Null model
m0_ds <- null_model("digit_symbol", data_ds)
## Alt. model
m_ds <- lm(digit_symbol ~ age + sex + education + cardio, data = data_fitness)
## R2 change (original = 0.00)
R2_ds <- R2_change(m_ds, m0_ds) # 0.00
## Bayes Factor
bf_ds <- bf_models(m0_ds,m_ds)


