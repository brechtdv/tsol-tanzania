### TAENIA SOLIUM IN TANZANIA - SENSITIVITY ANALYSES
### last update: 14/11/2015

###
### SENSITIVITY ANALYSIS FUNCTIONS
###

## standardized regression coefficients
sa_src <-
function(y, x) {
  df <- as.data.frame(apply(x, 2, scale))
  summary(lm(scale(y) ~ ., data = df))
}

## partial correlation coefficients
sa_pcc <-
function(y, x) {
  out <- matrix(ncol = 2, nrow = ncol(x))
  colnames(out) <- c("rho", "p")
  rownames(out) <- colnames(x)

  for (i in seq(ncol(x))){
    lm_y <- lm(y ~ x[, -i])      # regress y to other x's
    lm_x <- lm(x[, i] ~ x[, -i]) # regress x to other x's
    out[i, ] <-
      unlist(cor.test(lm_y$residuals, lm_x$residuals)[4:3],
             use.names = FALSE)
  }

  return(out[order(out[, "rho"], decreasing = TRUE), ])
}


###
### COSTS
###

## load cost script
source("tsol-cost.R")

## pigs
sa_src(cost_pigs,
       cbind(prev_pigs, price_pigs))
sa_pcc(cost_pigs,
       cbind(prev_pigs, price_pigs))

## humans
sa_src(cost_ncc,
       cbind(e_prev, prop_ncc,
             p_hosp, n_visit_med, price_heal, p_carba,
             working_days, loss_workingtime, monthly_salary))

sa_pcc(cost_ncc,
       cbind(e_prev, prop_ncc,
             p_hosp, n_visit_med, price_heal, p_carba,
             working_days, loss_workingtime, monthly_salary))

## total
sa_pcc(cost_total,
       cbind(prev_pigs, price_pigs,
             e_prev, prop_ncc,
             p_hosp, n_visit_med, price_heal, p_carba,
             working_days, loss_workingtime, monthly_salary))


###
### DALYs
###

## load cost script
source("tsol-daly.R")

## total
sa_src(daly_all,
       cbind(e_prev, prop_ncc, ep_trt, ep_dsw_tr, ep_dsw_nt, ep_cfr))
sa_pcc(daly_all,
       cbind(e_prev, prop_ncc, ep_trt, ep_dsw_tr, ep_dsw_nt, ep_cfr))