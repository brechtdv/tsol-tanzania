### TAENIA SOLIUM IN TANZANIA - SENSITIVITY ANALYSES
### last update: 20/11/2015

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

  return(out[order(abs(out[, "rho"]), decreasing = TRUE), ])
}


###
### TORNADO GRAPH
###

## requires library 'ggplot2'
library(ggplot2)

## ggplot2
tornado <-
function(coef, names = NULL) {
  ## copy names if needed
  if (is.null(names)) names <- rownames(coef)

  ## create data frame
  df <- data.frame(est = coef[, "rho"],
                   order = order(abs(coef[, "rho"])),
                   name = names)

  ## sort data frame
  df <- df[df$order, ]

  ## create ggplot
  ggplot(df, aes(x = order, y = est)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    scale_x_continuous(element_blank(),
                       breaks = seq(nrow(df)),
                       labels = df$name) +
    scale_y_continuous("partial correlation coefficient",
                       limits = c(min(0, min(df$est) - 0.1),
                                  max(0, max(df$est) + 0.1))) +
    geom_text(aes(x = order, y = est, label = formatC(est, 3, form = "f")),
              size = 3,
              hjust = ifelse(df$est > 0, -0.1, 1.1),
              vjust = 0.4) +
    theme_bw()
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
sa_cost_pcc <-
sa_pcc(cost_total,
       cbind(prev_pigs, price_pigs,
             e_prev, prop_ncc,
             p_hosp, n_visit_med, price_heal, p_carba,
             working_days, loss_workingtime, monthly_salary))
sa_cost_pcc

## tornado graph
cost_items <-
c("Monthly salary", "Prevalence of E", "Proportion of NCC-associated E", 
  "Cost of a pig", "Prevalence of PC", "Working days", "Woking time lost", 
  "Proportion of hospitalizations", "Cost for a traditional healer", 
  "No of visits to a medical doctor", "Proportion receiving medication")

png("sa_cost_pcc.png", 7, 3.5, units = "in", res = 400)
tornado(sa_cost_pcc, cost_items)
graphics.off()


###
### DALYs
###

## load cost script
source("tsol-daly.R")

## total
sa_src(daly_all,
       cbind(e_prev, prop_ncc, ep_trt, ep_dsw_tr, ep_dsw_nt, ep_cfr))

sa_daly_pcc <-
sa_pcc(daly_all,
       cbind(e_prev, prop_ncc, ep_trt, ep_dsw_tr, ep_dsw_nt, ep_cfr))
sa_daly_pcc

## tornado graph
daly_items <-
c("E prevalence", "Proportion of NCC-associated E",
  "E case-fatality ratio", "E disability weight (untreated)",
  "E disability weight (treated)", "Treatment proportion")

png("sa_daly_pcc.png", 7, 2.5, units = "in", res = 400)
tornado(sa_daly_pcc, daly_items)
graphics.off()