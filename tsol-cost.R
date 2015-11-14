### TAENIA SOLIUM IN TANZANIA - ECONOMIC IMPACT
### last update: 14/11/2015

###
### SETTINGS
###

## set seed to allow for reproducibility
set.seed(264)

## number of iterations
n <- 1e5

## currency conversion factor, Tanzanian Shilling to US Dollar
tzs2usd <- 1695

## HUMANS

## population size
pop <- 44928923

## epilepsy prevalence, Uniform parameters
e_prev_min <- 0.00282453
e_prev_max <- 0.0115

## proportion PWE with NCC, uniform parameters 
prop_ncc_min <- 0.07692308
prop_ncc_max <- 0.22764228

## hospitalization probability, Beta parameters
p_hosp_alpha <- 17
p_hosp_beta <- 320

## hospitalization duration, Uniform parameters
stay_min <- 1
stay_max <- 27

## care seeking probabilities, Multinomial parameters
p_care <- c(0.357, 0.024, 0.048, 0.571)

## number of visits to healthcare provider, Uniform parameters
n_visit_med_min <- 1
n_visit_med_max <- 12

## number of visits to a traditional healer, fixed
n_year_heal <- 4

## carbamazepin treatment probability, Beta parameters
p_carba_alpha <- 212 
p_carba_beta <- 125  

## loss of working time, Uniform parameters
loss_workingtime_min <- 1
loss_workingtime_max <- 24

## probability of losing job, Fixed
unemployed_duetoepilepsy <- 0.3

## number of working days, Uniform parameters
working_days_min <- 220
working_days_max <- 312

## proportion of active population, Fixed
active <- 0.491

## monthly salary, Gamma parameters 
monthly_salary_shape <- 1.874692e+00
monthly_salary_rate  <- 1.992633e-05

## cost of medication, Fixed
price_med <- 5000 

## cost of hospitalization per day, Fixed
price_day_hosp <- 20000 

## cost of visiting traditional healer, Gamma parameters 
price_heal_shape <- 1.120164e+00
price_heal_rate <- 3.936949e-05

## price of carbamazepin, Fixed
price_carba <- 30000

## number of working days per month, Fixed
n_working_day_by_month <- 26


## PIGS

## pig population sizes, Fixed
n_pigs_smallscale <- 1573080

## value of a pig, Gamma parameters
price_pigs_shape <- 12.32
price_pigs_rate <- 8.04e-05

## value reduction, Fixed
price_loss_pigs <- 0.5

## proportion of pigs sold per year, Fixed
pigs_sold <- 0.333

## porcine cysticercosis prevalence, Uniform parameters
prev_pigs_min <- 0.060
prev_pigs_max <- 0.174 


###
### SIMULATIONS, HUMANS
###

## epilepsy prevalence
e_prev <- runif(n, e_prev_min, e_prev_max)

## proportion PWE with NCC
prop_ncc <- runif(n, prop_ncc_min, prop_ncc_max)

## prevalence NCC-associated epilepsy
ep_ncc <- e_prev * prop_ncc

## epilepsy cases due to ncc
n_ncc <- ep_ncc * pop

## number of hospitalized NCC patients
p_hosp <- rbeta(n, p_hosp_alpha, p_hosp_beta)
n_hosp <- n_ncc * p_hosp

## duration of hospital stay
## .. per iteration, generate 'n_hosp' random durations, and sum them up
stay <- apply(t(n_hosp), 2, function(x) sum(runif(x, stay_min, stay_max)))

## patients not in hospital, seeking medical treatment
n_ncc2 <- n_ncc - n_hosp
xyz <- rmultinom(n, n_ncc2, p_care)
n_heal <- xyz[1, ]
n_med <- xyz[2, ]
n_medheal <- xyz[3, ]
n_notreat <- xyz[4, ]

## number of visits to the medical doctor
n_visit_med <- runif(n, n_visit_med_min, n_visit_med_max)

## number of visits to the traditional healer
#n_visit_heal <- runif(n, n_year_heal_min, n_year_heal_max)
#mean(n_visit_heal)

## price of traditional healer
price_heal <- rgamma(n, price_heal_shape, price_heal_rate)

## carbamazepin use
p_carba <- rbeta(n, p_carba_alpha, p_carba_beta)
n_carba <- (n_hosp + n_med + n_medheal) * p_carba

working_days <- runif(n, working_days_min, working_days_max)

## loss of work, corrected
loss_workingtime <- runif(n, loss_workingtime_min, loss_workingtime_max)
n_days1 <- n_ncc * active * unemployed_duetoepilepsy * working_days
n_days2 <- n_ncc * active * (1 - unemployed_duetoepilepsy) * loss_workingtime
n_days_inactivity <- n_days1 + n_days2

## monthly salary
monthly_salary <- rgamma(n, monthly_salary_shape, monthly_salary_rate)


###
### SIMULATIONS, PIGS
###

## porcine cysticercosis prevalence
prev_pigs <- runif(n, prev_pigs_min, prev_pigs_max)

## value of pig
price_pigs <- rgamma(n, price_pigs_shape, price_pigs_rate)


###
### COSTS, HUMANS
###

## hospitalization costs
cost_hosp <- stay * price_day_hosp

## healthcare provider costs
cost_med <- n_visit_med * price_med * (n_med + n_medheal)

## traditional healer costs
cost_heal <- (n_heal + n_medheal) * price_heal / n_year_heal

## medication costs
cost_medicine <- n_carba * price_carba

## productivity losses
cost_inactivity <-
  n_days_inactivity * monthly_salary / n_working_day_by_month


###
### COSTS, PIGS
###

## number of infected pigs
n_pigs_infected <- n_pigs_smallscale * prev_pigs

## losses due to porcine cysticercosis
cost_pigs <-
  n_pigs_smallscale * pigs_sold * price_loss_pigs * prev_pigs * price_pigs

usd_cost_pigs <- cost_pigs / tzs2usd


###
### TOTAL COSTS AND CONVERSIONS
###

cost_total <-
  cost_hosp + cost_med + cost_heal + cost_inactivity + cost_medicine +
  cost_pigs

cost_ncc <- cost_total - cost_pigs

cost_by_ncc <- cost_ncc / n_ncc

usd_cost_total <- cost_total / tzs2usd
usd_cost_ncc <- cost_ncc / tzs2usd
usd_cost_by_ncc <- cost_by_ncc / tzs2usd

usd_cost_hosp <- cost_hosp / tzs2usd
usd_cost_med <- cost_med / tzs2usd
usd_cost_heal <- cost_heal / tzs2usd
usd_cost_inactivity <- cost_inactivity / tzs2usd
usd_cost_medicine <- cost_medicine / tzs2usd
usd_cost_pigs <- cost_pigs / tzs2usd


###
### SUMMARIES
###

Summary <-
function(x) {
  print(c(mean = mean(x),
          quantile(x, c(.025, .975))))
}

Summary(cost_total)
Summary(cost_ncc)
Summary(cost_pigs)

Summary(usd_cost_total)
Summary(usd_cost_ncc)
Summary(usd_cost_pigs)

Summary(cost_by_ncc)
Summary(usd_cost_by_ncc)

Summary(usd_cost_hosp)
Summary(usd_cost_med)
Summary(usd_cost_heal)
Summary(usd_cost_medicine)
Summary(usd_cost_inactivity)

Summary(n_ncc)
Summary(n_pigs_infected)
Summary(n_pigs_infected/n_pigs_smallscale)

Summary(cost_hosp / cost_ncc)
Summary(cost_med / cost_ncc)
Summary(cost_heal / cost_ncc)
Summary(cost_medicine / cost_ncc)
Summary(cost_inactivity / cost_ncc)

Summary(cost_pigs / cost_total)
Summary(cost_inactivity / cost_total)
Summary((cost_hosp + cost_med + cost_heal + cost_medicine) /
        cost_total)