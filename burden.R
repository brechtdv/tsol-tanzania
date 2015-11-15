### TAENIA SOLIUM IN TANZANIA - DALY HELPER FUNCTIONS
### last update: 14/11/2015

## function to calculate integral
f <-
function(x, K, C = .1658, beta = .04, r, a) {
  K * C * x * exp(-beta * x) * exp(-r * (x - a)) +
  (1 - K) * exp(-r * (x - a))
}
## burden calculation function
burden <-
function(N, DW, A, L, K, r, a) {
  N * DW * integrate(f, lower = A, upper = A + L, K = K, r = r, a = a)$value
}

