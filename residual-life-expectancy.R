### TAENIA SOLIUM IN TANZANIA - RESIDUAL LIFE EXPECTANCY
### last update: 14/11/2015

## define ages for which RLE is available
age <- c(0, 1, 5 * 1:19)

## Coale-Demeny model life table West
le_cd <-
structure(c(80.00, 79.36, 75.38, 70.40, 65.41, 60.44, 55.47,
            50.51, 45.57, 40.64, 35.77, 30.99, 26.32, 21.81,
            17.50, 13.58, 10.17,  7.45,  5.24,  3.54,  2.31,
            82.50, 81.84, 77.95, 72.99, 68.02, 63.08, 58.17,
            53.27, 48.38, 43.53, 38.72, 33.99, 29.37, 24.83,
            20.44, 16.20, 12.28,  8.90,  6.22,  4.25,  2.89),
          .Dim = c(21L, 2L))

## function to interpolate life expectancy table
rle <-
function(x, sex = c("m", "f"), le = le_cd) {
  sex <- match.arg(sex)
  col <- which(c("m", "f") == sex)
  approx(age, le[, col], x)$y
}