#' funkcja przeprowadzajaca testy normalnosci dla zadanej proby.
#' @export
#' @param x proba poddawana testom normalnosci
#' @return Funkcja zwraca p.value dla 5 testow normalnosci:
#' \itemize{
#'     \item Shapiro
#'     \item JarqueBera
#'     \item AndersonDarling
#'     \item KolmogorovSmirnov
#'     \item PearsonChiSquare
#' }
NormalityTesting <- function(x) UseMethod("NormalityTesting")

#' funkcja przeprowadzajaca testy normalnosci dla zadanej proby.
#' @export
#' @param x proba poddawana testom normalnosci
#' @return Funkcja zwraca p.value dla 5 testow normalnosci:
#' \itemize{
#'     \item Shapiro
#'     \item JarqueBera
#'     \item AndersonDarling
#'     \item KolmogorovSmirnov
#'     \item PearsonChiSquare
#' }
NormalityTesting.default <- function(x){

  stopifnot(is.numeric(x))

  shapiro.p <- stats::shapiro.test(x)$p.value
  jb.p <- tseries::jarque.bera.test(x)$p.value
  ad.p <- nortest::ad.test(x)$p.value
  ks.p <- nortest::lillie.test(x)$p.value
  chi.p <- nortest::pearson.test(x)$p.value
  res <- round(c("Shapiro" = shapiro.p,
                 "JarqueBera" = jb.p,
                 "AndersonDarling" = ad.p,
                 "KolmogorovSmirnov" = ks.p,
                 "PearsonChiSquare" = chi.p),4)

  class(res) <- append(class(res), 'NormalityTesting')
  return(res)

}

#' funkcja przeprowadzajaca testy normalnosci dla obiektu klasy DistMixing
#' @export
#' @param x obiekt klasy DistMixing
#' @return Funkcja zwraca p.value dla 5 testow normalnosci:
#' \itemize{
#'     \item Shapiro
#'     \item JarqueBera
#'     \item AndersonDarling
#'     \item KolmogorovSmirnov
#'     \item PearsonChiSquare
#' }
NormalityTesting.DistMixing <- function(x){

  shapiro.p <- stats::shapiro.test(x$vec)$p.value
  jb.p <- tseries::jarque.bera.test(x$vec)$p.value
  ad.p <- nortest::ad.test(x$vec)$p.value
  ks.p <- nortest::lillie.test(x$vec)$p.value
  chi.p <- nortest::pearson.test(x$vec)$p.value
  res <- round(c("Shapiro" = shapiro.p,
                 "JarqueBera" = jb.p,
                 "AndersonDarling" = ad.p,
                 "KolmogorovSmirnov" = ks.p,
                 "PearsonChiSquare" = chi.p),4)

  class(res) <- append(class(res), 'NormalityTesting')
  return(res)

}


#' funkcja sluzaca do wypisywania wynikow NormalityTesting
#' @export
#' @param x obiekt klasy NormalityTesting, ktory nalezy wypisac
#' @param ... dalsze argumenty przekazywane do lub z innych metod
print.NormalityTesting <- function(x, ...){
  cat("Wyniki testow normalnosci (H0 - badany rozklad jest rozkladem normalnym):\n")
  print(data.frame(p.value = x))
}
