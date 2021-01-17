#' funkcja przeprowadzajaca 5 testow normalnosci: Shapiro-Wilka, Jarque-Bera, Andersona-Darlinga, Kolmogorova-Smirnova oraz test Chi Pearsona dla zadanej proby.
#' @export
#' @param x proba poddawana testom normalnosci
#funkcja podstawowa
NormalityTesting <- function(x){

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

  class(res)<-append(class(res), 'NormalityTesting')
  return(res)

}

#' funkcja generyczna
#' @export
#' @param x proba poddawana testom normalnosci
#' @param ... not sure
NormalityTesting <- function(x, ...) UseMethod("NormalityTesting")

#' funkcja przeprowadzajaca 5 testow normalnosci: Shapiro-Wilka, Jarque-Bera, Andersona-Darlinga, Kolmogorova-Smirnova oraz test Chi Pearsona dla zadanej proby.
#' @export
#' @param x proba poddawana testom normalnosci
#' @param ... not sure
NormalityTesting.default <- function(x, ...){

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

#' funkcja przeprowadzajaca 5 testow normalnosci: Shapiro-Wilka, Jarque-Bera, Andersona-Darlinga, Kolmogorova-Smirnova oraz test Chi Pearsona dla zadanej proby.
#' @export
#' @param x proba poddawana testom normalnosci
#' @param ... not sure
NormalityTesting.DistMixing <- function(x, ...){

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
#' @param x obiekt klasy NormalityTesting, kt nalezy wypisac
#' @param ... not sure
print.NormalityTesting <- function(x, ...){
  cat("Wyniki testow normalnosci (H0 - badany rozklad jest rozkladem normalnym):\n")
  print(data.frame(p.value = x))
}
