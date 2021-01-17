#' funkcja generujaca probe pochodzaca z 2 zadanych rozkladow
#' @export
#' @param n dlugosc proby
#' @param family_1 nazwa pierwszego rozkladu
#' @param par_1 parametry pierwszego rozkladu
#' @param  family_2 nazwa drugiego z rozkladow
#' @param  par_2 parametry drugiego z rozkladow
#' @param p czesc koncowej proby jaka maja stanowic obserwacje wylosowane z 1 rozkladu
#' @param ... not sure
#metoda podstawowa
DistMixing <- function(n, family_1, par_1, family_2, par_2, p = .5){

  my_rfam1 = NULL
  my_rfam2 = NULL

  # tworzenie wektora bedacego mieszanka dwoch rozkladow
  # Args: n - dlugosc proby
  #       family_1 - nazwa pierwszego rozkladu, odpowiadająca nazewnictwu rozkladow w pakiecie stats
  #       par_1 - wektor/lista parametrow do losowania rozkladu 1
  #       family_2 - nazwa drugiego rozkladu, odpowiadajaca nazewnictwu rozkladow w pakiecie stats
  #       par_2 - wektor/lista parametrow do losowania rozkladu 2
  #       p - czesc proby jaka powinien stanowic 1 rozklad - 0.4 oznacza, ze stanowi 40% proby, itd.
  # Result: proba (wektor) bedaca mieszanina dwoch zadanych rozkladow, z podanymi proporcjami

  dist_names <- c("beta", "binom", "cauchy", "chisq", "exp", "f",
                  "gamma", "geom", "hyper", "logis", "lnorm",
                  "nbinom", "norm", "pois", "t", "tukey", "unif",
                  "weibull", "wilcox", "signrank")

  # Sprawdzanie poprawnosci argumentow
  stopifnot(is.numeric(n))
  stopifnot(is.numeric(p) && p > 0)
  stopifnot(is.list(par_1) | is.vector(par_1))
  stopifnot(is.list(par_2) | is.vector(par_2))
  stopifnot(family_1 %in% dist_names)
  stopifnot(family_2 %in% dist_names)

  index <- sample(c(1,2), n, prob = c(p, 1-p), replace = TRUE)
  x1 <- length(index[index == 1])
  x2 <- length(index[index == 2])

  eval(parse(text = paste0("my_rfam1 <- r", family_1)))
  result1 <- do.call(my_rfam1, as.list(c(x1, par_1)))

  eval(parse(text = paste0("my_rfam2 <- r", family_2)))
  result2 <- do.call(my_rfam2, as.list(c(x2, par_2)))
  res <- list(vec = c(result1, result2), fam1 = family_1, fam2 = family_2,
              pars1 = unlist(par_1), pars2 = unlist(par_2))


  class(res)<-append(class(res),'DistMixing')

  return(res)
}


#' funkcja generujaca probe pochodzaca z 2 zadanych rozkladow
#' @export
#' @param n dlugosc proby
#' @param family_1 nazwa pierwszego rozkladu
#' @param par_1 parametry pierwszego rozkladu
#' @param  family_2 nazwa drugiego z rozkladow
#' @param  par_2 parametry drugiego z rozkladow
#' @param p czesc koncowej proby jaka maja stanowic obserwacje wylosowane z 1 rozkladu
#' @param ... not sure
DistMixing <- function(n, family_1, par_1, family_2, par_2, p = .5, ...) UseMethod("DistMixing")

#' funkcja generujaca probe pochodzaca z 2 zadanych rozkladow
#' @export
#' @param n dlugosc proby
#' @param family_1 nazwa pierwszego rozkladu
#' @param par_1 parametry pierwszego rozkladu
#' @param  family_2 nazwa drugiego z rozkladow
#' @param  par_2 parametry drugiego z rozkladow
#' @param p czesc koncowej proby jaka maja stanowic obserwacje wylosowane z 1 rozkladu
#' @param ... not sure
DistMixing.default <- function(n, family_1, par_1, family_2, par_2, p = .5, ...){
  my_rfam1 = NULL
  my_rfam2 = NULL

  # tworzenie wektora bedacego mieszanka dwoch rozkladow
  # Args: n - dlugosc proby
  #       family_1 - nazwa pierwszego rozkladu, odpowiadająca nazewnictwu rozkladow w pakiecie stats
  #       par_1 - wektor/lista parametrow do losowania rozkladu 1
  #       family_2 - nazwa drugiego rozkladu, odpowiadajaca nazewnictwu rozkladow w pakiecie stats
  #       par_2 - wektor/lista parametrow do losowania rozkladu 2
  #       p - czesc proby jaka powinien stanowic 1 rozklad - 0.4 oznacza, ze stanowi 40% proby, itd.
  # Result: proba (wektor) bedaca mieszanina dwoch zadanych rozkladow, z podanymi proporcjami

  dist_names <- c("beta", "binom", "cauchy", "chisq", "exp", "f",
                  "gamma", "geom", "hyper", "logis", "lnorm",
                  "nbinom", "norm", "pois", "t", "tukey", "unif",
                  "weibull", "wilcox", "signrank")

  # Sprawdzanie poprawnosci argumentow
  stopifnot(is.numeric(n))
  stopifnot(is.numeric(p) && p > 0)
  stopifnot(is.list(par_1) | is.vector(par_1))
  stopifnot(is.list(par_2) | is.vector(par_2))
  stopifnot(family_1 %in% dist_names)
  stopifnot(family_2 %in% dist_names)

  index <- sample(c(1,2), n, prob = c(p, 1-p), replace = TRUE)
  x1 <- length(index[index == 1])
  x2 <- length(index[index == 2])

  eval(parse(text = paste0("my_rfam1 <- r", family_1)))
  result1 <- do.call(my_rfam1, as.list(c(x1, par_1)))

  eval(parse(text = paste0("my_rfam2 <- r", family_2)))
  result2 <- do.call(my_rfam2, as.list(c(x2, par_2)))
  res <- list(vec = c(result1, result2), fam1 = family_1, fam2 = family_2,
              pars1 = unlist(par_1), pars2 = unlist(par_2))


  class(res)<-append(class(res),'DistMixing')

  return(res)
}

#' Komentarz
#' @export
#' @param x obiekt klasy DistMixing, ktory chcemy wypisac
#' @param ... not sure
print.DistMixing <- function(x, ...){
  cat("Szereg powstal z polaczenia rozkladu:\n")
  print(x$fam1)
  cat("o ponizszych parametrach:\n")
  print(x$pars1)
  cat("oraz rozkladu:\n")
  print(x$fam2)
  cat("o ponizszych parametrach:\n")
  print(x$pars2)
  cat("Wygenerowana proba:\n")
  print(x$vec)
}

#' Komentarz
#' @export
#' @param object obiekt klasy DistMixing, dla ktorego chcemy obliczyc podst. statystyki opisowe
#' @param ... not sure
summary.DistMixing <- function(object, ...){

  meanValue <- mean(object$vec)
  sdValue <- stats::sd(object$vec)
  minValue <- min(object$vec)
  maxValue <- max(object$vec)
  kurtosisValue <- moments::kurtosis(object$vec)
  skewValue <- moments::skewness(object$vec)

  round(data.frame(meanValue, sdValue, minValue, maxValue, kurtosisValue, skewValue),3)

}


