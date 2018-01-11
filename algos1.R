x <- c(4000, 8000, 16000, 32000, 64000)
y <- c(0.1, 0.3, 1.3, 5.1, 20.5)

UF <- structure(list(comps = list()
, meths = list(
    union = function(a, b) {
      if (!identical(c(a, b) %% 1, c(0, 0))) stop("a and b must both be int")
      curr_len <- length(UF$comps)
      if (curr_len == 0) {
        UF$comps[[1]] <<- c(a, b)
      } else {
        where_found <- lapply(UF$comps, function(x) {
          match(x, table = c(a, b))
        })
        where_found <- 
      }
    }
  ))
, class = "UF")


UF$meths$union(1, 0)

x1 <- c(0.1, 0.9)
x2 <- c(0.5, 0.5)
x <- x1
-sum(x * log(x))
log(x)
