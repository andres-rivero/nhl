clubs$gp <- sapply(clubs$club, FUN = function(x)
  length(
    which(games$team == x)
  )
)

clubs$gf <- sapply(clubs$club, FUN = function(x)
  sum(
    as.numeric(
      games$gf[which(
        games$team == x)]))
)

clubs$ga <- sapply(clubs$club, FUN = function(x)
  sum(
    as.numeric(
      games$ga[which(
        games$team == x)]))
)

clubs$gdiff_per <- (clubs$gf - clubs$ga)/clubs$gp


m <- matrix(rep(0, 31*31), 31, 31)
m[,1] <- clubs$gdiff_per
