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


m <- diag(clubs$gdiff_per, 31, 31)

times_played <- c()
for (i in clubs$club) times_played <- c(times_played,{
  length(
    which(
      games$opponent[which(
        games$team == "VAN")] == i))
})
