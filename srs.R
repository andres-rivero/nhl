gp_vs <- c()


for (i in clubs$club) gp_vs <- rbind(gp_vs,{
  sapply(clubs$club, FUN = function(x)
    length(
      which(
        games$opponent[which(
          games$team == x)] == i)))
})

colnames(gp_vs) <- clubs$club
rownames(gp_vs) <- clubs$club


gp_vs <- gp_vs/rowSums(gp_vs)

for (i in 1:31) {
  gp_vs[i, i] <- 1
}

gdiff_per <- matrix(clubs$gdiff_per, 31, 1)

srs <- solve(gp_vs) %*% gdiff_per