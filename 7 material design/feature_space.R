m_set <- seq(163, 363, 8)       # 25
Vi_set <- seq(36, 56, 0.8)      # 25
tt_set <- seq(0.74, 1.04, 0.012) # 25
pm_set <- seq(8, 18, 0.4)       # 25

Lm <- length(m_set) # 26
Lv <- length(Vi_set) # 26
Lt <- length(tt_set) # 26
Lp <- length(pm_set) # 26

possible_set <- matrix(0, nrow = Lm*Lv*Lt*Lp, ncol = 4)
l = 1

for (i in 1:Lm) {
  for (j in 1:Lv) {
    for (k in 1:Lt) {
      for (a in 1:Lp) {
        possible_set[l,1] <- m_set[i]
        possible_set[l,2] <- Vi_set[j]
        possible_set[l,3] <- tt_set[k]
        possible_set[l,4] <- pm_set[a]
        l = l + 1
      }
    }
  }
}


write.csv(possible_set,"/possible_set.csv")

