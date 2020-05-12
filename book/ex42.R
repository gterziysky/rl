library(data.table)

policy_iter <- function() {
  dt <- init()
  dt <- policy_eval(dt)
}

init <- function() {
  set.seed(7)
  # action space
  aset <- -5:5
  # state space
  dt <- expand.grid(l1 = 0:20, l2 = 0:20)
  dt <- setDT(dt)
  # init state values
  dt$val <- runif(n = NROW(dt))
  # init possible actions, ie A(s)
  dt$act <- lapply(seq_len(NROW(dt)), function(i) {
    aset[(dt$l1[i] - aset) %in% 0:20 & (dt$l2[i] + aset) %in% 0:20]
  })
  # init policy
  dt$pi <- sapply(seq_len(NROW(dt)), function(i) sample(x = dt$act[[i]], size = 1))
  
  #View(dt)
  
  return(dt)
}

policy_eval <- function(dt) {
  xval = 0:20
  #s = 0
  # TODO: do one big DT instead of looping
  v <- expand.grid(i = 0:20, j = 0:20, k = 0:20, l = 0:20)
  setDT(v)
  v[, p := dpois(i, 3) * dpois(j, 3) * dpois(k, 2) * dpois(l, 4)]
  
  
  
  gamma <- 0.9
  for (h in 1:100) {
    delta <- 0
    futile.logger::flog.info("iter %d", h)
    for (i in xval) {
      futile.logger::flog.info("iter %d", i)
      for (j in xval)
        for (k in xval)
          for (l in xval) {
            #s <- s + dpois(i, 3) * dpois(j, 3) * dpois(k, 2) * dpois(l, 4)
            # i - return L1, j - request L1
            # k - return L2, l - request L2
            reward <- pmin(dt$l1 - dt$pi, j) + pmin(dt$l2 + dt$pi, l)
            reward <- reward * 10 - abs(dt$pi) * 2
            # account for requests
            l1_new <- pmax(dt$l1 - dt$pi - j, 0)
            l2_new <- pmax(dt$l2 + dt$pi - l, 0)
            # account for returns
            l1_new <- pmin(l1_new + i, 20)
            l2_new <- pmin(l2_new + k, 20)
            
            pass <- dpois(i, 3) * dpois(j, 3) * dpois(k, 2) * dpois(l, 4)
            val <- dt$val
            # obtain s'
            s_new <- dt[data.table(l1_new, l2_new), .(l1, l2, val), on = .(l1 = l1_new, l2 = l2_new)]
            
            dt$val <- dt$val + pass * (reward + gamma * s_new$val)
            delta <- max(delta, abs(val - dt$val))
          }
    }
    if (delta <= .Machine$double.eps ^ 0.25) break
  }
  
  return(dt)  
}

policy_eval2 <- function(dt) {
  xval = 0:20
  gamma <- 0.9
  #s = 0
  # TODO: do one big DT instead of looping 
  
  # pois <- get_pois_lookup()
  # pois <- split(pois, by = "lambda")
  
  v <- expand.grid(l1 = 0:20, l2 = 0:20, i = 0:20, j = 0:20, k = 0:20, l = 0:20)
  v <- expand.grid(i = 0:20, j = 0:20, k = 0:20, l = 0:20)
  v <- setDT(v)
  v[, prob := dpois(i, 3) * dpois(j, 3) * dpois(k, 2) * dpois(l, 4)]
  #v <- merge(x = dt, y = v, by = c("l1", "l2"), all = TRUE)
  #v <- data.table:::merge.data.table(x = dt, y = v, by = c("l1", "l2"), all = TRUE, sort = TRUE)
  v <- optiRum::CJ.dt(dt, v)
  setnames(x = v, old = "i.k", new = "k")
  
  setkeyv(v, c("l1", "l2"))
  # View(v[sample(seq_len(NROW(v)), 10)])
  # set.seed(7)
  # idx = sample(seq_len(NROW(v)), 10)
  
  # update pi - policy iteration
  # main loop - policy evaluation
  for (counter in 1:100) {
    delta <- 0
    v[, reward := 10 * (pmin(l1 - pi, j) + pmin(l2 + pi, l)) - abs(pi) * 2]
    
    v[, l1_new := pmax(l1 - pi - j, 0)]
    v[, l2_new := pmax(l2 + pi - l, 0)]
    
    v[, l1_new := pmin(l1_new + i, 20)]
    v[, l2_new := pmin(l2_new + k, 20)]
    
    tmp <- unique(v[, .(l1,l2,val_next_state = val)])
    v <- merge(x = v, y = tmp, by.x = c("l1_new", "l2_new"), by.y = c("l1", "l2"), sort = F)
    v$old_val <- v$val
    v$val <- v$val + v$prob * (v$reward + gamma * v$val_next_state)
    
    delta <- max(delta, abs(v$val - v$old_val))
    
    if (delta <= .Machine$double.eps ^ 0.25) break
  }
  
  return(dt)  
}

get_pois_lookup <- function() {
  pois <- setDT(expand.grid(n = 0:20, lambda = 2:4))
  pois <- pois[, p := dpois(x = n, lambda = lambda)]
  setkeyv(pois, c("n", "lambda"))
  return(pois)
}
