#vars <- list(qval = NULL, q_star = NULL, action_count = NULL)
vars <- new.env()

agent <- function(epsilon) {
  if (runif(1) < epsilon) return(sample(x = seq_len(10), size = 1))
  #futile.logger::flog.info(vars$qval)
  return(which.max(vars$qval))
}

step <- function(a, alpha) {
  r_a <- rnorm(n = 1, mean = vars$q_start[a], sd = 1)
  # # constant step size
  # vars$qval[a] <- vars$qval[a] + alpha * (r_a - vars$qval[a])
  vars$action_count[a] <- vars$action_count[a] + 1
  vars$qval[a] <- vars$qval[a] + (r_a - vars$qval[a]) / vars$action_count[a]

  # next_state, reward, is_done
  return(r_a)
}

simulate_run <- function(steps = 1000, epsilon = 0.1, alpha = 1) {
  # initial values for Q(a)
  vars$qval <- rep(0, k)
  vars$action_count <- rep(0, k)
  # vars$qval <- runif(k)

  rewards <- sapply(seq_len(steps), function(i) {
    action <- agent(epsilon = epsilon)
    step(action, alpha = alpha)
  })

  dt <- data.table::data.table(steps = seq_along(rewards),
                               rewards = rewards,
                               average_reward = cumsum(rewards) / seq_along(rewards))
  return(dt)
}

main <- function() {
  set.seed(7)
  k <- 10
  num_runs <- 10
  num_steps <- 1000
  epsilon <- c(0, 0.01, 0.1)

  dt <- lapply(epsilon, function(eps) {
    dt <- lapply(seq_len(num_runs), function(i) {
      # generate a k-armed bandit task
      vars$q_start <- rnorm(n = 10, mean = 0, sd = 1)

      dt <- simulate_run(steps = num_steps, epsilon = eps, alpha = 1)
      dt[, `:=`(run = i, epsilon = eps)]
      dt
    })
    dt <- data.table::rbindlist(dt, fill = TRUE)
    dt
  })

  dt <- data.table::rbindlist(dt, fill = TRUE)

  v <- dt[, .(average_reward = sum(average_reward) / num_runs), by = .(epsilon, steps)]
  v[, epsilon := as.character(epsilon)]

  p <- ggplot2::ggplot(data = v,
                       ggplot2::aes(x = steps, y = average_reward, group = epsilon)) +
    ggplot2::geom_line(ggplot2::aes(colour = epsilon))

  p <- ggplot2::ggplot(data = dt[epsilon == 0.1],
                       ggplot2::aes(x = steps, y = average_reward, group = epsilon)) +
    ggplot2::geom_line()
  print(p)
}

