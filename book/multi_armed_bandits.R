#vars <- list(qval = NULL, q_star = NULL, action_count = NULL)
vars <- new.env()

#' Call the agent to take an action
#'
#' @param epsilon exploration probability/rate
#'
#' @return integer - the action from 1 to number of bandit arms.
#' @export
#'
#' @examples
agent <- function(epsilon) {
  # if (abs(epsilon) < .Machine$double.eps ** 0.25 && any(vars$action_count == 0)) {
  #   # when epsilon == 0, select any actions which have not been taken yet
  #   return(sample(x = which(vars$action_count == 0), size = 1))
  # }
  if (runif(1) < epsilon) return(sample(x = seq_len(vars$k), size = 1))
  
  # select the action with highest Qval, breaking ties randomly)
  max_id <- which(vars$qval == max(vars$qval))
  if (length(max_id) > 1) max_id <- sample(max_id, 1)
  return(max_id)
}

#' Passes an action to the RL environment and obtains a reward
#'
#' @param a action, integer from 1 to the number of bandit arms
#' @param alpha step-size parameter
#'
#' @return numeric reward from the given action
#' @export
#'
#' @examples
step <- function(a, alpha) {
  # Actual reward is selected from a normal distribution with mean q_star(action) and variance 1
  r_a <- rnorm(n = 1, mean = vars$q_star[a], sd = 1)
  # # constant step size
  # vars$qval[a] <- vars$qval[a] + alpha * (r_a - vars$qval[a])
  vars$action_count[a] <- vars$action_count[a] + 1
  vars$qval[a] <- vars$qval[a] + (r_a - vars$qval[a]) / vars$action_count[a]

  # next_state, reward, is_done
  return(r_a)
}

#' Simulate a run for the k-armed testbed
#'
#' @param steps number of time steps
#' @param epsilon exploration probability
#' @param alpha step-size parameter
#'
#' @return data.table with reward, average reward and optimal action for each of the time steps
#' @export
#'
simulate_run <- function(steps = 1000, epsilon = 0.1, alpha = 1) {
  # initial values for Q(a)
  vars$qval <- rep(0, vars$k)
  vars$action_count <- rep(0, vars$k)
  # vars$qval <- runif(vars$k)
  
  optimal_action <- rep(NA, steps)
  rewards <- rep(NA_real_, steps)
  
  for (i in seq_len(steps)) {
    action <- agent(epsilon = epsilon)
    if (action == which.max(vars$q_star)) optimal_action[i] <- T else optimal_action[i] <- F
    rewards[i] <- step(action, alpha = alpha)
  }

  dt <- data.table::data.table(steps = seq_along(rewards),
                               rewards = rewards,
                               average_reward = cumsum(rewards) / seq_along(rewards),
                               optimal_action = optimal_action)
  return(dt)
}

#' Repeats independent runs of the different bandit problems
#'
#' @return
#' @export
#' 
#' @details Generates Figure 2.2 on p. 29 of Sutton and Barto (2018)
#'
#' @examples
main <- function() {
  set.seed(0)
  k_armed <- 10
  vars$k <- k_armed
  num_runs <- 2000
  num_steps <- 1000
  epsilon <- c(0, 0.01, 0.1)
  
  res2 <- list()
  for (i in seq_along(epsilon)) {
    eps <- epsilon[i]
    res <- list()
    for (j in seq_len(num_runs)) {
      # generate a k-armed bandit task/problem
      vars$q_star <- rnorm(n = k_armed, mean = 0, sd = 1)
      
      dt <- simulate_run(steps = num_steps, epsilon = eps, alpha = 1)
      dt[, `:=`(run = j, epsilon = eps)]
      res[[j]] <- dt
    }
    res2[[i]] <- data.table::rbindlist(res, fill = TRUE)
  }
  dt <- data.table::rbindlist(res2, fill = TRUE)

  v <- dt[, .(average_reward = mean(rewards), # sum(average_reward) / num_runs,
              opt_act_p = mean(optimal_action)), by = .(epsilon, steps)]
  v[, epsilon := as.character(epsilon)]
  # Generate Figure 2.2 on p. 29 of Sutton and Barto (2018)
  p1 <- ggplot2::ggplot(data = v,
                       ggplot2::aes(x = steps, y = average_reward, group = epsilon)) +
    ggplot2::geom_line(ggplot2::aes(colour = epsilon))
  
  p2 <- ggplot2::ggplot(data = v,
                       ggplot2::aes(x = steps, y = opt_act_p, group = epsilon)) +
    ggplot2::geom_line(ggplot2::aes(colour = epsilon))
  
  Rmisc::multiplot(p1, p2, cols = 1)
  
  return(0)
}

