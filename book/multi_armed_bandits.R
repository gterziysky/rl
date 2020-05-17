#vars <- list(qval = NULL, q_star = NULL, action_count = NULL)
vars <- new.env()

#' Select the maximum value from a vector, breaking ties randomly
#'
#' @param vec input vector
#'
#' @return vector of length 1 with the maximum element of the input
#' @export
#'
#' @examples
which_max2 <- function(vec) {
  max_id <- which(vec == max(vec))
  if (length(max_id) > 1) max_id <- sample(max_id, 1)
  return(max_id)
}

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
  return(which_max2(vars$qval))
}

#' Passes an action to the RL environment and obtains a reward
#'
#' @param a action, integer from 1 to the number of bandit arms
#' @param alpha step-size parameter. If NULL sample average method is used.
#' Otherwise, a constant step-size parameter of alpha is used.
#'
#' @return numeric reward from the given action
#' @export
#'
#' @examples
step <- function(a, alpha=NULL) {
  # Actual reward is selected from a normal distribution with mean q_star(action) and variance 1
  r_a <- rnorm(n = 1, mean = vars$q_star[a], sd = 1)
  # # constant step size
  # vars$qval[a] <- vars$qval[a] + alpha * (r_a - vars$qval[a])
  vars$action_count[a] <- vars$action_count[a] + 1
  if (is.null(alpha)) {
    # incremental sample average 
    vars$qval[a] <- vars$qval[a] + (r_a - vars$qval[a]) / vars$action_count[a]
  } else {
    # constant alpha
    vars$qval[a] <- vars$qval[a] + alpha * (r_a - vars$qval[a])
  }

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
simulate_run <- function(steps = 1000,
                         epsilon = 0.1,
                         alpha = NULL,
                         stationary_q_star = T,
                         initial_qval = NULL) {
  # Generate a k-armed bandit task/problem
  if (!stationary_q_star) {
    # For exercise 2.5 on p. 33 of Sutton and Barto (2018)
    vars$q_star <- rep(0, vars$k)
  } else {
    # For Figure 2.2 on p. 29 of Sutton and Barto (2018)
    vars$q_star <- rnorm(n = vars$k, mean = 0, sd = 1)
  }
  
  # initial values for Q(a)
  vars$qval <- rep(0, vars$k)
  
  if (!is.null(initial_qval)) {
    # addition for Figure 2.3
    stopifnot(length(initial_qval) == vars$k)
    vars$qval <- initial_qval
  }
  vars$action_count <- rep(0, vars$k)
  # vars$qval <- runif(vars$k)
  
  optimal_action <- rep(NA, steps)
  rewards <- rep(NA_real_, steps)
  
  for (i in seq_len(steps)) {
    if (!stationary_q_star) {
      vars$q_star <- vars$q_star + rnorm(vars$k, mean = 0, sd = 0.01)
    }
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

#' Generates Figure 2.2 on p. 29 of Sutton and Barto (2018)
#'
#' @return
#' @export
#'
#' @examples
figure2.2 <- function() {
  set.seed(0)
  k_armed <- 10
  vars$k <- k_armed
  num_runs <- 2000
  num_steps <- 1000
  epsilon <- c(0, 0.01, 0.1)
  stationary_q_star <- T
  # report progress every 10 percent
  report_step <- as.integer(num_runs * 0.1)
  
  res2 <- list()
  for (i in seq_along(epsilon)) {
    eps <- epsilon[i]
    res <- list()
    for (j in seq_len(num_runs)) {
      # report progress
      if (j %% report_step == 0 || j == 1 || j == num_runs)
        futile.logger::flog.info("eps: %.4f, %d / %d", eps, j, num_runs)
      
      dt <- simulate_run(steps = num_steps,
                         epsilon = eps,
                         alpha = NULL, # const step size or NULL for sample-average
                         stationary_q_star = stationary_q_star)
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
    ggplot2::geom_line(ggplot2::aes(colour = epsilon)) + ggplot2::ylim(c(0, 1.5))
  
  p2 <- ggplot2::ggplot(data = v,
                        ggplot2::aes(x = steps, y = opt_act_p, group = epsilon)) +
    ggplot2::geom_line(ggplot2::aes(colour = epsilon)) + ggplot2::ylim(c(0, 1))
  
  Rmisc::multiplot(p1, p2, cols = 1)
}

#' Generate Figure 2.3 of Sutton and Barto (2018)
#'
#' @return
#' @export
#'
#' @examples
figure2.3 <- function() {
  set.seed(0)
  k_armed <- 10
  vars$k <- k_armed
  num_runs <- 2000
  num_steps <- 1000
  epsilon <- c(0, 0.1)
  stationary_q_star <- T
  # report progress every 10 percent
  report_step <- as.integer(num_runs * 0.1)
  
  res2 <- list()
  for (i in seq_along(epsilon)) {
    eps <- epsilon[i]
    res <- list()
    for (j in seq_len(num_runs)) {
      # report progress
      if (j %% report_step == 0 || j == 1 || j == num_runs)
        futile.logger::flog.info("eps: %.4f, %d / %d", eps, j, num_runs)
      # initial Q* = +5
      if (eps == 0.1) {
        # initial Q* = 0
        dt <- simulate_run(steps = num_steps,
                           epsilon = eps,
                           alpha = 0.1, # const step size or NULL for sample-average
                           stationary_q_star = stationary_q_star,
                           initial_qval = rep(0, vars$k))
        dt[, `:=`(run = j, epsilon = eps, initial_qval = 0)]
      } else if (eps == 0) {
        # initial Q* = 5
        dt <- simulate_run(steps = num_steps,
                           epsilon = eps,
                           alpha = 0.1, # const step size or NULL for sample-average
                           stationary_q_star = stationary_q_star,
                           initial_qval = rep(5, vars$k))
        dt[, `:=`(run = j, epsilon = eps, initial_qval = 5)]
      }
      res[[j]] <- dt
    }
    res2[[i]] <- data.table::rbindlist(res, fill = TRUE)
  }
  dt <- data.table::rbindlist(res2, fill = TRUE)
  
  v <- dt[, .(average_reward = mean(rewards), # sum(average_reward) / num_runs,
              opt_act_p = mean(optimal_action)), by = .(epsilon, steps)]
  v[, `:=`(epsilon = as.character(epsilon))]
  
  v[epsilon == "0", epsilon := stringr::str_replace(string = epsilon, pattern = "0", replacement = "Q1=5, eps=0")]
  v[epsilon == "0.1", epsilon := stringr::str_replace(string = epsilon, pattern = "0.1", replacement = "Q1=0, eps=0.1")]
  
  p2 <- ggplot2::ggplot(data = v,
                        ggplot2::aes(x = steps, y = opt_act_p, group = epsilon)) +
    ggplot2::geom_line(ggplot2::aes(colour = epsilon)) + ggplot2::ylim(c(0, 1))
  
  print(p2)
  
  # Rmisc::multiplot(p1, p2, cols = 1)
}

#' Repeats independent runs of the different bandit problems
#'
#' @return
#' @export
#' 
#'
main <- function() {
  figure2.2()
  figure2.3()
  return(0)
}

