policyIteration <- function() {
  state_space_l1 <- state_space_l2 <- 0:20
  # number of cars move from 1st to 2nd location
  action_space <- -5:5
  rent_fee <- 10
  moving_expense <- 2
  
  # 1. Initialization
  init <- expand.grid(state0_l1 = state_space_l1,
                      state0_l2 = state_space_l2, state_value = 0)
  state_action_set <- expand.grid(state0_l1 = state_space_l1,
                                  state0_l2 = state_space_l2, action = action_space)
  data.table::setDT(init)
  data.table::setDT(state_action_set)
  # possible state actions are between 0 and 20
  state_action_set <- state_action_set[(state0_l1 - action) %between% c(0,20) &
                                         (state0_l2 + action) %between% c(0, 20)]
  
  # init state-value function
  set.seed(7)
  init[, state_value := runif(.N, -1, 1)]
  
  # init policy pi(s) = a where a is from A(s), i.e. set of all possible action for state s
  # v <- state_action_set[, .(action_space = list(action)),
  #                       by = c("state0_l1", "state0_l2")]
  v <- state_action_set[, .(action = sample(x = action, size = 1)),
                        by = c("state0_l1", "state0_l2")]
  init <- init[v, on = c("state0_l1", "state0_l2")]
  setorder(init, state0_l2, state0_l1)
  
  
  trans_prob <- getTransitions()
  
  v = optiRum::CJ.dt(X = init, Y = trans_prob)
  # this approach does not includes only randomly pre-selected actions
  v[, `:=`(state1_l1 = state0_l1 - action + ret1 - req1,
           state1_l2 = state0_l2 + action + ret2 - req2)]
  
  # 2. Policy Evaluation
  iter <- 0
  eps <- .Machine$double.eps^(1/4)
  delta <- 1
  repeat {
    delta = 0
    init[, old_state_value := state_value]
    # compute requests at location 1 and 2
    
    v[, `:=`(state1_l1 = state0_l1 - action + ret1 - req1,
             state1_l2 = state0_l2 + action + ret2 - req2)]
    # compute reward
    reward <- init[, .(rent_fee * (pmin(state0_l1 - action, req_l1) + pmin(state0_l2 + action, req_l2)) -
                         moving_expense * action)]
    #init[]
    # need P[s' | s & a ] to continue
    # init[, state_value := ]
    
    if (delta < eps || iter > 100) break
  }
  View(head(init))
}

getTransitions <- function() {
  # the requests and returns at location 1 and 2 are
  # independent of the starting state for a given day
  trans_prob <- expand.grid(req1 = 0:20, ret1 = 0:20,
                            req2 = 0:20, ret2 = 0:20)
  data.table::setDT(trans_prob)
  trans_prob[, prob := dpois(x = req1, lambda = 3) * dpois(x = ret1, lambda = 3) *
                       dpois(x = req2, lambda = 4) * dpois(x = ret2, lambda = 2)]
  #View(head(trans_prob))
  return(trans_prob)
}
