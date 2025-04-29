#' Simulate foraging within a habitat patch
#'
#' @description
#' The function takes in the description of the local community and surrounding environmental factors.
#' Based on these, each individual within a community takes a turn in random sequence consuming the available resource.
#' All else being equal, an individual with a trait value optimal for the local environmental factor \code{env} will consume more resource.
#' If there are many individuals in the community with a similar trait value, the focal individual is less likely to consume the resource.
#' Similarly, in an individuals-rich environment, the chances of consuming anything at all are reduced.
#'
#' @param com A tibble (dataframe) representing the \strong{local} community with the following columns:
#' \code{species} - species ID;
#' \code{trait} - individual trait value;
#' \code{age} - current age of the individual;
#' \code{mass} - current body mass of the individual;
#' \code{lifespan} - maximum lifespan after reaching which the individual will perish;
#' \code{repmass} - body mass after reaching which the individual will asexually reproduce into two children individuals.
#'
#' @param env Numeric, value of the environmental factor in the patch.
#'
#' @param res Numeric, value of the resource available within the patch.
#'
#' @param res_input Numeric, increment of the resource level within a time step.
#'
#' @param R Numeric, resource level at which all individuals within a community successfully consume the resource (i.e., probability of resource consumption equals one).
#'
#' @param clustering Numeric, effect of niche clustering on probability of competition. Default to one.
#'
#' @param dispersion Numeric, effect of niche dispersion on probability of trait filtering by the environment. Default to one.
#'
#' @returns A list with the following elements:
#' \code{[["com"]]} - updated community object,
#' \code{[["res"]]} - available resource level,
#' \code{[["res_input"]]} - increment of the resource level within a time step,
#' \code{[["p_compete"]]} - sum of foraging probabilities given competition,
#' \code{[["p_consume"]]} - sum of foraging probabilities given resource availability.
#'
#' @export
#'
#' @examples
#' x = init_meta()
#' y = draw_lcom(x, 100)
#' forage(com = y, env = 0, res = 1000, res_input = 10, R = 1000)
#'
forage <- function(com, env, res, res_input, R, clustering = 1, dispersion = 1){
  #
  # `com` - local community object with the following columns:
  #   `$species` - species ID
  #   `$trait` - individual trait value
  #   `$age` - age of the individual
  #   `$mass` - body mass
  #   `$lifespan` - maximum lifespan
  #   `$repmass` - mass when reproduction occurs
  # `env` - environmental factor value
  # `res` - available resource level
  # `res_input` - increase in resource level per time step
  # `R` - resource level at which (or greater than which) all individuals consume resource (probability = 1)
  #
  # Output: a list of
  #   [["com"]] - updated community object
  #   [["res"]] - updated resource value
  #   [["res_consumed"]] - sum of consumed resource
  #   [["p_compete"]] - sum of foraging probabilities given competition
  #   [["p_consume"]] - sum of foraging probabilities given resource availability

  res <- res + res_input
  res <- ifelse(res < 0, 0, res)

  idx <- sample(1:nrow(com), size = nrow(com)) # randomize order of foraging individuals

  com_traits <- com$trait %>% unlist() # trait values within community

  res_consumed <- 0
  sum_p_compete <- 0
  sum_p_consume <- 0

  for (i in idx){
    bmass <- com[i, "mass"] %>% unlist() %>% unname() # body mass of an individual
    # account for resource availability
    p_resource <- res/R # need for each cycle
    p_resource <- ifelse(p_resource > 1, 1, p_resource)
    # account for environmental filtering
    t <- com[i, "trait"] %>% unlist() %>% unname() # individual trait value
    p_filter <- 1 - (1/(1 + exp(-log(abs(t - env))))) # logistic probability based on difference between trait and env
    # account for interactions
    d <- sapply(com_traits[-i], function(x){
      abs(x - t)
    }) # %>% sort() # sorted absolute difference between focal individual trait and community traits
    # w <- (length(d):1)/sum(1:length(d)) # weights for differences; smallest diff gets higher weights
    # mwd <- mean(w*d) # mean weighted distance
    mwd <- ifelse(length(d) == 0, Inf, mean(d)) # if nobody to compete with, next step will be 1
    p_compete <- (1/(1 + exp(-log(mwd)))) # low probabilities for low MWD (= there are many similar individuals)
    # tweak clustering / dispersion, purely for developing purposes
    p_compete <- p_compete * dispersion
    p_filter <- p_filter * clustering
    #
    feed <- runif(1) <= p_resource*(p_resource + p_compete - p_resource*p_compete) # foraging given resource availability and competition are inclusive
    mass_increase <- feed*bmass*p_filter # how much more the individual gets
    res <- res - mass_increase # exclude from the resource pool
    com[i, "mass"] <- bmass + mass_increase # write updated body mass
    res_consumed <- res_consumed + mass_increase
    sum_p_compete <- sum_p_compete + p_compete
    sum_p_consume <- sum_p_consume + p_resource
  }
  return(list(com = com,
              res = res,
              consumed = res_consumed,
              p_compete = sum_p_compete,
              p_consume = sum_p_consume))
}
