# Simulates the two deprivation indices used in omega_deprivation.Rmd
# Output: deprivation_data.csv (one row per person) and item_dictionary.csv
# Run once from the folder where the Rmd lives: source("simulate_data.R")
# Share only the two CSV files with students: this script reveals the true loadings.

set.seed(2026)
n <- 10000

eta     <- rnorm(n)  # true deprivation
housing <- rnorm(n)  # Index A: shared residual for damp and heating

# Index B: a second substantive factor and two nuisance components
r_ubn   <- 0.30                                       # correlation with deprivation
ubn     <- r_ubn * eta + sqrt(1 - r_ubn^2) * rnorm(n) # territorial / public services
finance <- rnorm(n)  # recent financial shock (savings, celebrations)
neigh   <- rnorm(n)  # neighbourhood insecurity and school quality (learning, violence)

# Index A: ten consensual necessities ------------------------------------------
items_A <- data.frame(
  item  = c("a_meals", "a_shoes", "a_coat", "a_fruitveg", "a_damp",
            "a_warm", "a_medicine", "a_savings", "a_celebrate", "a_appliances"),
  label = c("Two meals a day", "Two pairs of shoes", "Warm waterproof coat",
            "Fresh fruit and vegetables daily", "Damp-free home", "Keep home warm",
            "Prescribed medicines", "Small savings for emergencies",
            "Celebrations on special occasions", "Replace broken appliances"),
  loading    = c(.81, .86, .73, .79, .89, .71, .83, .76, .69, .78),
  prevalence = c(.08, .12, .15, .20, .10, .25, .18, .30, .22, .14)
)

A <- sapply(seq_len(nrow(items_A)), function(j) {
  lam <- items_A$loading[j]
  g   <- if (items_A$item[j] %in% c("a_damp", "a_warm")) 0.40 else 0
  ystar <- lam * eta + g * housing + sqrt(1 - lam^2 - g^2) * rnorm(n)
  as.integer(ystar > qnorm(1 - items_A$prevalence[j]))
})
colnames(A) <- items_A$item

# Index B: four relative deprivation items + six UBN indicators ----------------
# UBN items load mainly on a separate territorial / public services factor (ubn),
# correlated only .30 with deprivation:
#   child death and violence: almost nothing from deprivation, mostly ubn
#   dirt floor, no piped water, incomplete primary school: very rare (high severity)
#   learning: common, but mostly driven by school quality and neighbourhood
# Two nuisance pairs keep even the two-factor model from fitting well:
#   finance (savings, celebrations) and neigh (learning, violence)
items_B <- data.frame(
  item  = c("b_fruitveg", "b_dirtfloor", "b_warm", "b_learning", "b_childdeath",
            "b_savings", "b_violence", "b_water", "b_celebrate", "b_educ"),
  label = c("Fresh fruit and vegetables daily", "Dirt floor in the dwelling",
            "Keep home warm", "Child below minimum learning level",
            "Death of a child under five (last 5 years)",
            "Small savings for emergencies", "Victim of violence (last 12 months)",
            "No piped water in the dwelling", "Celebrations on special occasions",
            "Adult without completed primary school"),
  prevalence = c(.20, .03, .25, .45, .02, .30, .20, .05, .22, .03),
  l_eta      = c(.79, .45, .78, .20, .10, .76, .05, .30, .74, .35),
  l_ubn      = c(  0, .50,   0, .50, .65,   0, .60, .60,   0, .50),
  l_finance  = c(  0,   0,   0,   0,   0, .55,   0,   0, .55,   0),
  l_neigh    = c(  0,   0,   0, .50,   0,   0, .50,   0,   0,   0)
)

B <- sapply(seq_len(nrow(items_B)), function(j) {
  le <- items_B$l_eta[j];     lu <- items_B$l_ubn[j]
  lf <- items_B$l_finance[j]; ln <- items_B$l_neigh[j]
  common <- le^2 + lu^2 + 2 * le * lu * r_ubn + lf^2 + ln^2
  ystar  <- le * eta + lu * ubn + lf * finance + ln * neigh +
    sqrt(1 - common) * rnorm(n)
  as.integer(ystar > qnorm(1 - items_B$prevalence[j]))
})
colnames(B) <- items_B$item

# Write files ------------------------------------------------------------------
dat <- data.frame(id = seq_len(n), eta = round(eta, 5), A, B)

dictionary <- rbind(
  data.frame(index = "A", item = items_A$item, label = items_A$label),
  data.frame(index = "B", item = items_B$item, label = items_B$label)
)

write.csv(dat, "deprivation_data.csv", row.names = FALSE)
write.csv(dictionary, "item_dictionary.csv", row.names = FALSE)