############################################################################
#                      Regressao Linear                                    #
############################################################################


# dataset
data("kidiq")
?kidiq


# Pacotes
library(rstanarm)
library(brms)

# multi-threading
options(mc.cores = parallel::detectCores())
options(Ncpus = parallel::detectCores())
options(brms.backend = "cmdstanr", brms.normalize = FALSE)




