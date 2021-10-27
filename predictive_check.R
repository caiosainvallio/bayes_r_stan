
# Worklow de modelos Bayesianos

#               Prior           Posterior
#             Predictive       Predictive
#               check            check
#            <--------->      <--------->
# especificacao       Elicitação        Inferência
#   do modelo         das Prioris       Posterior




# Prior Predictive Check
# inspeção visual
# rstanarm  prior_PD = TRUE
# brms      sample_prior = "only"




# Posterior Predictive Check
# inspeção visual
# rstanarm  pp_check(modelo)
# brms      pp_check(modelo)  pp_check(modelo, type = "ecdf_overlap")



