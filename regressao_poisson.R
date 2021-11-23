############################################################################
#                      Regressao Poisson                                   #
############################################################################



# Exemplo dataset Roaches Gelman, 2007

# É uma basse de dados com 262 observacoes sonre a eficacia de um sistema
# de controle de pragas em reduzir o numero de baratas (roaches)
# em apartamentos urbanos.

# Variaveis:
#
# y: variavel dependente - numero de baratas mortas
# roach1: numero de baratas antes da detetizacao
# treatment: dummy para indicar se o apto foi detetizado
# senior: dummy para indicar se ha apenas idosos no apto
# exposure2: numero de dias de exposicao a detetizacao

library(rstanarm)
options(mc.cores = parallel::detectCores())
data("roaches")






