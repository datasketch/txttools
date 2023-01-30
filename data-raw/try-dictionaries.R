

library(hunspell)

#Sys.setenv(DICPATH = "~/dsrepo/txttools/data-raw/titoBouzout/es/")
hunspell:::dicpath()

spanish <- dictionary("~/dsrepo/txttools/data-raw/titoBouzout/es/Spanish.dic")

txt <- "Iba caminando camino a casa bafrets con camion"

bad <- hunspell(txt, dict = spanish)

hunspell_suggest(bad[[1]])

txt <- "Iba caminando camino a casa bafrets"
hunspell_stem(txt, dict = spanish)

words <- c("Iba", "caminando", "camino", "a", "casa", "bafrets")
hunspell_stem(words, dict = spanish)


words <- c("Iba", "caminando", "camino", "a", "casa", "bafrets")
hunspell_check(words, dict = spanish)




