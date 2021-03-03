

valoresprimos=c(160482019,160482053,160482059,160482163,160482167,160482181,160482187,160482197,160482221,160482229,160482263,160482269,160482313,160482331,160482349,160482359,160482373,160482389,160482391,160482403,160482457,160482473,160482503,160482529,160482541,160482551,160482563,160482577,160482583,160482599,160482613,160482631,160482659,160482667,160482697,160482719,160482739,160482743,160482757,160482767,160482811,160482821,160482853,160482857,160482869,160482893,160482899,160482929,160482947,160482977,160483021,160483031,160483039,160483087,160483091,160483093,160483163,160483229,160483241,160483261,160483313,160483321,160483327,160483339,160483343,160483369,160483387,160483403,160483409,160483441,160483447,160483507,160483513,160483549,160483567,160483577,160483591,160483601,160483613,160483637,160483649,160483663,160483667,160483679,160483699,160483727,160483733,160483751,160483759,160483787,160483789,160483819,160483847,160483853,160483867,160483879,160483891,160483901,160483909,160483919,160484017,160484021,160484047,160484113,160484141,160484153,160484167,160484201,160484221,160484237,160484251,160484263,160484323,160484333,160484383,160484399,160484411,160484413,160484447,160484449,160484459,160484479,160484491,160484531,160484591,160484603,160484627,160484633,160484663,160484683,160484707,160484713,160484719,160484741,160484749,160484767,160484789,160484801,160484809,160484827,160484833,160484843,160484867,160484893,160484903,160484939,160484957,160484959,160484977,160484983,160484993,160484999,160485011,160485023,160485121,160485173,160485181,160485187,160485197,160485203,160485209,160485257,160485271,160485277,160485293,160485313,160485359,160485361,160485373)

primo <- function(n) {
  if (n == 1 || n == 2) {
    return(TRUE)
  }
  if (n %% 2 == 0) {
    return(FALSE)
  }
  for (i in seq(3, max(3, ceiling(sqrt(n))), 2)) {
    if ((n %% i) == 0) {
      return(FALSE)
    }
  }
  return(TRUE)
  for (noprimo in (valoresprimos[1] + 1):(valoresprimos[2]-1))
  {print(noprimo)}
}


original <- valoresprimos
invertido <- valoresprimos
tiempoaleatorio <- sample(valoresprimos) 
replicas <- 10
suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))
tiempooriginal <-  numeric()
tiempoinvertido <-  numeric()
tiempoale <-  numeric()
for (r in 1:replicas) {
  tiempooriginal <- c(tiempooriginal, system.time(foreach(n =original, .combine=c) %dopar% primo(n))[3]) # de menor a mayor
  tiempoinvertido <- c(tiempoinvertido, system.time(foreach(n =invertido, .combine=c) %dopar% primo(n))[3]) # de mayor a menor
  tiempoaleatorio <- c(tiempoale, system.time(foreach(n = tiempoaleatorio, .combine=c) %dopar% primo(n))[3]) # orden aleatorio
}
stopImplicitCluster()

summary(tiempooriginal)
summary(tiempoinvertido)
summary(tiempoale)
