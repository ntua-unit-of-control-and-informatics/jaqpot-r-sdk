envFile <- "jaqpot.env"
modelID = "1933"
df <-list(
  "weight" = 80,
  "dose" = 10,
  "administration.time" = 0.1,
  "sim.start" = 0,
  "sim.step" = 1,
  "sim.end" = 10
)

jaqpot.predict( df, modelID, url = "https://api.jaqpot.org/", envFile =envFile)
