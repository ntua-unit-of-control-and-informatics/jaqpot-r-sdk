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

PFOS_rat_pbk_modelID = "1557"  # Loccisano et al. 2012
# Seacat et al. 2002 - PFOS exposure - LOAEL
pfos_bw <- 0.282  # kg
pfos_daily_intake_per_kg <- 1  # mg/kg/day
pfos_daily_intake_per_rat <- pfos_daily_intake_per_kg * pfos_bw  # mg/day
pfos_exposure_duration <- 14 * 7  # days

exposure_scenario <- list("admin.type"= "oral",
                        "admin.dose"= list(rep(pfos_daily_intake_per_rat, pfos_exposure_duration)),
                        "admin.time"= list(seq(0, pfos_exposure_duration * 24-1, 24)),
                         "BW"= 0.282,
                         "BW.times"= 0,
                        "F_unabs"= 0.0,
                        "sex"= "M",
                        "sim.start"= 0,
                        "sim.end"= pfos_exposure_duration * 24,
                        "sim.step"= 1)

jaqpot.predict( df = exposure_scenario, modelID = PFOS_rat_pbk_modelID, 
                url = "https://api.jaqpot.org/", envFile =envFile)
