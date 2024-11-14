hpc_settings <- "#!/bin/bash
#SBATCH --job-name=\"prevalence_10_13\"
#SBATCH --time=7-23:00:00
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=10G
#SBATCH --partition=epyc2

# Your code below this line
module load Workspace
module load Workspace_Home
module load R"


days <- list(
  mth = c(rep("10", 3), rep("11", 2)),
  dy = c("10", "13", "25", "04", "05")
)

sims <- list(
  aname = c(
    "main",
    "nomasks",
    "vent",
    "nomasksvent",
    "suspected",
    "prevalence",
    "temporal"
  ),
  is_masking = c(1, 0, 1, 0, 1, 1, 1),
  fixed_aer = c(0, 0, 6, 6, 0, 0, 0),
  who_is_tb = c(rep("du", 4), "ds", "pu", "du"),
  mod = c(rep("spattemp", 6), "temp"),
  save_quanta = c(TRUE, rep(FALSE, 6))
)

for (d in 1:length(days[[1]])) {
  for (s in 1:length(sims[[1]])) {
    rcall <- sprintf(
      "R CMD BATCH --'args cont_n=1 term_n=5000 mth=\"%s\" dy=\"%s\" aname=\"%s\" cellSize=250 is_masking=%i fixed_aer=%i who_is_tb=\"%s\" mod=\"%s\" sel_rooms=\"clinic\" save_quanta=%s' simulations/simulate_qctr.R",
      days$mth[d],
      days$dy[d],
      sims$aname[s],
      sims$is_masking[s],
      sims$fixed_aer[s],
      sims$who_is_tb[s],
      sims$mod[s],
      sims$save_quanta[s]
    )
    writeLines(
      rcall,
      paste0(
        "simulations/local-runs/",
        sims$aname[s], "-", days$mth[d], "-", days$dy[d],
        ".sh"
      ),
      useBytes = FALSE
    )
    writeLines(
      c(hpc_settings, rcall),
      paste0(
        "simulations/hpc-runs/",
        sims$aname[s], "-", days$mth[d], "-", days$dy[d],
        ".sh"
      ),
      useBytes = FALSE
    )
  }
}
