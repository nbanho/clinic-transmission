days <- list(
  mth = c(rep("10", 3), rep("11", 2)),
  dy = c("13", "15", "25", "04", "05")
)

scenarios <- c(
  "main",
  "nomasks",
  "vent",
  "nomasksvent",
  "suspected",
  "prevalence",
  "temporal"
)

for (s in 1:length(scenarios)) {
  for (d in 1:length(days[[1]])) {
    rcall <- sprintf(
      "R CMD BATCH --'args mth=\"%s\" dy=\"%s\" scenario=\"%s\" cellsize=250 nsim=5000 ncores=8' simulations/simulate_risk.R simulations/Routputs/%s_%s_%s.Rout",
      days$mth[d],
      days$dy[d],
      scenarios[s],
      scenarios[s],
      days$mth[d],
      days$dy[d]
    )
    hpc_settings <- sprintf(
      "#!/bin/bash
#SBATCH --job-name=\"%s%s%s\"
#SBATCH --time=23:00:00
#SBATCH --ntasks=8
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=3G
#SBATCH --partition=epyc2

# Your code below this line
module load Workspace
module load Workspace_Home
module load R",
      scenarios[s],
      days$mth[d],
      days$dy[d]
    )
    writeLines(
      c(hpc_settings, rcall),
      paste0(
        "simulations/hpc-runs/",
        scenarios[s], "_",
        days$mth[d], "_",
        days$dy[d],
        ".sh"
      ),
      useBytes = FALSE
    )
    writeLines(
      c(rcall),
      paste0(
        "simulations/local-runs/",
        scenarios[s], "_",
        days$mth[d], "_",
        days$dy[d],
        ".sh"
      ),
      useBytes = FALSE
    )
  }
}
