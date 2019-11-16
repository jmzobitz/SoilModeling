library(devtools)

### Author: JMZ
### Modified: 3/17/19
### Purpose: Identify the defaults for the MCMC method

### STEP 3: identify the seed value for the MCMC chain and other MCMC characteristics
NUM_AT_ONCE = 10000 #NEEDED
NCHAINS = 5 # NEEDED
BURN_ITER = 70000 # NEEDED Number of runs we do after convergences, for burn-in
ESTIMATE_ITER = 80000 # Number of runs that we actually record data
MAX_ITER = 150000  # NEEDED
CHAIN_TUNING = TRUE;    # Do we tune parameters while we start from chains?
ESTIMATE_TUNING = FALSE;    # Do we tune parameters while we estimate?

percentIter = 0.5    # Percentage of the iterations we will randomly sample after parameter estimation


### I guess the <<- assigns a global value to the variable.
A_STAR <-0.4  # target acceptance rate
DEC <-0.99  # how much to decrease temp. by on rejection
INC <- DEC^((A_STAR - 1)/A_STAR);
# want INC^A_STAR * DEC^(1 - A_STAR) = 1

THRESH <- 0.05  # how close we have to get to A_STAR before stop adjusting temperatures


mcmc_defaults <- data.frame(NUM_AT_ONCE,NCHAINS,BURN_ITER,ESTIMATE_ITER,MAX_ITER,CHAIN_TUNING,ESTIMATE_TUNING,percentIter,A_STAR,DEC,INC,THRESH)

use_data(mcmc_defaults,overwrite=TRUE)


### Define a lighter version of the code for quick analysis
### STEP 3: identify the seed value for the MCMC chain and other MCMC characteristics
NUM_AT_ONCE = 100 #NEEDED
NCHAINS = 3 # NEEDED
BURN_ITER = 200 # NEEDED Number of runs we do after convergences, for burn-in
ESTIMATE_ITER = 300 # Number of runs that we actually record data
MAX_ITER = 500  # NEEDED
CHAIN_TUNING = TRUE;    # Do we tune parameters while we start from chains?
ESTIMATE_TUNING = FALSE;    # Do we tune parameters while we estimate?

percentIter = 0.5    # Percentage of the iterations we will randomly sample after parameter estimation


### I guess the <<- assigns a global value to the variable.
A_STAR <-0.4  # target acceptance rate
DEC <-0.99  # how much to decrease temp. by on rejection
INC <- DEC^((A_STAR - 1)/A_STAR);
# want INC^A_STAR * DEC^(1 - A_STAR) = 1

THRESH <- 0.05  # how close we have to get to A_STAR before stop adjusting temperatures


mcmc_superfast <- data.frame(NUM_AT_ONCE,NCHAINS,BURN_ITER,ESTIMATE_ITER,MAX_ITER,CHAIN_TUNING,ESTIMATE_TUNING,percentIter,A_STAR,DEC,INC,THRESH)

use_data(mcmc_superfast,overwrite=TRUE)

