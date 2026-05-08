
# In this section we are defining global variables



# Filter A coverage threshold
COVERAGE_TRESHOLD <- 0.9

# Filter B thresholds
GAP_THR_DAY <- 10      # big gap if >= CAP_THR_DAY minutes
BIG_GAP_THR <- 0.10    # max share of days with big gaps
MAX_GAP_ANY <- 20      # absolute max gap allowed (minutes)

# Early Close return threshold
ZERO_SHARE_THR <- 0.9

# Present Date. To divide the observations in train and test sample, we choose
# the "Present Date", past observations will be train set and future
# observations are test set. Format AAAA-MM-DD HH:MM:00
PRESENT_DATETIME <- as.POSIXct(
  "2022-03-31 14:30:00",
  tz = "America/New_York"
)

# Volatility window. Specifies the width of the window to compute volatilities
# from our stock data. Voltility is computed on windows of type:
# PRESENT_DATETIME = PD, WINDOW = W.
# Train Windows = {[PD, PD-W] , [PD-W, PD-2W], [PD-2W, PD-3W], ...}
# Test Windows = {[PD, PD+W] , [PD+W, PD+2W], [PD+2W, PD+3W], ...}
WINDOW <- 30



# Target stock is the stock used to compute the Historical Volatility Ratio. It
# is the denominator of the ratio, the term for which we express other
# stocks volatilities in relative terms to to the target stock.
TARGET_STOCK <- "PEP"



# N_EMPIRICAL_DIST is the number of observation that constitute each empirical
# distribution of HVR to assess how the distribution is changing overtime.
# The bigger N_EMPIRICA_DIST, the more accurate the empirical distribition is
# but it redices the number of distributions to compare.
N_EMPIRICAL_DIST <- 70



# Order of the Wasserstein distance used to measure distributional shifts.
# p = 1 corresponds to the average absolute displacement between quantiles,
# providing a robust measure of typical distributional changes.
p_chosen <- 1



#_____________________________END_OF_THE_SCRIPT_________________________________