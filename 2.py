import random
from scipy.stats import norm
import numpy as np
random.seed(42)
x=random.random()
x1=norm.logpdf(x, loc=5, scale=1)
print(f"logscf of normal {x1}")
# Log of the cumulative distribution function.
x2=norm.cdf(x, loc=5, scale=1)
print(f"logaritmo cdf of noramal :{x2}")
# Survival function (also defined as 1 - cdf, but sf is sometimes more accurate).
print(f"Survival function : {norm.sf(x, loc=5, scale=1)}")
print(f"logaritmo Survival function : {norm.logsf(x, loc=5, scale=1)}")