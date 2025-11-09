import pandas as pd
import numpy as np
import statistics
import matplotlib.pyplot as plt

data={
    "ID":[1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
    "gender":["man", "woman","man", "woman","man", "woman","woman", "man","woman", "man"],
    "Exam score":[14, 16, 12, 17, 10, 18, 15, 13, 16, 11],
    "height(kg)":[55, 63, 68, 50, 68, 80, 58, 85, 62, 78],
    "weight(cm)":[165, 170, 180, 158, 170, 175, 160, 182, 168, 177],
    "age":[18, 20, 21, 19, 22, 23,24, 21, 19, 22],
}
data=pd.DataFrame(data)
import statistics
import numpy as np
def q(x):
    q1=np.quantile(x, 0.25)
    q2=np.quantile(x, 0.5)
    q3=np.quantile(x, 0.75)
    return q1, q2, q3
def descriptive_stats(x):
    mean_x=np.mean(x)
    median_x=np.median(x)
    mode_x=statistics.mode(x)
    var_x=np.var(x)
    std_x=np.std(x)
    q1, q2, q3 =q(x)
    return print(f" mena:{mean_x} \n meddian: {median_x} \n mode:{mode_x} \n Variance: {var_x:.2f} \n standard deviation:{std_x:.2f}\n quantile:{q1, q2, q3}")
y=data['height(kg)']
dic1=descriptive_stats(y)
plt.hist(y)
