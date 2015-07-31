# Solar Irradiance
Bayesian approach to probabilistic clear sky models. Solar irradiance data is
taken from the University of Oregon Solar Monitoring Laboratory:
http://solardat.uoregon.edu/.

Clear Sky Detection
===================
Clear sky periods are detected by calculating multiple criterion during a
rolling window period. If the differences between the observed irradiance and
clear sky model being used are within a certain margin of error, the entire
window is declared clear. Only clear period are used to fit the model.
An example of the result on a single day is shown below:


![example](https://raw.githubusercontent.com/dslaw/Solar-irradiance/master/plots/example.png)
