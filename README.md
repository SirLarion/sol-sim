# solar-sim

Solar system simulator for modeling the movements of celestial bodies. Made as a course project for Aalto University course CS-C2120.

<img src=documentation/screenshots/main1.png></img>

## Building
To rebuild from source, run
```
sbt clean package
```
in the project root. <br></br>
(Requires sbt https://www.scala-sbt.org/)

## Using
Run solar-sim.jar with java or run directly in sbt with
```
sbt run
```
in the project root.

## Credits
Data for the default displayed solar system gotten from JPL:s planetary ephemerides: 
- https://ssd.jpl.nasa.gov/horizons.cgi

4th order Runge-Kutta method for calculating orbital movements: 
- http://spiff.rit.edu/richmond/nbody/OrbitRungeKutta4.pdf
- https://gafferongames.com/post/integration_basics/
- https://en.wikipedia.org/wiki/Runge-Kutta_methods
