module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

type EarthYears = Float
type PlanetYears = Float

orbitalPeriod :: Planet -> EarthYears
orbitalPeriod Mercury = 0.2408467
orbitalPeriod Venus = 0.61519726
orbitalPeriod Mars = 1.8808158
orbitalPeriod Jupiter = 11.862615
orbitalPeriod Saturn = 29.447498
orbitalPeriod Uranus = 84.016846
orbitalPeriod Neptune = 164.79132

toPlanetYears :: EarthYears -> Planet -> PlanetYears
toPlanetYears years planet = years / orbitalPeriod planet

ageOn :: Planet -> Float -> Float
ageOn Earth seconds = seconds / 31557600
ageOn planet seconds = ageOn Earth seconds `toPlanetYears` planet
