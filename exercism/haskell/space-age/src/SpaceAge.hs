module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune


ageOnEarth :: Float -> Float
ageOnEarth seconds = seconds/31557600

ageOn :: Planet -> Float -> Float
ageOn Mercury seconds = 0.2408467 * (ageOnEarth seconds)
ageOn Venus seconds = 0.61519726 * (ageOnEarth seconds)
ageOn Earth seconds = 1 * (ageOnEarth seconds)
ageOn Mars seconds = 1.8808158 * (ageOnEarth seconds)
ageOn Jupiter seconds = 11.862615 * (ageOnEarth seconds)
ageOn Saturn seconds = 29.447498 * (ageOnEarth seconds)
ageOn Uranus seconds = 84.016846 * (ageOnEarth seconds)
ageOn Neptune seconds = 164.79132 * (ageOnEarth seconds)