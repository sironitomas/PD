# Simple Haskell Habit Tracker

## Features

- Data stored in local JSON file
- Track multiple habits, with daily occurences
- Calculation of age-weighted habit score

## How to build

You need to have [haskellstack.org](http://haskellstack.org) installed. Then, clone the repository and simply:

    cd PD/Habits/
    stack build

## How to run

### Add a new habit

    stack exec Habits-exe add habits.json Meditate

### Mark habit occurence

    stack exec Habits-exe mark habits.json Meditate 2020-02-28

### View habits and scores

    stack exec Habits-exe view habits.json

### Remove an habit

    stack exec Habits-exe remove habits.json Meditate
