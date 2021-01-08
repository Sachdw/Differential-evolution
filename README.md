# Differential-evolution

This is a simple program written in R to demonstrate an implementation of the differential evolution algorithm, an evolutionary algorithm with a wide range of applications related to optimization problems.

In this example, it has been adapted to solve a logic grid puzzle with a set of constraints. However it can be applied to a range of different problems including problems related to flight scheduling, route planning, parts selection etc.

The puzzle assumes 5 cars are parked in a row from left to right at a rental car place at an Airport terminal.
The aim of this program is to solve a logic grid puzzle to find: Which car was going to Port Macquarie? and Which car was hired by a Canadian couple?
The following constraints are assumed:

1. The Toyota Camry was hired at 6:00am by a British couple.
2. The car in the middle had a black colour.
3. The Hyundai Accent left the depot at 9:00am.
4. The Holden Barina with a blue colour was to the left of the car that carries the British couple.
5. To the right of the car hired by a French lady was the car going to Gold Coast.
6. The Nissan X-Trail was heading for Sydney.
7. To the right of the car carrying a Chinese businessman was the car with a green colour.
8. The car going to Newcastle left at 5:00am.
9. The Honda Civic left at 7:00am and was on the right of the car heading for Gold Coast.
10. The car with a red colour was going to Tamworth.
11. To the left of the car that left at 7:00am was the car with a white colour.
12. The last car was hired by an Indian man.
13. The car with a black colour left at 8:00am.
14. The car carrying an Indian man was to the right of the car hired by a Chinese businessman.
15. The car heading for Tamworth left at 6:00am.

The key part to this algorithm is mutation, where generates a set number of solutions in a generation, randomly selects three solutions and uses mutation to generate a new solution. The new solution is then compared to the current best solution (as defined by a fitness function) and if it is better, it is kept as the new best solution. Otherwise it is discarded.

The mutation formula is:
      
      a + F*(b-c)

Where a, b, and c are the three random solutions, and F is a mutation rate that we can specify.

To conceptualize the constraints of the problem, I implemented them as a binary matrix with each column representing a car, and each row representing a variable. 

The program is written in R for the sake of simplicity to take advantage of the ease of representing matrices. To run the program you can use R studio or run it in the terminal by typing: "R" and the file name.
