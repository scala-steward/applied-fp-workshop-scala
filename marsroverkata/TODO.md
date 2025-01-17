# Enanched Mars Rover Kata

You’re part of the team that explores Mars by sending remotely controlled vehicles to the surface of the planet.
Implement an application that simulates the movement of a rover on a planet with data provided by user.

NOTE: for each version try to first model all functions signatures (use ??? marker) and then implement them.
The idea is to get a quick and cheap responsibility distribution phase.

For a full kata explanation see: https://kata-log.rocks/mars-rover-kata

## V1 - Focus on the center (pure domain logic and types)

Develop an API that translates the commands sent from earth to instructions that are executed by the rover.

- The planet is divided into a grid with x (width) and y (height) size.
- The rover has a position expressed as x, y co-ordinates and an orientation (North, Est, West, South).
- The rover can handle four commands: turn left or right, move forward or backward.
- Commands are sent in batch (like an array)
- Implement wrapping from one edge of the grid to another (pacman effect).

## V2 - Focus on boundaries (from primitive types to domain types and viceversa)

Our domain is composed by rich types but input/output data must be privitive

- Write a parser for the planet (grid) size: "5x4"
- Write a rover initial state parser: "1,3:W"
- Write an rendering as string: "positionX:positionY:direction"

## V3 - More domain logic (partial function in domain logic)

We discover that there are obstacles on the planet.

- domain logic:
  - An obstacle has a position expressed as x, y co-ordinates.
  - There are many obstacles.
  - Implement obstacle detection before each move to a new position.
  - If a given sequence of commands encounters an obstacle, the rover moves up to the last possible point and aborts the sequence.
- boundaries:
  - Write a parser for a list of obstacles: "1,2 0,0 3,4"
  - Update rendering, show hit obstacle info: "O:positionX:positionY:direction"

## V4 - Focus on I/O (compose pure IO values)

Extend the "pure" way of work also to the infrastructural layer

- Build initial state and execute all commands:
  - Read planet.txt from file (size and obstacles)
  - Read rover.txt from file (position and direction)
  - Read commands from console (ask to the user)
- After commands execution:
  - Print final output to the console (happy and not happy paths)
  - Handle, in a safe way, any unhandled exception and log them
