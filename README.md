# Countdown Numbers Round

## Overview
These are various implementations of the Numbers Round from the game show "Countdown". Players aim to reach a target number using a combination of randomly selected numbers and arithmetic operations. This is a personal study project to explore how different languages handle the same things (stacks + random numbers).

## Rules
- Only standard operations (addition, subtraction, multiplication, division) are allowed.
- Players can choose how many "large numbers" (25, 50, 75, 100) are included; the rest will be "small numbers" (two sets of numbers from 1 to 10).
- The target number is a random integer between 100 and 999.
- Each number can be used at most once in the solution.
- Players input their solutions in Reverse Polish Notation (RPN aka postfix).

## Scoring
- Exact match with the target number: 10 points.
- Difference of 1 to 5: 7 points.
- Difference of 6 to 10: 5 points.
- Difference greater than 10: 0 points.

## Version Notes
- Python
- Bash
  - Had to modulo 900 and offset by 100 to get the right range. Probability skew to the lower numbers as a result.
- Rust
- Haskell

## License
This project is licensed under the GNU Affero General Public License - see the [LICENSE.md](LICENSE.md) file for details.
