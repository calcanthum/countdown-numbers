# Countdown Numbers Round

## Overview
These are various implementations of the Numbers Round from the game show "Countdown". Players aim to reach a target number using a combination of randomly selected numbers and arithmetic operations.

## Rules
- The game involves manipulating a combination of six numbers with standard operations (add, sub, mul, div) to reach a randomly generated target number.
- Players can choose how many "large numbers" (25, 50, 75, 100) are included; the rest will be "small numbers" (two sets of numbers from 1 to 10).
- The target number is a random integer between 100 and 999.
- Players input their solutions in Reverse Polish Notation (Python + Rust) or infix (bash).

## Scoring
- Exact match with the target number: 10 points.
- Difference of 1 to 5: 7 points.
- Difference of 6 to 10: 5 points.
- Difference greater than 10: 0 points.

## License
This project is licensed under the GNU Affero General Public License - see the [LICENSE.md](LICENSE.md) file for details.
