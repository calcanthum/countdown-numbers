# Numbers Game

## Overview
This program is a Python implementation of the Numbers Round from the game show "Countdown". Players aim to reach a target number using a combination of randomly selected numbers through arithmetic operations.

## Requirements
- Python 3.x

## Rules
- The game involves a combination of six numbers to reach a randomly generated target number.
- Players can choose how many "large numbers" (25, 50, 75, 100) are included; the rest will be "small numbers" (two sets of numbers from 1 to 10).
- The target number is a random integer between 100 and 999.
- Players input their solutions in Reverse Polish Notation (RPN).

## Scoring
- Exact match with the target number: 10 points.
- Difference of 1 to 5: 7 points.
- Difference of 6 to 10: 5 points.
- Difference greater than 10: 0 points.

## Running the Game
1. Clone or download this repository.
2. Run `python main.py` in your terminal.
3. Follow the on-screen instructions to play.

## License
This project is licensed under the GNU Affero General Public License - see the [LICENSE.md](LICENSE.md) file for details.
