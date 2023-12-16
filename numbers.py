import random
import operator

def generate_game(large_count):
    # Consolidate input checks
    if not isinstance(large_count, int) or not 0 <= large_count <= 6:
        return "Invalid input: Enter a number between 0 and 6.", []

    large_numbers = random.sample([25, 50, 75, 100], large_count)
    small_numbers = random.sample(list(range(1, 11)) * 2, 6 - large_count)

    game_numbers = large_numbers + small_numbers
    target_number = random.randint(100, 999)

    return target_number, game_numbers

def evaluate_rpn_expression(expression, game_numbers):
    stack = []
    operators = {'+', '-', '*', '/'}

    for token in expression.split():
        if token in operators:
            if len(stack) < 2:
                return "Invalid RPN expression: not enough operands."
            b, a = stack.pop(), stack.pop()
            if token == '/' and b == 0:
                return "Invalid expression: Division by zero."
            result = {'+': operator.add, '-': operator.sub, '*': operator.mul, '/': operator.truediv}[token](a, b)
            stack.append(result)
        else:
            try:
                number = int(token)
                if number not in game_numbers:
                    return f"Invalid expression: Number {number} is not in this round."
                if game_numbers.count(number) < expression.split().count(token):
                    return f"Invalid expression: Number {number} used more times than it appears in the game."
                stack.append(number)
            except ValueError:
                return f"Invalid token in expression: '{token}' is not a number."

    if len(stack) != 1:
        return "Invalid RPN expression: Incorrect number of elements remaining."

    return stack[0]

def calculate_points(target, result):
    difference = abs(target - result)
    if difference == 0:
        return 10
    elif difference <= 5:
        return 7
    elif difference <= 10:
        return 5
    else:
        return 0

# Interactive part
print("Welcome to the Numbers Game!")

while True:
    large_count_input = input("How many large numbers? ")
    if not large_count_input.isdigit():
        print("Invalid input. Please enter a valid number.")
        continue

    large_count = int(large_count_input)
    if large_count < 0:
        print("Please enter a non-negative number.")
        continue

    target_number, game_numbers = generate_game(large_count)
    if not game_numbers:
        print("Error: Invalid number of large numbers.")
        continue
    break

print("Target Number:", target_number)
print("Game Numbers:", game_numbers)

while True:
    user_expression = input("Enter your solution in Reverse Polish Notation: ")
    result = evaluate_rpn_expression(user_expression, game_numbers)

    if isinstance(result, int):
        print("Result of Expression:", result)
        points = calculate_points(target_number, result)
        print(f"You scored {points} points!")
        break
    else:
        print(result)
        print("Please try again.")
