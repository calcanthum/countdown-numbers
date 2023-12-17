#!/bin/bash

generate_numbers() {
    local large_count=$1
    local large_numbers=(25 50 75 100)
    local small_numbers=({1..10} {1..10})
    local game_numbers=()

    for i in $(seq 1 $large_count); do
        local rand_index=$((RANDOM % ${#large_numbers[@]}))
        game_numbers+=("${large_numbers[$rand_index]}")
        unset 'large_numbers[rand_index]'
    done

    for i in $(seq 1 $((6 - large_count))); do
        local rand_index=$((RANDOM % ${#small_numbers[@]}))
        game_numbers+=("${small_numbers[$rand_index]}")
        unset 'small_numbers[rand_index]'
    done

    echo "${game_numbers[@]}"
}

calculate_points() {
    local target=$1
    local result=$2
    local difference=$((target > result ? target - result : result - target))

    if [ $difference -eq 0 ]; then
        echo 10
    elif [ $difference -le 5 ]; then
        echo 7
    elif [ $difference -le 10 ]; then
        echo 5
    else
        echo 0
    fi
}

validate_expression() {
    local expression=$1
    local game_numbers=("${@:2}")

    declare -A count_map
    for num in "${game_numbers[@]}"; do
        ((count_map[$num]++))
    done

    for num in $(echo $expression | grep -o '[0-9]\+'); do
        if [[ ${count_map[$num]} -gt 0 ]]; then
            ((count_map[$num]--))
        else
            echo "Invalid expression: Number $num is not in the game numbers or is used more times than it appears."
            return 1
        fi
    done
}

echo "Welcome to the Numbers Game!"
read -p "How many large numbers? " large_count

if ! [[ $large_count =~ ^[0-6]$ ]]; then
    echo "Invalid input. Please enter a number between 0 and 6."
    exit 1
fi

game_numbers=($(generate_numbers $large_count))
target_number=$((RANDOM % 900 + 100))

echo "Target Number: $target_number"
echo "Game Numbers: ${game_numbers[*]}"

while true; do
    read -p "Enter your solution: " user_expression

    if validate_expression "$user_expression" "${game_numbers[@]}"; then
    
        result=$(bc <<< "$user_expression")
        echo "Result of Expression: $result"

        points=$(calculate_points $target_number $result)
        echo "You scored $points points!"
        break
    else
        echo "Please try again."
    fi
done
