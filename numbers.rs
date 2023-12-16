use rand::Rng;
use std::io;
use rand::prelude::SliceRandom;

fn main() {
    println!("Welcome to the Numbers Game!");

    let large_numbers = [25, 50, 75, 100];
    let small_numbers: Vec<i32> = (1..=10).collect();

    loop {
        let large_count = loop {
            println!("How many large numbers? ");
            let mut large_count_input = String::new();
            io::stdin()
                .read_line(&mut large_count_input)
                .expect("Failed to read line");

            match large_count_input.trim().parse::<usize>() {
                Ok(num) if num <= 6 => break num,
                _ => println!("Please enter a number between 0 and 6."),
            }
        };

        let (target_number, game_numbers) = match generate_game(large_count, &large_numbers, &small_numbers) {
            Ok(game) => game,
            Err(e) => {
                println!("{}", e);
                continue;
            }
        };

        println!("Target Number: {}", target_number);
        println!("Game Numbers: {:?}", game_numbers);
        println!("Enter your solution in RPN (Reverse Polish Notation):");
        let result = loop {
            let mut user_solution = String::new();
            io::stdin()
                .read_line(&mut user_solution)
                .expect("Failed to read line");

            match evaluate_rpn_expression(user_solution.trim(), &game_numbers) {
                Ok(result) => break result,
                Err(e) => println!("Error evaluating expression: {}", e),
            }
        };

        println!("Your result: {}", result);
        let points = calculate_points(target_number, result);
        println!("You scored {} points!", points);

        println!("Play again? (y/n)");
        let mut play_again_input = String::new();
        io::stdin()
            .read_line(&mut play_again_input)
            .expect("Failed to read line");

        if play_again_input.trim().eq_ignore_ascii_case("n") {
            break;
        }
    }
}

fn generate_game(large_count: usize, large_numbers: &[i32], small_numbers: &[i32]) -> Result<(i32, Vec<i32>), String> {
    if large_count > 6 {
        return Err("Invalid input: Enter a number between 0 and 6.".to_string());
    }

    let mut rng = rand::thread_rng();
    let mut game_numbers = Vec::new();

    for &num in large_numbers.choose_multiple(&mut rng, large_count) {
        game_numbers.push(num);
    }

    for &num in small_numbers.choose_multiple(&mut rng, 6 - large_count) {
        game_numbers.push(num);
    }

    let target_number = rng.gen_range(100..=999);

    Ok((target_number, game_numbers))
}

fn evaluate_rpn_expression(expression: &str, game_numbers: &[i32]) -> Result<i32, String> {
    let mut stack: Vec<i32> = Vec::new();
    let mut num_usage = std::collections::HashMap::new();

    for token in expression.split_whitespace() {
        match token {
            "+" | "-" | "*" | "/" => {
                if stack.len() < 2 {
                    return Err("Invalid RPN expression: not enough operands.".to_string());
                }

                let b = stack.pop().unwrap();
                let a = stack.pop().unwrap();

                if token == "/" && b == 0 {
                    return Err("Invalid expression: Division by zero.".to_string());
                }

                let result = match token {
                    "+" => a + b,
                    "-" => a - b,
                    "*" => a * b,
                    "/" => a / b, // Assuming integer division
                    _ => unreachable!(),
                };

                stack.push(result);
            }
            _ => {
                let number = match token.parse::<i32>() {
                    Ok(num) => num,
                    Err(_) => return Err(format!("Invalid token in expression: '{}' is not a number.", token)),
                };

                if !game_numbers.contains(&number) {
                    return Err(format!("Invalid expression: Number {} is not in this round.", number));
                }

                let count = num_usage.entry(number).or_insert(0);
                *count += 1;

                if *count > game_numbers.iter().filter(|&&x| x == number).count() {
                    return Err(format!("Invalid expression: Number {} used more times than it appears in the game.", number));
                }

                stack.push(number);
            }
        }
    }

    if stack.len() != 1 {
        return Err("Invalid RPN expression: Incorrect number of elements remaining.".to_string());
    }

    Ok(stack[0])
}

fn calculate_points(target: i32, result: i32) -> i32 {
    let difference = (target - result).abs();
    match difference {
        0 => 10,
        1..=5 => 7,
        6..=10 => 5,
        _ => 0,
    }
}
