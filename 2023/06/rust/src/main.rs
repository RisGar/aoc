use std::fs;

fn main() {
  part1();
  part2();
}

fn get_input() -> Vec<String> {
  fs::read_to_string("../input/input.txt")
    .unwrap()
    .lines()
    .map(|x| x.to_string())
    .collect()
}

fn get_input_1() -> Vec<(u64, u64)> {
  let input: Vec<Vec<u64>> = get_input()
    .into_iter()
    .map(|x| {
      x.split(':').collect::<Vec<&str>>()[1]
        .split_ascii_whitespace()
        .map(|x| x.parse().unwrap())
        .collect()
    })
    .collect();

  input[0].clone().into_iter().zip(input[1].clone()).collect()
}

fn get_input_2() -> (u64, u64) {
  let input: Vec<u64> = get_input()
    .into_iter()
    .map(|x| {
      x.split(':').collect::<Vec<&str>>()[1]
        .replace(' ', "")
        .parse()
        .unwrap()
    })
    .collect();

  (input[0], input[1])
}

fn calculate_time(total_time: u64, button_time: u64) -> u64 {
  (total_time - button_time) * button_time
}

fn calculate_ways((time, target_distance): (u64, u64)) -> u64 {
  (1..time)
    .filter(|&t| calculate_time(time, t) > target_distance)
    .count()
    .try_into()
    .unwrap()
}

fn part1() {
  let inputs = get_input_1();
  let output: u64 = inputs.into_iter().map(calculate_ways).product();

  println!("{:#?}", output);
}

fn part2() {
  let input = get_input_2();
  let output = calculate_ways(input);

  println!("{:#?}", output);
}
