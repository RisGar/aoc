use std::fs;

fn main() {
  part1();
  part2();
}

fn parse_input() -> Vec<Vec<i32>> {
  fs::read_to_string("input/input.txt")
    .unwrap()
    .lines()
    .map(|x| {
      x.split_ascii_whitespace()
        .map(|x| x.parse().unwrap())
        .collect()
    })
    .collect()
}

fn find_differences(line: Vec<i32>) -> Vec<Vec<i32>> {
  let mut sequences: Vec<Vec<i32>> = vec![line];
  loop {
    let mut differences: Vec<i32> = vec![];

    let last = sequences.last().unwrap();
    for i in 0..last.len() - 1 {
      differences.push(last[i + 1] - last[i]);
    }

    if differences.iter().all(|x| *x == 0) {
      sequences.push(differences);
      break;
    } else {
      sequences.push(differences);
    }
  }

  sequences
}

fn predict_next_value(line: Vec<i32>) -> i32 {
  let sequences = find_differences(line);

  let mut last_number: i32 = 0;
  for sequence in sequences.clone().into_iter().rev().skip(1) {
    last_number += sequence.last().unwrap();
  }

  last_number
}

fn predict_last_value(line: Vec<i32>) -> i32 {
  let sequences = find_differences(line);

  let mut first_number: i32 = 0;
  for sequence in sequences.clone().into_iter().rev().skip(1) {
    first_number = sequence.first().unwrap() - first_number;
  }

  first_number
}

fn part1() {
  let lines = parse_input();
  let value: i32 = lines.into_iter().map(predict_next_value).sum();

  println!("{:#?}", value);
}
fn part2() {
  let lines = parse_input();
  let value: i32 = lines.into_iter().map(predict_last_value).sum();

  println!("{:#?}", value);
}
