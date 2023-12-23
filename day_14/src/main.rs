use itertools::Itertools;
use std::{collections::HashMap, fs, hash::Hash};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum RockType {
  Cube,
  Round,
}

impl RockType {
  fn get(char: char) -> Self {
    match char {
      '#' => RockType::Cube,
      'O' => RockType::Round,
      _ => unreachable!("Invalid rock type"),
    }
  }
}

type RockMap = Vec<Option<RockType>>;

#[inline]
fn coords_to_index(x: usize, y: usize, dims: usize) -> usize {
  y * dims + x
}

fn parse_input() -> (RockMap, usize) {
  let binding = fs::read_to_string("input/input.txt").unwrap();
  let input = binding.lines();

  let rows = input.clone().collect::<Vec<_>>().len();

  let mut map: RockMap = vec![None; rows * rows];

  input.enumerate().for_each(|(y, line)| {
    line
      .chars()
      .enumerate()
      .filter(|x| x.1 == '#' || x.1 == 'O')
      .for_each(|(x, char)| {
        map[coords_to_index(x, y, rows)] = Some(RockType::get(char));
      })
  });

  (map, rows)
}

fn tilt_cycle(rocks: RockMap, times: usize, dims: usize) -> usize {
  let mut memory: HashMap<RockMap, usize> = HashMap::new();
  let mut current = rocks;

  for n in 1..times {
    for _ in 0..4 {
      current = tilt(&current, dims);
      current = rotate(&current, dims);
    }

    if let Some(seen) = memory.insert(current.clone(), n) {
      if (times - n) % (n - seen) == 0 {
        break;
      }
    }
  }

  calculate_load(&current, dims)
}

fn rotate(rocks: &RockMap, dims: usize) -> RockMap {
  let mut rocks_new: RockMap = vec![None; rocks.len()];
  for (y, x) in (0..dims).cartesian_product(0..dims) {
    rocks_new[coords_to_index(dims - 1 - y, x, dims)] = rocks[coords_to_index(x, y, dims)];
  }

  rocks_new
}

fn tilt(rocks: &RockMap, dims: usize) -> RockMap {
  let mut done = false;
  let mut rocks = rocks.clone();

  while !done {
    done = true;

    for i in 0..rocks.len() {
      if i >= dims && rocks[i] == Some(RockType::Round) && rocks[i - dims].is_none() {
        rocks[i] = None;
        rocks[i - dims] = Some(RockType::Round);
        done = false;
      }
    }
  }
  rocks
}

fn calculate_load(rocks: &RockMap, dims: usize) -> usize {
  rocks
    .iter()
    .enumerate()
    .filter(|x| x.1 == &Some(RockType::Round))
    .map(|x| dims - (x.0 / dims))
    .sum()
}

fn main() {
  let (rocks, dims) = parse_input();
  println!("cols: {}, rows: {}", dims, dims);

  let part1 = calculate_load(&tilt(&rocks, dims), dims);
  println!("part 1: {}", part1);

  let part2 = tilt_cycle(rocks, 1000000000, dims);
  println!("part 2: {}", part2);
}
