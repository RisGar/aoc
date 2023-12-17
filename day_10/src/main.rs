use num::Complex;
use std::{collections::HashMap, fs, ops::Add};

type PipeMap = HashMap<Complex<i32>, Vec<Complex<i32>>>;
type PipeMapEntry = (Complex<i32>, Vec<Complex<i32>>);

const NORTH: Complex<i32> = Complex::new(0, -1);
const SOUTH: Complex<i32> = Complex::new(0, 1);
const EAST: Complex<i32> = Complex::new(1, 0);
const WEST: Complex<i32> = Complex::new(-1, 0);

fn get_dirs(char: char) -> Vec<Complex<i32>> {
  match char {
    '|' => vec![NORTH, SOUTH],
    '-' => vec![EAST, WEST],
    'L' => vec![NORTH, EAST],
    'J' => vec![NORTH, WEST],
    '7' => vec![SOUTH, WEST],
    'F' => vec![SOUTH, EAST],
    'S' => vec![],
    _ => unreachable!("Chars not filtered correctly"),
  }
}

fn parse_input() -> PipeMap {
  fs::read_to_string("input/input.txt")
    .unwrap()
    .lines()
    .enumerate()
    .flat_map(|(y, line)| {
      line
        .chars()
        .enumerate()
        .filter(|x| x.1 != '.')
        .map(move |(x, c)| (Complex::new(x as i32, y as i32), get_dirs(c)))
        .collect::<PipeMap>()
    })
    .collect()
}

fn find_start(pipes: &PipeMap) -> PipeMapEntry {
  let directions: Vec<Complex<i32>> = vec![NORTH, SOUTH, EAST, WEST];

  let position = *pipes.iter().find(|x| x.1.is_empty()).unwrap().0;

  let next_dirs: Vec<Complex<i32>> = directions
    .into_iter()
    .filter(|direction| {
      let next = position.add(direction);
      match pipes.get_key_value(&next) {
        Some(pipe) => pipe.1.contains(&direction.scale(-1)),
        None => false,
      }
    })
    .collect();

  (position, next_dirs)
}

fn follow_pipe(starting: PipeMapEntry, pipes: &PipeMap) -> Vec<PipeMapEntry> {
  let mut nodes: Vec<(&Complex<i32>, &Vec<Complex<i32>>)> = vec![(&starting.0, &starting.1)];
  let mut direction: Complex<i32> = *starting.1.first().unwrap();

  loop {
    let next = nodes.last().unwrap().0.add(direction);

    if next == starting.0 {
      break;
    }

    let next_pipe = pipes.get_key_value(&next).unwrap();
    nodes.push(next_pipe);
    direction = *next_pipe
      .1
      .iter()
      .find(|x| **x != direction.scale(-1))
      .unwrap();
  }

  nodes.iter().map(|x| (*x.0, x.1.clone())).collect()
}

fn shoelace(pipes: &[PipeMapEntry]) -> usize {
  let corner_dirs: Vec<Vec<Complex<i32>>> = vec![
    vec![NORTH, EAST],
    vec![NORTH, WEST],
    vec![SOUTH, WEST],
    vec![SOUTH, EAST],
  ];

  let corners: Vec<Complex<i32>> = pipes
    .iter()
    .filter(|x| corner_dirs.contains(&x.1))
    .map(|x| x.0)
    .collect();

  let mut sum: i32 = 0;
  for i in 0..corners.len() {
    if i < corners.len() - 1 {
      sum += (corners[i].im + corners[i + 1].im) * (corners[i].re - corners[i + 1].re)
    } else {
      sum += (corners[i].im + corners[0].im) * (corners[i].re - corners[0].re)
    }
  }

  (sum.abs() / 2) as usize
}

fn main() {
  let pipes = parse_input();
  let starting = find_start(&pipes);
  let loop_pipes = follow_pipe(starting, &pipes);
  let part1 = loop_pipes.len() / 2;
  println!("part 1: {}", part1);

  let area = shoelace(&loop_pipes);
  let part2 = area - part1 + 1;
  println!("part 2: {}", part2);
}
