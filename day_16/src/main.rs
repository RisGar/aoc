use itertools::Itertools;
use num::Complex;
use std::{
  collections::{BTreeMap, HashMap},
  fs::read_to_string,
  sync::PoisonError,
};

#[derive(Debug, Clone, Copy)]
enum MirrorType {
  Vertical,
  Horizontal,
  DiagonalRight,
  DiagonalLeft,
  Empty,
}

impl MirrorType {
  fn parse(char: char) -> Self {
    match char {
      '|' => MirrorType::Vertical,
      '-' => MirrorType::Horizontal,
      '/' => MirrorType::DiagonalRight,
      '\\' => MirrorType::DiagonalLeft,
      '.' => MirrorType::Empty,
      _ => unreachable!("Invalid mirror type"),
    }
  }
}

type Position = Complex<i32>;
type MirrorMap = HashMap<Position, MirrorType>;

fn debug_map(map: &Vec<(Position, Position)>, dims: usize) {
  let mut vec = vec![vec!['.'; dims]; dims];

  for position in map {
    vec[position.0.im as usize][position.0.re as usize] = '#';
  }

  println!(
    "{:#?}",
    vec
      .iter()
      .map(|x| x.iter().collect::<String>())
      .collect::<Vec<String>>()
  );
}

fn parse_input() -> (MirrorMap, usize) {
  let binding = read_to_string("input/input.txt").unwrap();
  let input = binding.lines();

  let rows = input.clone().count();

  let map = input
    .enumerate()
    .flat_map(|(y, line)| {
      line
        .chars()
        .enumerate()
        .map(move |(x, char)| (Complex::new(x as i32, y as i32), MirrorType::parse(char)))
    })
    .collect();

  (map, rows)
}

const UP: Position = Complex::new(0, -1);
const DOWN: Position = Complex::new(0, 1);
const LEFT: Position = Complex::new(-1, 0);
const RIGHT: Position = Complex::new(1, 0);

fn energise_wall(
  map: &mut MirrorMap,
  energised_positions: &mut Vec<(Position, Position)>,
  position: Position,
  direction: Position,
) {
  // println!("{} {}", position, direction);
  // debug_map(energised_positions, (10, 10));

  if !energised_positions.contains(&(position, direction)) {
    {
      if let Some(position_type) = map.get(&position) {
        energised_positions.push((position, direction));

        match position_type {
          MirrorType::Vertical => {
            if direction == UP || direction == DOWN {
              energise_wall(map, energised_positions, position + direction, direction);
            } else {
              energise_wall(map, energised_positions, position + UP, UP);
              energise_wall(map, energised_positions, position + DOWN, DOWN);
            }
          }
          MirrorType::Horizontal => {
            if direction == LEFT || direction == RIGHT {
              energise_wall(map, energised_positions, position + direction, direction);
            } else {
              energise_wall(map, energised_positions, position + LEFT, LEFT);
              energise_wall(map, energised_positions, position + RIGHT, RIGHT);
            }
          }
          MirrorType::DiagonalRight => match direction {
            UP => {
              energise_wall(map, energised_positions, position + RIGHT, RIGHT);
            }
            DOWN => {
              energise_wall(map, energised_positions, position + LEFT, LEFT);
            }
            LEFT => {
              energise_wall(map, energised_positions, position + DOWN, DOWN);
            }
            RIGHT => {
              energise_wall(map, energised_positions, position + UP, UP);
            }
            _ => unreachable!("Invalid direction"),
          },
          MirrorType::DiagonalLeft => match direction {
            UP => {
              energise_wall(map, energised_positions, position + LEFT, LEFT);
            }
            DOWN => {
              energise_wall(map, energised_positions, position + RIGHT, RIGHT);
            }
            LEFT => {
              energise_wall(map, energised_positions, position + UP, UP);
            }
            RIGHT => {
              energise_wall(map, energised_positions, position + DOWN, DOWN);
            }
            _ => unreachable!("Invalid direction"),
          },
          MirrorType::Empty => {
            energise_wall(map, energised_positions, position + direction, direction);
          }
        }
      }
    }
  }
}

fn main() {
  let (mut input, dims) = parse_input();

  let mut energised = vec![];
  energise_wall(&mut input, &mut energised, Complex::new(0, 0), RIGHT);
  let count = energised.iter().map(|x| x.0).unique().count();
  println!("{}", count);

  let starting_positions = input
    .iter()
    .map(|x| x.0)
    .filter(|x| x.re == 0 || x.im == 0 || x.re == (dims - 1) as i32 || x.im == (dims - 1) as i32)
    .collect::<Vec<_>>();

  let mut counts = vec![];

  for starting_position in starting_positions {
    let mut directions = vec![];

    if starting_position.re == 0 {
      directions.push(RIGHT);
    }
    if starting_position.re == (dims - 1) as i32 {
      directions.push(LEFT);
    }

    if starting_position.im == 0 {
      directions.push(DOWN);
    }
    if starting_position.im == (dims - 1) as i32 {
      directions.push(UP);
    }

    for direction in directions {
      let mut input = input.clone();
      let mut energised = vec![];
      energise_wall(&mut input, &mut energised, *starting_position, direction);
      let count = energised.iter().map(|x| x.0).unique().count();
      counts.push(count);
    }
  }

  println!("{}", counts.iter().max().unwrap());
}