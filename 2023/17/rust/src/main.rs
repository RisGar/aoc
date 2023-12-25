use num::Complex;
use std::cmp::Ordering;
use std::collections::BinaryHeap;
use std::time::Instant;
use std::{collections::HashMap, fs::read_to_string};

fn parse_input() -> (HeatMap, usize) {
  let binding = read_to_string("input/input.txt").unwrap();
  let input = binding.lines();
  let dims = input.clone().count();
  let map = input
    .enumerate()
    .flat_map(|(y, line)| {
      line.chars().enumerate().map(move |(x, char)| {
        (
          Complex::new(x as i32, y as i32),
          char.to_digit(10).unwrap() as u8,
        )
      })
    })
    .collect();
  (map, dims)
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
struct State {
  cost: usize,
  position: Position,
  direction: Position,
}

impl Ord for State {
  fn cmp(&self, other: &Self) -> Ordering {
    other.cost.cmp(&self.cost)
  }
}
impl PartialOrd for State {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    Some(self.cmp(other))
  }
}

type Position = Complex<i32>;
type HeatMap = HashMap<Position, u8>;
type PositionDirection = (Position, Position);

const UP: Position = Complex::new(0, -1);
const DOWN: Position = Complex::new(0, 1);
const LEFT: Position = Complex::new(-1, 0);
const RIGHT: Position = Complex::new(1, 0);

fn dijkstra(pos_map: &HeatMap, min: i32, max: i32, dims: i32) -> usize {
  let mut distances: HashMap<PositionDirection, usize> = HashMap::new();

  let mut heap = BinaryHeap::new();
  heap.push(State {
    cost: 0,
    position: Complex::new(0, 0),
    direction: Complex::new(0, 0),
  });

  while let Some(State {
    cost,
    position,
    direction,
  }) = heap.pop()
  {
    if position == Complex::new(dims - 1, dims - 1) {
      return cost;
    }

    if distances.contains_key(&(position, direction)) && cost > distances[&(position, direction)] {
      continue;
    }

    for possible_direction in [UP, DOWN, LEFT, RIGHT] {
      if direction == possible_direction || direction == possible_direction.scale(-1) {
        continue;
      }

      let mut next_cost = cost;
      for distance in 1..=max {
        let new_position = position + possible_direction * distance;
        if !pos_map.contains_key(&new_position) {
          continue;
        }

        next_cost += pos_map[&new_position] as usize;

        if distance < min {
          continue;
        }

        let next = State {
          cost: next_cost,
          position: new_position,
          direction: possible_direction,
        };

        if &next.cost
          < distances
            .get(&(new_position, possible_direction))
            .unwrap_or(&usize::MAX)
        {
          distances.insert((next.position, next.direction), next.cost);
          heap.push(next)
        }
      }
    }
  }
  unreachable!("Goal not reachable")
}

fn main() {
  let start = Instant::now();
  let (input, dims) = parse_input();

  let part1 = dijkstra(&input, 1, 3, dims as i32);
  println!("Part 1: {}", part1);

  let part2 = dijkstra(&input, 4, 10, dims as i32);
  println!("Part 1: {}", part2);

  println!("Time: {}ms", start.elapsed().as_millis());
}
