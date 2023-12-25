use std::fs;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CharType {
  Ash,
  Rock,
}

impl CharType {
  fn get(char: char) -> Self {
    match char {
      '#' => Self::Rock,
      '.' => Self::Ash,
      _ => unreachable!("invalid char: {}", char),
    }
  }
}

#[derive(Debug, Clone, Copy)]
struct Char {
  val: CharType,
  x: usize,
  y: usize,
}

fn parse_input() -> Vec<Pattern> {
  let mut input = fs::read_to_string("input/input.txt").unwrap();
  input.pop();

  input
    .split("\n\n")
    .map(|pattern| {
      let chars: Vec<Char> = pattern
        .lines()
        .enumerate()
        .flat_map(|(y, line)| {
          line
            .chars()
            .enumerate()
            .map(move |(x, char)| Char {
              val: CharType::get(char),
              x,
              y,
            })
            .collect::<Vec<Char>>()
        })
        .collect();

      let width = chars.iter().map(|c| c.x).max().unwrap() + 1;
      let height = chars.iter().map(|c| c.y).max().unwrap() + 1;

      let rows: Vec<Vec<CharType>> = (0..height)
        .map(|n| {
          chars
            .iter()
            .filter(|char| char.y == n)
            .map(|char| char.val)
            .collect()
        })
        .collect();

      let cols: Vec<Vec<CharType>> = (0..width)
        .map(|n| {
          chars
            .iter()
            .filter(|char| char.x == n)
            .map(|char| char.val)
            .collect()
        })
        .collect();

      Pattern {
        rows,
        cols,
        width,
        height,
      }
    })
    .collect()
}

#[derive(Debug, Clone)]
struct Pattern {
  rows: Vec<Vec<CharType>>,
  cols: Vec<Vec<CharType>>,
  width: usize,
  height: usize,
}

impl Pattern {
  fn get_axis(&self, expected_smudges: usize) -> usize {
    if let Some(vertical) = self.vertical_axis(expected_smudges) {
      vertical
    } else if let Some(horizontal) = self.horizontal_axis(expected_smudges) {
      horizontal * 100
    } else {
      0
    }
  }

  fn vertical_axis(&self, expected_smudges: usize) -> Option<usize> {
    (1..self.width).find(|possible| {
      let mut smudges_left = expected_smudges;

      self
        .rows
        .iter()
        .map(|row| {
          let first = row[..*possible].iter().rev();
          let second = row[*possible..].iter();

          first.zip(second).all(|a| {
            if a.0 == a.1 {
              true
            } else if smudges_left > 0 {
              smudges_left -= 1;
              true
            } else {
              false
            }
          })
        })
        .all(|n| n)
        && smudges_left == 0
    })
  }

  fn horizontal_axis(&self, expected_smudges: usize) -> Option<usize> {
    (1..self.height).find(|possible| {
      let mut smudges_left = expected_smudges;

      self
        .cols
        .iter()
        .map(|col| {
          let first = col[..*possible].iter().rev();
          let second = col[*possible..].iter();

          first.zip(second).all(|a| {
            if a.0 == a.1 {
              true
            } else if smudges_left > 0 {
              smudges_left -= 1;
              true
            } else {
              false
            }
          })
        })
        .all(|n| n)
        && smudges_left == 0
    })
  }
}

fn main() {
  let patterns = parse_input();

  let part1: usize = patterns.iter().map(|pattern| pattern.get_axis(0)).sum();
  println!("part 1: {}", part1);

  let part2: usize = patterns.iter().map(|pattern| pattern.get_axis(1)).sum();
  println!("part 2: {}", part2);
}
