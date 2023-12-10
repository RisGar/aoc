use std::{collections::BTreeMap, fs};

// START From Rosetta Code
use std::cmp::{max, min};

fn gcd(a: usize, b: usize) -> usize {
  match ((a, b), (a & 1, b & 1)) {
    ((x, y), _) if x == y => y,
    ((0, x), _) | ((x, 0), _) => x,
    ((x, y), (0, 1)) | ((y, x), (1, 0)) => gcd(x >> 1, y),
    ((x, y), (0, 0)) => gcd(x >> 1, y >> 1) << 1,
    ((x, y), (1, 1)) => {
      let (x, y) = (min(x, y), max(x, y));
      gcd((y - x) >> 1, x)
    }
    _ => unreachable!(),
  }
}

fn lcm(a: usize, b: usize) -> usize {
  a * b / gcd(a, b)
}
// END From Rosetta Code

fn lcm_vec(a: Vec<usize>) -> usize {
  a.into_iter().reduce(lcm).unwrap()
}

fn main() {
  part1();
  part2();
}

#[derive(Debug, Clone)]
struct Children {
  left: String,
  right: String,
}

type Map = BTreeMap<String, Children>;

fn parse_input() -> (String, Map) {
  let binding = fs::read_to_string("input/input.txt").unwrap();
  let lines: Vec<&str> = binding.lines().collect();

  let instructions = lines[0].to_string();

  let elements: Map = lines
    .iter()
    .skip(2)
    .map(|l| {
      let mut split = l.split(" = ");

      let value: String = split.next().unwrap().to_string();
      let children: Vec<String> = split
        .next()
        .unwrap()
        .replace(['(', ')'], "")
        .split(", ")
        .map(String::from)
        .collect();

      (
        value,
        Children {
          left: children[0].clone(),
          right: children[1].clone(),
        },
      )
    })
    .collect();

  (instructions, elements)
}

fn part1() {
  let (instructions, elements) = parse_input();

  let mut steps: usize = 0;
  let mut current = elements.first_key_value().unwrap();

  loop {
    if current.0 == "ZZZ" {
      break;
    }

    let instruction = instructions.as_bytes()[steps % instructions.len()] as char;

    match instruction {
      'L' => {
        current = elements.get_key_value(&current.1.left).unwrap();
      }
      'R' => {
        current = elements.get_key_value(&current.1.right).unwrap();
      }
      _ => unreachable!(),
    }
    steps += 1;
  }

  println!("{}", steps);
}

fn steps(instructions: String, elements: Map, current: (&String, &Children)) -> usize {
  let mut steps: usize = 0;
  let mut current = current;

  loop {
    if current.0.ends_with('Z') {
      break;
    }

    let instruction = instructions.as_bytes()[steps % instructions.len()] as char;

    match instruction {
      'L' => {
        current = elements.get_key_value(&current.1.left).unwrap();
      }
      'R' => {
        current = elements.get_key_value(&current.1.right).unwrap();
      }
      _ => unreachable!(),
    }
    steps += 1;
  }

  steps
}

fn part2() {
  let (instructions, elements) = parse_input();

  let currents: Vec<(&String, &Children)> =
    elements.iter().filter(|x| x.0.ends_with('A')).collect();

  let steps: Vec<usize> = currents
    .iter()
    .map(|x| steps(instructions.clone(), elements.clone(), *x))
    .collect();

  let steps = lcm_vec(steps);

  println!("{}", steps);
}
