use std::fs;

type LensMap = Vec<Vec<Lens>>;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct Lens {
  label: String,
  focal_length: u8,
}

fn parse_input() -> Vec<Vec<char>> {
  let mut input = fs::read_to_string("input/input.txt").unwrap();
  input.pop();
  input
    .split(',')
    .map(|x| x.chars().collect::<Vec<_>>())
    .collect::<Vec<_>>()
}

fn hash(input: &[char]) -> u8 {
  let mut current: u32 = 0;

  for char in input {
    current += *char as u32;
    current *= 17;
    current %= 256;
  }

  current as u8
}

fn modify_boxes(input: &[char], map: &mut LensMap) {
  let label: Vec<char> = input
    .iter()
    .take_while(|char| char.is_ascii_alphabetic())
    .copied()
    .collect();
  let i = hash(&label);
  let label: String = label.iter().collect();
  let lens_box = &mut map[i as usize];

  match input.contains(&'-') {
    true => {
      if let Some(e) = lens_box.iter().position(|lens| lens.label == label) {
        lens_box.remove(e);
      }
    }
    false => {
      let focal_length = input[label.len() + 1].to_digit(10).unwrap() as u8;

      if let Some(e) = lens_box.iter().position(|lens| lens.label == label) {
        lens_box[e].focal_length = focal_length;
      } else {
        lens_box.push(Lens {
          label,
          focal_length,
        });
      }
    }
  }
}

fn total_power(map: &LensMap) -> usize {
  map
    .iter()
    .enumerate()
    .flat_map(|(box_i, lenses)| {
      lenses
        .iter()
        .enumerate()
        .map(move |(lens_i, lens)| (box_i + 1) * (lens_i + 1) * lens.focal_length as usize)
    })
    .sum()
}

fn main() {
  let input = parse_input();

  let part1: u32 = input.iter().map(|x| hash(x) as u32).sum();
  println!("part 1: {}", part1);

  let mut map: LensMap = vec![vec![]; u8::MAX as usize + 1];
  input.iter().for_each(|step| modify_boxes(step, &mut map));

  let part2 = total_power(&map);
  println!("part 2: {}", part2);
}
