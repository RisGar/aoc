use std::{fs, ops::Range};

fn main() {
  part1();
  part2();
}

struct Infinite {
  current: i64,
}

impl Infinite {
  fn new() -> Infinite {
    Infinite { current: -1 }
  }
}

impl Iterator for Infinite {
  type Item = i64;

  fn next(&mut self) -> Option<Self::Item> {
    self.current += 1;
    Some(self.current)
  }
}

#[derive(Debug)]
struct Map {
  destination: Range<i64>,
  source: Range<i64>,
}

fn get_input() -> Vec<String> {
  fs::read_to_string("../input/input.txt")
    .unwrap()
    .split("\n\n")
    .map(String::from)
    .collect()
}

fn part1() {
  let input = get_input();

  let mut seeds: Vec<i64> = input[0]
    .strip_prefix("seeds: ")
    .unwrap()
    .split(' ')
    .map(|x| x.parse().unwrap())
    .collect();

  for e in input.iter().skip(1) {
    let maps: Vec<Map> = e
      .lines()
      .skip(1)
      .map(|x| {
        let x: Vec<i64> = x.split(' ').map(|x| x.parse().unwrap()).collect();

        Map {
          destination: Range {
            start: x[0],
            end: x[0] + x[2],
          },
          source: Range {
            start: x[1],
            end: x[1] + x[2],
          },
        }
      })
      .collect();

    let mut seeds_new: Vec<i64> = vec![];

    seeds.retain(|seed| {
      for map in &maps {
        if *seed >= map.source.start && *seed <= map.source.end {
          seeds_new.push(map.destination.start + seed - map.source.start);
          return false;
        }
      }
      true
    });

    seeds.append(&mut seeds_new);
  }

  let seeds = seeds.iter().min().unwrap();

  println!("{:#?}", seeds);
}

fn part2() {
  let input = get_input();

  let seeds: Vec<Range<i64>> = input[0]
    .strip_prefix("seeds: ")
    .unwrap()
    .split(' ')
    .map(|num| num.parse().unwrap())
    .collect::<Vec<i64>>()
    .chunks(2)
    .map(|range| Range {
      start: range[0],
      end: range[0] + range[1],
    })
    .collect();

  let all_maps: Vec<Vec<Map>> = input
    .iter()
    .skip(1)
    .rev()
    .map(|lines| {
      lines
        .lines()
        .skip(1)
        .map(|line| {
          let nums: Vec<i64> = line.split(' ').map(|num| num.parse().unwrap()).collect();

          Map {
            destination: Range {
              start: nums[0],
              end: nums[0] + nums[2],
            },
            source: Range {
              start: nums[1],
              end: nums[1] + nums[2],
            },
          }
        })
        .collect()
    })
    .collect();

  'outer: for n in Infinite::new() {
    let mut val = n;

    all_maps.iter().for_each(|maps| {
      for map in maps {
        if map.destination.start <= val && val < map.destination.end {
          val = map.source.start + (val - map.destination.start);
          break;
        }
      }
    });

    for seed in &seeds {
      if seed.start <= val && val < seed.end {
        println!("{:#?}", n);
        break 'outer;
      }
    }
  }
}
