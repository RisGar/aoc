use num::Complex;
use std::{collections::BTreeMap, fs};

fn parse_input() -> Vec<Complex<i64>> {
  fs::read_to_string("input/input.txt")
    .unwrap()
    .lines()
    .enumerate()
    .flat_map(|(y, line)| {
      line
        .chars()
        .enumerate()
        .filter(|x| x.1 == '#')
        .map(move |(x, _)| Complex::new(x as i64, y as i64))
        .collect::<Vec<_>>()
    })
    .collect()
}

struct Expansion {
  original: Complex<i64>,
  expanded: Complex<i64>,
}

fn expand_rows(galaxies: &[Complex<i64>], expansion_factor: i64) -> Vec<Complex<i64>> {
  let mut galaxies: Vec<Expansion> = galaxies
    .iter()
    .map(|x| Expansion {
      original: *x,
      expanded: *x,
    })
    .collect();

  // Real
  let real_max = galaxies.iter().map(|x| x.original.re).max().unwrap();
  for i in 0..real_max {
    if !galaxies.iter().any(|x| x.original.re == i) {
      galaxies
        .iter_mut()
        .filter(|x| x.original.re > i)
        .for_each(|x| x.expanded += Complex::new(expansion_factor - 1, 0));
    }
  }

  // Imaginary
  let imaginary_max = galaxies.iter().map(|x| x.original.im).max().unwrap();
  for i in 0..imaginary_max {
    if !galaxies.iter().any(|x| x.original.im == i) {
      galaxies
        .iter_mut()
        .filter(|x| x.original.im > i)
        .for_each(|x| x.expanded += Complex::new(0, expansion_factor - 1));
    }
  }

  galaxies.into_iter().map(|x| x.expanded).collect()
}

fn manhattan_distance(galaxy_1: &Complex<i64>, galaxy_2: &Complex<i64>) -> i64 {
  (galaxy_1.im - galaxy_2.im).abs() + (galaxy_1.re - galaxy_2.re).abs()
}

fn calculate_distances(galaxies: Vec<Complex<i64>>) -> i64 {
  // ((id_1, id_2), distance)
  let mut distance_map: BTreeMap<(usize, usize), i64> = BTreeMap::new();

  for galaxy_1 in galaxies.iter().enumerate() {
    for galaxy_2 in galaxies.iter().enumerate() {
      if galaxy_1.0 != galaxy_2.0
        && !distance_map.contains_key(&(galaxy_1.0, galaxy_2.0))
        && !distance_map.contains_key(&(galaxy_2.0, galaxy_1.0))
      {
        distance_map.insert(
          (galaxy_1.0, galaxy_2.0),
          manhattan_distance(galaxy_1.1, galaxy_2.1),
        );
      }
    }
  }

  distance_map.iter().map(|x| x.1).sum()
}

fn main() {
  let galaxies = parse_input();

  let part1 = expand_rows(&galaxies, 2);
  let part1 = calculate_distances(part1);
  println!("Part 1: {}", part1);

  let part2 = expand_rows(&galaxies, 1000000);
  let part2 = calculate_distances(part2);
  println!("Part 2: {}", part2);
}
