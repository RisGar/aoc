use std::{fs, vec};

fn main() {
  part1();
  part2();
}

#[derive(Debug, Clone)]
struct Character {
  val: String,
  x: i32,
  y: i32,
}

fn part1() {
  let (numbers, symbols) = get_input();

  let output: i32 = numbers
    .into_iter()
    .fold(vec![], |mut acc, x| {
      if acc.is_empty() {
        acc.push(vec![x]);
        return acc;
      }

      let last = acc.last().unwrap().last().unwrap();
      if last.y == x.y && last.x == x.x - 1 {
        acc.last_mut().unwrap().push(x)
      } else {
        acc.push(vec![x])
      }

      acc
    })
    .into_iter()
    .filter(|a| a.iter().any(|x| symbols.iter().any(|e| adjacent(x, e))))
    .map(|x| {
      x.iter()
        .map(|y| y.val.clone())
        .collect::<Vec<String>>()
        .join("")
    })
    .map(|x| x.parse::<i32>().unwrap())
    .sum();

  println!("{:#?}", output);
}

fn part2() {
  // Convert to `Character` struct with coordinates and seperate numbers and symbols
  let (numbers, symbols) = get_input();

  // Group numbers that are next to each other
  let numbers = numbers.into_iter().fold(vec![], |mut acc, x| {
    if acc.is_empty() {
      acc.push(vec![x]);
      return acc;
    }

    let last = acc.last().unwrap().last().unwrap();
    if last.y == x.y && last.x == x.x - 1 {
      acc.last_mut().unwrap().push(x)
    } else {
      acc.push(vec![x])
    }

    acc
  });

  let output: i32 = symbols
    .iter()
    .filter(|x| x.val == "*")
    .map(|symbol| {
      numbers
        .clone()
        .into_iter()
        .filter(|nums| nums.iter().any(|num| adjacent(symbol, num)))
        .map(|x| {
          x.iter()
            .map(|y| y.val.clone())
            .collect::<Vec<String>>()
            .join("")
            .parse::<i32>()
            .unwrap()
        })
        .collect()
    })
    .filter(|x: &Vec<i32>| x.len() == 2)
    .map(|x| x.iter().product::<i32>())
    .sum();

  println!("{:#?}", output);
}

fn get_input() -> (Vec<Character>, Vec<Character>) {
  fs::read_to_string("input/input.txt")
    .unwrap()
    .lines()
    .enumerate()
    .flat_map(|(y, s)| {
      s.chars()
        .enumerate()
        .map(|(x, c)| Character {
          val: c.to_string(),
          x: x as i32,
          y: y as i32,
        })
        .collect::<Vec<Character>>()
    })
    .filter(|x| x.val != ".")
    .partition(|x| x.val.parse::<i32>().is_ok())
}

fn adjacent(a: &Character, b: &Character) -> bool {
  (b.x == a.x - 1 || b.x == a.x + 1 || b.y == a.y - 1 || b.y == a.y + 1)
    && (b.x == a.x - 1 || b.x == a.x + 1 || b.x == a.x)
    && (b.y == a.y || b.y == a.y - 1 || b.y == a.y + 1)
}
