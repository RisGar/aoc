use std::fs;

fn main() {
  part1();
  part2();
}

#[derive(Debug)]
struct Card {
  winning: Vec<i32>,
  own: Vec<i32>,
}

fn part1() {
  let output: u32 = fs::read_to_string("../input/input.txt")
    .unwrap()
    .lines()
    .map(|line| {
      let both: Vec<Vec<i32>> = line.split(": ").collect::<Vec<&str>>()[1]
        .split(" | ")
        .map(move |x| {
          x.split(' ')
            .filter(|x| !x.is_empty())
            .map(|x| x.parse::<i32>().unwrap())
            .collect()
        })
        .collect();

      let card = Card {
        winning: both[0].clone(),
        own: both[1].clone(),
      };

      match card
        .own
        .into_iter()
        .filter(|x| card.winning.contains(x))
        .count()
      {
        0 => 0,
        x => 1 << (x - 1),
      }
    })
    .sum();

  println!("{:#?}", output);
}

#[derive(Debug)]
struct Card2 {
  amount: usize,
  wins: usize,
}

fn part2() {
  let mut vec: Vec<Card2> = fs::read_to_string("../input/input.txt")
    .unwrap()
    .lines()
    .map(|line| {
      let both: Vec<Vec<i32>> = line.split(": ").collect::<Vec<&str>>()[1]
        .split(" | ")
        .map(move |x| {
          x.split(' ')
            .filter(|x| !x.is_empty())
            .map(|x| x.parse::<i32>().unwrap())
            .collect()
        })
        .collect();

      let card = Card {
        winning: both[0].clone(),
        own: both[1].clone(),
      };

      let wins = card
        .own
        .into_iter()
        .filter(|x| card.winning.contains(x))
        .count();

      Card2 { amount: 1, wins }
    })
    .collect();

  let mut output = 0;

  for i in 0..vec.len() {
    for _ in 0..vec[i].amount {
      output += 1;
      for m in 0..vec[i].wins {
        if i + m + 1 < vec.len() {
          vec[i + m + 1].amount += 1;
        }
      }
    }
  }

  println!("{:#?}", output);
}
