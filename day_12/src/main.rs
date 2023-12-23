use std::{collections::BTreeMap, fs};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
enum Spring {
  Operational,
  Damaged,
  Unknown,
}

impl Spring {
  fn parse(char: char) -> Self {
    match char {
      '.' => Spring::Operational,
      '#' => Spring::Damaged,
      '?' => Spring::Unknown,
      _ => unreachable!("Unexpected character: {char}"),
    }
  }
}

#[derive(Debug, Clone)]
struct SpringRow {
  springs: Vec<Spring>,
  groups: Vec<usize>,
}

fn get_input() -> Vec<SpringRow> {
  fs::read_to_string("input/input.txt")
    .unwrap()
    .lines()
    .map(|line| {
      let lines: Vec<&str> = line.split_ascii_whitespace().collect();
      let springs: Vec<Spring> = lines[0].chars().map(Spring::parse).collect();
      let groups = lines[1].split(',').map(|x| x.parse().unwrap()).collect();

      SpringRow {
        springs,
        groups,
        // unknown,
      }
    })
    .collect()
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct MemoIndex {
  spring_index: usize,
  group_index: usize,
  current_block: usize,
}

type Memoization = BTreeMap<MemoIndex, usize>;

fn find_arrangements(memo: &mut Memoization, index: MemoIndex, row: &SpringRow) -> usize {
  if let Some(key) = memo.get(&index) {
    return *key;
  }

  if index.spring_index == row.springs.len() {
    if (index.group_index == row.groups.len() && index.current_block == 0)
      || (index.group_index == row.groups.len() - 1
        && row.groups[index.group_index] == index.current_block)
    {
      return 1;
    } else {
      return 0;
    }
  }

  let mut valid_arrangements: usize = 0;

  for spring in [Spring::Operational, Spring::Damaged] {
    if row.springs[index.spring_index] == spring
      || row.springs[index.spring_index] == Spring::Unknown
    {
      if spring == Spring::Damaged {
        valid_arrangements += find_arrangements(
          memo,
          MemoIndex {
            spring_index: index.spring_index + 1,
            group_index: index.group_index,
            current_block: index.current_block + 1,
          },
          row,
        );
      } else if spring == Spring::Operational && index.current_block == 0 {
        valid_arrangements += find_arrangements(
          memo,
          MemoIndex {
            spring_index: index.spring_index + 1,
            group_index: index.group_index,
            current_block: index.current_block,
          },
          row,
        );
      } else if spring == Spring::Operational
        && index.current_block > 0
        && index.group_index < row.groups.len()
        && index.current_block == row.groups[index.group_index]
      {
        valid_arrangements += find_arrangements(
          memo,
          MemoIndex {
            spring_index: index.spring_index + 1,
            group_index: index.group_index + 1,
            current_block: 0,
          },
          row,
        );
      }
    }
  }

  memo.insert(index, valid_arrangements);

  valid_arrangements
}

impl SpringRow {
  fn expand_input(&self) -> Self {
    let groups = self.groups.repeat(5);
    let springs: Vec<Spring> = self
      .clone()
      .springs
      .into_iter()
      .chain([Spring::Unknown])
      .cycle()
      .take(self.springs.len() * 5 + 5 - 1)
      .collect();

    Self { springs, groups }
  }
}

fn main() {
  let input = get_input();
  let part_1: usize = input
    .iter()
    .map(|x| {
      let mut memo: Memoization = BTreeMap::new();
      find_arrangements(
        &mut memo,
        MemoIndex {
          spring_index: 0,
          group_index: 0,
          current_block: 0,
        },
        x,
      )
    })
    .sum();

  println!("part 1: {:#?}", part_1);

  let expanded_input: Vec<SpringRow> = input.iter().map(|x| x.expand_input()).collect();
  let part_2: usize = expanded_input
    .iter()
    .map(|x| {
      let mut memo: Memoization = BTreeMap::new();
      find_arrangements(
        &mut memo,
        MemoIndex {
          spring_index: 0,
          group_index: 0,
          current_block: 0,
        },
        x,
      )
    })
    .sum();

  println!("part 2: {:#?}", part_2);
}
