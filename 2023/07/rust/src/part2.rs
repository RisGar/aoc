use std::{cmp::Ordering, collections::BTreeMap, fs};

#[derive(Debug, Clone)]
enum CardRating {
  Jack,
  Two,
  Three,
  Four,
  Five,
  Six,
  Seven,
  Eight,
  Nine,
  Ten,
  Queen,
  King,
  Ace,
}

fn get_card_rating(card: char) -> CardRating {
  let mut map: BTreeMap<char, CardRating> = BTreeMap::new();
  map.insert('J', CardRating::Jack);
  map.insert('2', CardRating::Two);
  map.insert('3', CardRating::Three);
  map.insert('4', CardRating::Four);
  map.insert('5', CardRating::Five);
  map.insert('6', CardRating::Six);
  map.insert('7', CardRating::Seven);
  map.insert('8', CardRating::Eight);
  map.insert('9', CardRating::Nine);
  map.insert('T', CardRating::Ten);
  map.insert('Q', CardRating::Queen);
  map.insert('K', CardRating::King);
  map.insert('A', CardRating::Ace);

  map.get(&card).unwrap().clone()
}

#[derive(Debug)]
struct Hand {
  cards: Vec<char>,
  hand_type: HandType,
  bid: u32,
}

impl Ord for Hand {
  fn cmp(&self, other: &Self) -> Ordering {
    if self.hand_type > other.hand_type {
      return Ordering::Greater;
    }

    if self.hand_type < other.hand_type {
      return Ordering::Less;
    }

    let tuple = self.cards.iter().zip(other.cards.iter());

    for (a, b) in tuple {
      if a == b {
        continue;
      }

      if get_card_rating(*a) as u32 > get_card_rating(*b) as u32 {
        return Ordering::Greater;
      } else {
        return Ordering::Less;
      }
    }

    Ordering::Equal
  }
}

impl PartialOrd for Hand {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    Some(self.cmp(other))
  }
}

impl PartialEq for Hand {
  fn eq(&self, other: &Self) -> bool {
    self.hand_type.eq(&other.hand_type)
  }
}

impl Eq for Hand {}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
enum HandType {
  None,
  HighCard,
  OnePair,
  TwoPair,
  ThreeOfAKind,
  FullHouse,
  FourOfAKind,
  FiveOfAKind,
}

fn parse_input() -> Vec<Hand> {
  fs::read_to_string("../input/input.txt")
    .unwrap()
    .lines()
    .map(|line| {
      let mut split = line.split_whitespace();

      let cards = split.next().unwrap().chars().collect();
      let bid = split.next().unwrap().parse().unwrap();

      Hand {
        cards,
        hand_type: HandType::None,
        bid,
      }
    })
    .collect()
}

fn calculate_type(hand: &mut Hand) -> &mut Hand {
  let mut map: Vec<(char, u32)> = hand
    .cards
    .iter()
    .filter(|x| **x != 'J')
    .fold(BTreeMap::new(), |mut acc, x| {
      *acc.entry(*x).or_default() += 1;
      acc
    })
    .into_iter()
    .collect();

  map.sort_by(|a, b| b.1.cmp(&a.1));

  let mut j_amount = hand.cards.iter().filter(|x| **x == 'J').count() as u32;

  if j_amount == 5 {
    hand.hand_type = HandType::FiveOfAKind;
    return hand;
  }

  let mut triple_found = false;
  let mut doubles_found = 0;

  for card in map.into_iter() {
    if card.1 == 5 || card.1 + j_amount == 5 {
      hand.hand_type = HandType::FiveOfAKind;
      break;
    } else if card.1 == 4 || card.1 + j_amount == 4 {
      hand.hand_type = HandType::FourOfAKind;
      break;
    } else if card.1 == 3 || card.1 + j_amount == 3 {
      j_amount -= 3 - card.1;
      triple_found = true;
    } else if card.1 == 2 || card.1 + j_amount == 2 {
      j_amount -= 2 - card.1;
      doubles_found += 1;
    }
  }

  if hand.hand_type == HandType::None {
    if triple_found {
      if doubles_found == 1 {
        hand.hand_type = HandType::FullHouse;
      } else {
        hand.hand_type = HandType::ThreeOfAKind;
      }
    } else if doubles_found == 2 {
      hand.hand_type = HandType::TwoPair;
    } else if doubles_found == 1 {
      hand.hand_type = HandType::OnePair;
    } else {
      hand.hand_type = HandType::HighCard;
    }
  }

  hand
}

pub fn part2() {
  let mut binding = parse_input();
  let mut ranks: Vec<&mut Hand> = binding.iter_mut().map(calculate_type).collect();
  ranks.sort();

  let output: u32 = ranks
    .iter()
    .enumerate()
    .map(|(i, e)| e.bid * (i as u32 + 1))
    .sum();

  println!("{:#?}", output);
}
