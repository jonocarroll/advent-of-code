use std::env;
use std::fs;
use std::time::Instant;

pub fn read_file(file: &str) -> String {
    let cwd = env::current_dir().unwrap();
    let filepath = cwd.join(file);
    let f = fs::read_to_string(filepath);
    f.expect("could not open input file")
}

fn part_one() -> i64 {
    let tmp = read_file("input09.txt");
    let input = tmp.strip_suffix('\n').unwrap_or(&tmp);

    let els: Vec<_> = input.chars().map(|ch| ch.to_digit(10).unwrap()).collect();
    let n = els.len();

    #[derive(Debug, PartialEq)]
    enum Element {
        F(i64), // files with ID
        B,      // blanks
    }

    let mut whats: Vec<Element> = Vec::new();
    let mut j: i64 = 0;

    for i in 0..n {
        let count = els[i];
        if i % 2 == 1 {
            whats.extend((0..count).map(|_| Element::B));
        } else {
            whats.extend((0..count).map(|_| Element::F(j)));
            j += 1;
        }
    }

    let n_b = whats.iter().filter(|x| **x == Element::B).count();

    while !whats.iter().rev().take(n_b).all(|x| *x == Element::B) {
        let last_f = whats
            .iter()
            .rposition(|x| matches!(x, Element::F(_)))
            .unwrap();
        let first_b = whats.iter().position(|x| *x == Element::B).unwrap();

        if let Element::F(id) = whats[last_f] {
            whats[last_f] = Element::B;
            whats[first_b] = Element::F(id);
        }
    }

    let mut tot: i64 = 0;
    for (index, element) in whats.iter().enumerate() {
        if let Element::F(id) = element {
            tot += (index as i64) * id;
        }
    }

    tot
}

fn part_two() -> i64 {
    let tmp = read_file("input09.txt");
    let input = tmp.strip_suffix('\n').unwrap_or(&tmp);

    let els: Vec<_> = input.chars().map(|ch| ch.to_digit(10).unwrap()).collect();
    let n = els.len();

    #[derive(Debug, PartialEq)]
    enum Element {
        F(i64, usize),     // files with ID, size
        B(usize),          // blanks with size
        Moved(i64, usize), // moved file with ID, size
    }

    let mut whats: Vec<Element> = Vec::new();
    let mut j: i64 = 0;

    for i in 0..n {
        let count = els[i];
        if i % 2 == 1 {
            whats.push(Element::B(count as usize));
        } else {
            whats.push(Element::F(j, count as usize));
            j += 1;
        }
    }

    loop {
        if let Some(last_f) = whats.iter().rposition(|x| matches!(x, Element::F(_, _))) {
            let last_f_size: usize = if let Element::F(_, filesize) = whats[last_f] {
                filesize
            } else {
                break;
            };

            let first_b: usize = match whats
                .iter()
                .position(|x| matches!(x, Element::B(blanksize) if *blanksize >= last_f_size))
            {
                Some(find_b) => {
                    if find_b > last_f {
                        if let Element::F(id, filesize) = whats[last_f] {
                            whats[last_f] = Element::Moved(id, filesize)
                        };
                        continue;
                    }
                    find_b
                }
                None => {
                    if let Element::F(id, filesize) = whats[last_f] {
                        whats[last_f] = Element::Moved(id, filesize)
                    };
                    continue;
                }
            };

            if let Element::F(id, filesize) = whats[last_f] {
                if let Element::B(blanksize) = whats[first_b] {
                    if blanksize > filesize {
                        whats[last_f] = Element::B(filesize);
                        whats[first_b] = Element::Moved(id, filesize);
                        whats.insert(first_b + 1, Element::B(blanksize - filesize))
                    } else {
                        whats[last_f] = Element::B(filesize);
                        whats[first_b] = Element::Moved(id, filesize);
                    }
                }
            }
        } else {
            break;
        }
    }

    let mut tot: i64 = 0;
    let mut index: i64 = 0;
    for element in whats.iter() {
        if let Element::Moved(id, size) = element {
            for _ in 1..=(*size as usize) {
                tot += index * id;
                index += 1;
            }
        }
        if let Element::B(size) = element {
            for _ in 1..=(*size as usize) {
                tot += 0;
                index += 1;
            }
        }
    }

    tot
}

fn main() {
    let timer = Instant::now();
    let p1 = part_one();
    let elapsed = timer.elapsed();
    println!("Part 1: {} ({:.2?})", p1, elapsed);

    let timer = Instant::now();
    let p2 = part_two();
    let elapsed = timer.elapsed();
    println!("Part 2: {} ({:.2?})", p2, elapsed);
}
