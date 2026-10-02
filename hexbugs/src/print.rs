use crate::{Bug, Color, GameState, Hex, ROW_SIZE, START_HEX};
use std::fmt::{Display, Formatter, Result};

impl Display for GameState {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{}", self.fancy_fmt(&[]))
    }
}

const FREE_SPACE: char = '\u{ff0e}';
const INVERT_BG_COLOR: &str = "\x1b[7m";
const RESET_COLOR: &str = "\x1b[0m";

impl GameState {
    // return the Hex to the upper left and lower right of all occupied nodes.
    // Given wrapping, the second may be less than the first.
    fn bounding_box(&self) -> (Hex, Hex, Hex, Hex) {
        let empty_rows = (0..ROW_SIZE)
            .map(|r| (0..ROW_SIZE).all(|c| !self.occupied(r * ROW_SIZE + c)))
            .collect::<Vec<bool>>();
        let empty_cols = (0..ROW_SIZE)
            .map(|c| (0..ROW_SIZE).all(|r| !self.occupied(r * ROW_SIZE + c)))
            .collect::<Vec<bool>>();
        if empty_rows.iter().all(|&r| r) {
            // Center around start hex
            return (
                START_HEX / ROW_SIZE - 1,
                START_HEX / ROW_SIZE + 1,
                START_HEX % ROW_SIZE - 1,
                START_HEX % ROW_SIZE + 1,
            );
        }
        let mut minr = 0;
        let mut maxr = 0;
        let mut minc = 0;
        let mut maxc = 0;
        for i in 0..ROW_SIZE as usize {
            let j = (i + 1) % ROW_SIZE as usize;
            if empty_rows[i] && !empty_rows[j] {
                minr = i as Hex;
            }
            if !empty_rows[i] && empty_rows[j] {
                maxr = j as Hex;
            }
            if empty_cols[i] && !empty_cols[j] {
                minc = i as Hex;
            }
            if !empty_cols[i] && empty_cols[j] {
                maxc = j as Hex;
            }
        }
        (minr, maxr, minc, maxc)
    }

    pub fn fancy_fmt(&self, highlights: &[Hex]) -> String {
        let mut buf = String::new();
        let (startr, endr, startc, endc) = self.bounding_box();

        let mut r = startr;
        while r != (endr + 1) % ROW_SIZE {
            // Print prefix to get staggered hex rows
            let buflen = endr.wrapping_sub(r) % ROW_SIZE;
            if buflen % 2 == 1 {
                buf.push(' ');
            }
            for _ in 0..buflen / 2 {
                buf.push(FREE_SPACE);
            }

            let mut c = startc;
            while c != (endc + 1) % ROW_SIZE {
                let hex = c + r * ROW_SIZE;
                if let Some(index) = highlights.iter().position(|&x| x == hex) {
                    // Manual 2-byte space padding.
                    buf.push(if index > 9 { ((index / 10) + 48) as u8 as char } else { ' ' });
                    buf.push(((index % 10) + 48) as u8 as char);
                    c = (c + 1) % ROW_SIZE;
                    continue;
                }
                let node = self.node(hex);
                if node.occupied() {
                    if node.color() == Color::White {
                        buf.push_str(INVERT_BG_COLOR);
                    }
                    buf.push(node.bug().codepoint());
                    if node.color() == Color::White {
                        buf.push_str(RESET_COLOR);
                    }
                } else {
                    // Empty cell. Full width period.
                    buf.push(FREE_SPACE);
                }
                c = (c + 1) % ROW_SIZE;
            }

            // Stagger rows the other way to make the space look rectangular.
            for _ in 0..r.wrapping_sub(startr) % ROW_SIZE / 2 {
                buf.push(FREE_SPACE);
            }

            // On 2nd and 3rd rows, print remaining bugs from each side
            if r == (startr + 1) % ROW_SIZE {
                buf.push(' ');
                self.write_remaining(Color::White, &mut buf);
            } else if r == (startr + 2) % ROW_SIZE {
                self.write_remaining(Color::Black, &mut buf);
            }

            buf.push('\n');
            r = (r + 1) % ROW_SIZE;
        }
        buf
    }

    fn write_remaining(&self, color: Color, buf: &mut String) {
        for bug in Bug::iter_all() {
            let count = self.remaining[color as usize][bug as usize];
            let prefix = if count == 2 {
                '2'
            } else if count == 3 {
                '3'
            } else {
                ' '
            };
            if count > 0 {
                buf.push(prefix);
                if color == Color::White {
                    buf.push_str(INVERT_BG_COLOR);
                }
                buf.push(bug.codepoint());
                if color == Color::White {
                    buf.push_str(RESET_COLOR);
                }
                buf.push(' ');
            }
        }
    }

    pub fn println(&self) {
        self.println_highlights(&[]);
    }

    pub fn println_highlights(&self, highlights: &[Hex]) {
        print!("{}", self.fancy_fmt(highlights));
    }
}
