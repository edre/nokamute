use crate::board::*;
use crate::bug::Bug;
use crate::hex_grid::*;

use minimax::{Evaluation, Evaluator};

// An evaluator that knows nothing but the rules, and maximally explores the tree.
pub struct DumbEvaluator;

impl Evaluator for DumbEvaluator {
    type G = Rules;
    fn evaluate(&self, _: &Board) -> Evaluation {
        0
    }
}

// An evaluator that counts movable pieces and how close to death the queen is.
#[derive(Copy, Clone)]
pub struct BasicEvaluator {
    aggression: Evaluation,
    queen_liberty_factor: Evaluation,
    movable_queen_value: Evaluation,
    movable_bug_factor: Evaluation,
    unplayed_bug_factor: Evaluation,
    // Bonus for defensive pillbug or placeability thereof.
    pillbug_defense_bonus: Evaluation,
}

// Ideas:
//  - High level aggression setting
//    - Value mobility higher
//    - Opponent's mobility is more negative than you're mobility is positive.
//    - Don't value queen factor highly until a large mobility advantage is established.
//    - Quadratic score for filling queen liberties (counting virtual pillbug liberties)
//    - Conservative option: ignore queen and try to shut out opponent first.
//        Need to count placeable positions
//  - Directly encode "qualify for win" separate from queen liberties.
//    - Is the queen next to pillbug powers with an escape route?
//    - Is there a placeable position next to queen with a pillbug available?

impl BasicEvaluator {
    pub(crate) fn new(aggression: u8) -> Self {
        // Ensure aggression is a dial between 1 and 5.
        let aggression = aggression.clamp(1, 5) as Evaluation;
        Self {
            aggression,
            queen_liberty_factor: aggression * 10,
            movable_queen_value: aggression * 4,
            movable_bug_factor: 2,
            unplayed_bug_factor: 1,
            pillbug_defense_bonus: aggression * 40,
        }
    }

    pub(crate) fn aggression(&self) -> u8 {
        self.aggression as u8
    }

    fn value(&self, bug: Bug) -> Evaluation {
        // Mostly made up. All I know is that ants are good.
        match bug {
            Bug::Queen => self.movable_queen_value,
            Bug::Ant => 7,
            Bug::Beetle => 6,
            Bug::Grasshopper => 2,
            Bug::Spider => 2,
            Bug::Mosquito => 8, // See below.
            Bug::Ladybug => 6,
            Bug::Pillbug => 5,
        }
    }
}

impl Default for BasicEvaluator {
    fn default() -> Self {
        Self::new(3)
    }
}

fn count_liberties(board: &Board, origin: Hex, hex: Hex) -> Evaluation {
    adjacent(hex).into_iter().filter(|&adj| adj == origin || !board.occupied(adj)).count()
        as Evaluation
}

fn placeable(board: &Board, hex: Hex, color: Color) -> bool {
    !adjacent(hex).iter().any(|&adj| board.occupied(adj) && board.node(adj).color() != color)
}

#[test]
fn test_placeable() {
    let b = Board::from_game_string("Base;;;wA1;bA1 wA1-;wA2 /wA1").unwrap();
    assert!(!placeable(&b, Direction::SE.apply(START_HEX), Color::White));
    assert!(!placeable(&b, Direction::NE.apply(START_HEX), Color::White));
    assert!(placeable(&b, Direction::NW.apply(START_HEX), Color::White));
    assert!(!placeable(&b, Direction::SE.apply(START_HEX), Color::Black));
    assert!(!placeable(&b, Direction::NE.apply(START_HEX), Color::Black));
    assert!(!placeable(&b, Direction::NW.apply(START_HEX), Color::Black));
}

impl Evaluator for BasicEvaluator {
    type G = Rules;

    fn evaluate(&self, board: &Board) -> Evaluation {
        let mut buf = [0; 6];
        let mut immovable = board.find_cut_vertexes();

        let mut score = 0;
        let mut pillbug_defense = [false; 2];
        let mut queen_score = [0; 2];

        let remaining = board.get_remaining();
        let opp_remaining = board.get_opponent_remaining();
        for bug in Bug::iter_all() {
            score += (remaining[bug as usize] as Evaluation
                - opp_remaining[bug as usize] as Evaluation)
                * self.unplayed_bug_factor
                * self.value(bug);
        }

        for &hex in board.occupied_hexes[0].iter().chain(board.occupied_hexes[1].iter()) {
            let node = board.node(hex);
            let mut bug_score = self.value(node.bug());
            let mut pillbug_powers = node.bug() == Bug::Pillbug;
            let mut crawler = node.bug().crawler();
            if node.bug() == Bug::Mosquito {
                // Mosquitos are valued as they can currently move.
                bug_score = 0;
                crawler = true;
                if node.is_stacked() {
                    bug_score = self.value(Bug::Beetle);
                } else {
                    for adj in adjacent(hex) {
                        if board.occupied(adj) {
                            let bug = board.node(adj).bug();
                            if bug != Bug::Queen {
                                bug_score = self.value(bug);
                            }
                            if bug == Bug::Pillbug {
                                pillbug_powers = true;
                            }
                            if !bug.crawler() {
                                crawler = false;
                            }
                        }
                    }
                }
            };

            if crawler {
                // Treat blocked crawlers as immovable.
                if board.slidable_adjacent(&mut buf, hex, hex).next().is_none() {
                    immovable.set(hex);
                }
            }

            if node.is_stacked() {
                bug_score *= 2;
            }

            let friendly_queen = board.queens[node.color() as usize];

            // TODO: transpose this loop, i.e. categorize queen liberties after the bug loop.
            // Count libs for more if they are not crawlable (e.g. behind a gate)
            if adjacent(friendly_queen).contains(&hex) {
                // Filling friendly queen's liberty.
                if immovable.get(hex) && !node.is_stacked() {
                    queen_score[node.color() as usize] -= self.queen_liberty_factor;
                } else {
                    // Lower penalty for being able to leave.
                    queen_score[node.color() as usize] -= self.queen_liberty_factor / 2;
                }
                if pillbug_powers && board.node(friendly_queen).clipped_height() == 1 {
                    let best_escape = adjacent(hex)
                        .into_iter()
                        .map(|lib| {
                            if board.occupied(lib) {
                                0
                            } else {
                                count_liberties(board, friendly_queen, lib)
                            }
                        })
                        .max()
                        .unwrap_or(0);
                    if best_escape > 2 {
                        pillbug_defense[node.color() as usize] = true;
                    }
                }
            }

            let enemy_queen = board.queens[node.color().other()];

            if adjacent(enemy_queen).contains(&hex) {
                // Discourage liberty filling by valuable bugs, by setting their score to zero when filling a liberty.
                bug_score = 0;
                // A little extra boost for filling opponent's queen, as we will never choose to move.
                queen_score[node.color().other()] -= self.queen_liberty_factor * 12 / 10;
                if pillbug_powers {
                    let best_unescape = adjacent(hex)
                        .into_iter()
                        .map(|lib| {
                            if board.occupied(lib) {
                                6
                            } else {
                                count_liberties(board, enemy_queen, lib)
                            }
                        })
                        .min()
                        .unwrap_or(6);
                    if best_unescape < 3 {
                        queen_score[node.color().other()] = -self.queen_liberty_factor;
                    }
                }
            }

            if !node.is_stacked() && immovable.get(hex) {
                // Pinned bugs are worthless.
                continue;
            }

            bug_score *= self.movable_bug_factor;
            if node.color() != board.to_move() {
                bug_score = -bug_score;
                // Make low-aggression mode value opponent movability higher than ours.
                if self.aggression == 1 {
                    bug_score *= 2
                } else if self.aggression == 2 {
                    bug_score = bug_score * 3 / 2;
                }
            }
            score += bug_score;
        }

        let mut pillbug_defense_score = self.pillbug_defense_bonus
            * (pillbug_defense[board.to_move() as usize] as Evaluation
                - pillbug_defense[board.to_move().other()] as Evaluation);

        // Check for backup defensive pillbug placeability option, discounted value
        pillbug_defense = [false; 2];
        for &color in &[Color::Black, Color::White] {
            if board.node(board.queens[color as usize]).clipped_height() == 1
                && board.remaining[color as usize][Bug::Pillbug as usize] > 0
                && adjacent(board.queens[color as usize])
                    .iter()
                    .any(|&lib| placeable(board, lib, color))
            {
                pillbug_defense[color as usize] = true;
            }
        }
        pillbug_defense_score += self.pillbug_defense_bonus / 2
            * (pillbug_defense[board.to_move() as usize] as Evaluation
                - pillbug_defense[board.to_move().other()] as Evaluation);

        let queen_score =
            queen_score[board.to_move() as usize] - queen_score[board.to_move().other()];
        queen_score + pillbug_defense_score + score
    }

    // The idea here is to use quiescence search to avoid ending on a
    // placement. This is based on the hypothesis that new pieces are placed
    // with the intention of moving them on the next turn. Stopping the search
    // just after placing a piece can give bad results because it would
    // usually pins one of your own pieces and doesn't put the new piece where
    // it will be useful. Thus, each player can get a bonus move to move a
    // piece that they have just placed (but not other pieces).
    fn generate_noisy_moves(&self, board: &Board, moves: &mut Vec<Turn>) {
        if board.turn_history.len() < 4 || board.get_remaining()[Bug::Queen as usize] == 0 {
            // Wait until movements are at least possible.
            return;
        }
        let enemy_last_move = board.turn_history[board.turn_history.len() - 1];
        let my_last_move = board.turn_history[board.turn_history.len() - 2];

        if let Turn::Place(hex, _) = my_last_move {
            // Drop attack is quiet enough.
            if !adjacent(board.queens[board.to_move().other()]).contains(&hex) {
                // TODO: just generate from this spot (ignoring throws?).
                board.generate_movements(moves);
                moves.retain(|m| if let Turn::Move(start, _) = *m { start == hex } else { false });
                // If the piece became pinned or covered, this will return no
                // moves, which means the search will terminate.
                return;
            }
        }

        if let Turn::Place(hex, _) = enemy_last_move {
            if !adjacent(board.queens[board.to_move() as usize]).contains(&hex) {
                // We didn't just place, but opponent did. Do some movement to
                // give them a chance to quiesce.
                board.generate_movements(moves);
            }
        }

        // If no one just placed something, return nothing and stop the search.
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_minimax() {
        use minimax::{Move, Negamax, Strategy};

        // Find the winning move.
        // ．．．🐝🕷．．
        //．．🐜🐜🐝．．
        // ．．．🦗🪲
        let mut board = Board::default();
        Turn::Place(loc_to_hex((0, 0)), Bug::Queen).apply(&mut board);
        Turn::Place(loc_to_hex((1, 0)), Bug::Spider).apply(&mut board);
        Turn::Place(loc_to_hex((-1, 1)), Bug::Ant).apply(&mut board);
        Turn::Place(loc_to_hex((0, 1)), Bug::Ant).apply(&mut board);
        Turn::Place(loc_to_hex((1, 2)), Bug::Grasshopper).apply(&mut board);
        Turn::Place(loc_to_hex((1, 1)), Bug::Queen).apply(&mut board);
        Turn::Place(loc_to_hex((2, 2)), Bug::Beetle).apply(&mut board);
        Turn::Pass.apply(&mut board);
        for depth in 1..3 {
            let mut strategy = Negamax::new(DumbEvaluator {}, depth);
            let m = strategy.choose_move(&mut board);
            assert_eq!(Some(Turn::Move(loc_to_hex((-1, 1)), loc_to_hex((2, 1)))), m);

            let mut strategy = Negamax::new(BasicEvaluator::default(), depth);
            let m = strategy.choose_move(&mut board);
            assert_eq!(Some(Turn::Move(loc_to_hex((-1, 1)), loc_to_hex((2, 1)))), m);
        }

        // Find queen escape.
        //．．🕷🐝🐝．
        // ．．🦗🕷．
        let mut board = Board::default();
        Turn::Place(loc_to_hex((0, 0)), Bug::Queen).apply(&mut board);
        Turn::Place(loc_to_hex((1, 0)), Bug::Queen).apply(&mut board);
        Turn::Place(loc_to_hex((1, 1)), Bug::Spider).apply(&mut board);
        Turn::Place(loc_to_hex((0, 1)), Bug::Grasshopper).apply(&mut board);
        Turn::Place(loc_to_hex((-1, 0)), Bug::Beetle).apply(&mut board);
        Turn::Pass.apply(&mut board);
        for depth in 1..3 {
            let mut strategy = Negamax::new(BasicEvaluator::default(), depth);
            let m = strategy.choose_move(&mut board);
            assert_eq!(Some(Turn::Move(loc_to_hex((0, 0)), loc_to_hex((0, -1)))), m);
        }
    }
}
