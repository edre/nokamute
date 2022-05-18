use crate::board::*;

use minimax::{Evaluation, Evaluator};
use std::cmp::max;

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
    queen_factor: Evaluation,
    movable_bug_factor: Evaluation,
    unplayed_bug_factor: Evaluation,
    beetle_attack_factor: Evaluation,
}

impl Default for BasicEvaluator {
    fn default() -> Self {
        Self {
            queen_factor: 40,
            movable_bug_factor: 2,
            unplayed_bug_factor: 1,
            beetle_attack_factor: 15,
        }
    }
}

fn count_liberties(board: &Board, origin: Id, id: Id) -> Evaluation {
    adjacent(id).into_iter().filter(|&adj| adj == origin || !board.occupied(adj)).count()
        as Evaluation
}

// We really only care about 0, 1, 2, many
fn distance(start: Id, end: Id) -> Id {
    // Computing this directly on the spiral torus was too hard.
    if start == end {
        return 0;
    }
    if adjacent(start).contains(&end) {
        return 1;
    }
    for d1 in adjacent(start) {
        if adjacent(d1).contains(&end) {
            return 2;
        }
    }
    return 3;
}

impl Evaluator for BasicEvaluator {
    type G = Rules;

    fn evaluate(&self, board: &Board) -> Evaluation {
        let mut buf = [0; 6];
        let mut immovable = board.find_cut_vertexes();

        fn value(bug: Bug) -> Evaluation {
            // Mostly made up. All I know is that ants are good.
            match bug {
                Bug::Queen => 10,
                Bug::Ant => 7,
                Bug::Beetle => 6,
                Bug::Grasshopper => 4,
                Bug::Spider => 3,
                Bug::Mosquito => 8, // See below.
                Bug::Ladybug => 5,
                Bug::Pillbug => 4,
            }
        }

        let mut score = 0;
        let mut beetle_attack_score = [0; 2];
        let mut pillbug_defense_score = [0; 2];
        let mut queen_score = [0; 2];

        let remaining = board.get_remaining();
        let opp_remaining = board.get_opponent_remaining();
        for bug in Bug::iter_all() {
            score += (remaining[bug as usize] as Evaluation
                - opp_remaining[bug as usize] as Evaluation)
                * self.unplayed_bug_factor;
        }

        for &id in board.occupied_ids[0].iter().chain(board.occupied_ids[1].iter()) {
            let node = board.node(id);
            let mut bug_score = value(node.bug());
            let mut pillbug_powers = node.bug() == Bug::Pillbug;
            let mut beetle_powers = node.bug() == Bug::Beetle;
            let mut crawler = node.bug().crawler();
            if node.bug() == Bug::Mosquito {
                // Mosquitos are valued as they can currently move.
                bug_score = 0;
                crawler = true;
                if node.is_stacked() {
                    bug_score = value(Bug::Beetle);
                    beetle_powers = true;
                } else {
                    for adj in adjacent(id) {
                        if board.occupied(adj) {
                            let bug = board.node(adj).bug();
                            if bug != Bug::Queen {
                                bug_score = value(bug);
                            }
                            if bug == Bug::Beetle {
                                beetle_powers = true;
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
                if board.slidable_adjacent(&mut buf, id, id).next().is_none() {
                    immovable.set(id);
                }
            }

            let friendly_queen = board.queens[node.color() as usize];

            if adjacent(friendly_queen).contains(&id) {
                // Filling friendly queen's liberty.
                if immovable.get(id) && !node.is_stacked() {
                    queen_score[node.color() as usize] -= self.queen_factor;
                } else {
                    // Lower penalty for being able to leave.
                    queen_score[node.color() as usize] -= self.queen_factor / 2;
                }
                if pillbug_powers {
                    let best_escape = adjacent(id)
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
                    if best_escape > 2 && pillbug_defense_score[node.color() as usize] >= 0 {
                        // Don't double count bonus points from pillbug and mosquito. Only one of them can do an escape.
                        // Enemy pillbug trumps friendly pillbug. Move to safety early.
                        pillbug_defense_score[node.color() as usize] = self.queen_factor * 2;
                    }
                }
            }

            let enemy_queen = board.queens[node.color().other()];

            if adjacent(enemy_queen).contains(&id) {
                // A little extra boost for filling opponent's queen, as we will never choose to move.
                queen_score[node.color().other()] -= self.queen_factor * 12 / 10;
                // If this bug is already filling a queen's liberty, so don't
                // also give it its movability bonus. It's more valuable here
                // than moving around.
                bug_score = 0;
                if pillbug_powers {
                    let best_unescape = adjacent(id)
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
                        pillbug_defense_score[node.color().other()] = -self.queen_factor;
                    }
                }
            }

            if !node.is_stacked() && immovable.get(id) {
                // Pinned bugs are worthless.
                continue;
            }

            // Beetles prevent queen shenanigans, give a bonus for a movable beetle near opponent's queen.
            if beetle_powers {
                let dist = distance(id, board.queens[node.color().other()]);
                if dist < 3 {
                    beetle_attack_score[node.color() as usize] = max(
                        beetle_attack_score[node.color() as usize],
                        (3 - dist as Evaluation) * self.beetle_attack_factor,
                    );
                }
            }

            bug_score *= self.movable_bug_factor;
            if node.color() != board.to_move() {
                bug_score = -bug_score;
            }
            score += bug_score;
        }

        let queen_score =
            queen_score[board.to_move() as usize] - queen_score[board.to_move().other()];
        let beetle_attack_score = beetle_attack_score[board.to_move() as usize]
            - beetle_attack_score[board.to_move().other()];
        let pillbug_defense_score = pillbug_defense_score[board.to_move() as usize]
            - pillbug_defense_score[board.to_move().other()];
        queen_score + beetle_attack_score + pillbug_defense_score + score
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
        crate::Move::Place(loc_to_id((0, 0)), Bug::Queen).apply(&mut board);
        crate::Move::Place(loc_to_id((1, 0)), Bug::Spider).apply(&mut board);
        crate::Move::Place(loc_to_id((-1, 1)), Bug::Ant).apply(&mut board);
        crate::Move::Place(loc_to_id((0, 1)), Bug::Ant).apply(&mut board);
        crate::Move::Place(loc_to_id((1, 2)), Bug::Grasshopper).apply(&mut board);
        crate::Move::Place(loc_to_id((1, 1)), Bug::Queen).apply(&mut board);
        crate::Move::Place(loc_to_id((2, 2)), Bug::Beetle).apply(&mut board);
        crate::Move::Pass.apply(&mut board);
        for depth in 0..2 {
            let mut strategy = Negamax::new(DumbEvaluator {}, depth);
            let m = strategy.choose_move(&mut board);
            assert_eq!(Some(crate::Move::Movement(loc_to_id((-1, 1)), loc_to_id((2, 1)))), m);

            let mut strategy = Negamax::new(BasicEvaluator::default(), depth);
            let m = strategy.choose_move(&mut board);
            assert_eq!(Some(crate::Move::Movement(loc_to_id((-1, 1)), loc_to_id((2, 1)))), m);
        }

        // Find queen escape.
        //．．🕷🐝🐝．
        // ．．🦗🕷．
        let mut board = Board::default();
        crate::Move::Place(loc_to_id((0, 0)), Bug::Queen).apply(&mut board);
        crate::Move::Place(loc_to_id((1, 0)), Bug::Queen).apply(&mut board);
        crate::Move::Place(loc_to_id((1, 1)), Bug::Spider).apply(&mut board);
        crate::Move::Place(loc_to_id((0, 1)), Bug::Grasshopper).apply(&mut board);
        crate::Move::Place(loc_to_id((-1, 0)), Bug::Beetle).apply(&mut board);
        crate::Move::Pass.apply(&mut board);
        for depth in 0..3 {
            let mut strategy = Negamax::new(BasicEvaluator::default(), depth);
            let m = strategy.choose_move(&mut board);
            assert_eq!(Some(crate::Move::Movement(loc_to_id((0, 0)), loc_to_id((0, -1)))), m);
        }
    }
}
