use crate::board::*;

// An evaluator that knows nothing but the rules, and maximally explores the tree.
pub struct DumbEvaluator;

impl minimax::Evaluator for DumbEvaluator {
    type G = Rules;
    fn evaluate(&self, _: &Board) -> minimax::Evaluation {
        0
    }
}

// An evaluator that counts movable pieces and how close to death the queen is.
pub struct BasicEvaluator {
    queen_factor: i32,
    movable_bug_factor: i32,
    unplayed_bug_factor: i32,
}

impl Default for BasicEvaluator {
    fn default() -> Self {
        Self { queen_factor: 40, movable_bug_factor: 2, unplayed_bug_factor: 1 }
    }
}

impl minimax::Evaluator for BasicEvaluator {
    type G = Rules;
    fn evaluate(&self, board: &Board) -> minimax::Evaluation {
        let mut buf = [0; 6];
        let queens_surrounded = board.queens_surrounded();
        let immovable = board.find_cut_vertexes();

        fn value(bug: Bug) -> i32 {
            // Mostly made up. All I know is that ants are good.
            match bug {
                Bug::Queen => 10,
                Bug::Ant => 7,
                Bug::Beetle => 6,
                Bug::Grasshopper => 4,
                Bug::Spider => 3,
                Bug::Mosquito => 0, // See below.
                Bug::Ladybug => 5,
                Bug::Pillbug => 4,
            }
        }

        let mut score = queens_surrounded[1 - board.to_move() as usize] as i32
            - queens_surrounded[board.to_move() as usize] as i32;
        score *= self.queen_factor;

        let remaining = board.get_remaining();
        let opp_remaining = board.get_opponent_remaining();
        for bug in Bug::iter_all() {
            score += (remaining[bug as usize] as i32 - opp_remaining[bug as usize] as i32)
                * self.unplayed_bug_factor;
        }

        let mut queue = vec![board.queens[0]];
        let mut visited = NodeSet::new();
        while let Some(id) = queue.pop() {
            if visited.get(id) {
                continue;
            }
            visited.set(id);
            let node = board.node(id);
            if !node.occupied() {
                continue;
            }
            let mut bug_score = value(node.bug());
            let pillbug = node.bug() == Bug::Pillbug; // TODO also pillbug'd mosquito.
            let pillbug_near_its_queen = pillbug
                && adjacent(id).iter().any(|&adj| {
                    let node2 = board.node(adj);
                    node2.occupied() && node2.bug() == Bug::Queen && node2.color() == node.color()
                });
            if pillbug_near_its_queen {
                // Pillbugs get a bonus if adjacent to matching queen.
                // for each empty adjacent square.
                bug_score += (self.queen_factor / 2)
                    * adjacent(id).iter().filter(|adj| !board.occupied(**adj)).count() as i32;
            } else if !node.is_stacked() && immovable.get(id) {
                continue;
            }
            if node.bug() == Bug::Mosquito {
                // Mosquitos are valued as they can currently move.
                if node.is_stacked() {
                    bug_score = value(Bug::Beetle);
                } else {
                    bug_score = adjacent(id)
                        .iter()
                        .map(|&id| if board.occupied(id) { value(board.node(id).bug()) } else { 0 })
                        .max()
                        .unwrap_or(0);
                }
            }
            if node.bug().crawler() {
                // Treat blocked crawlers as immovable.
                if board.slidable_adjacent(&mut buf, id, id).next().is_none() {
                    continue;
                }
            }
            bug_score *= self.movable_bug_factor;
            if node.color() != board.to_move() {
                bug_score = -bug_score;
            }
            score += bug_score;
        }

        score as minimax::Evaluation
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
