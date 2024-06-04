package nfa

// A state in the NFA is labeled by a single integer.
type state uint

// TransitionFunction tells us, given a current state and some symbol, which
// other states the NFA can move to.
//
// Deterministic automata have only one possible destination state,
// but we're working with non-deterministic automata.
type TransitionFunction func(st state, act rune) []state

func Reachable(
	// `transitions` tells us what our NFA looks like
	transitions TransitionFunction,
	// `start` and `final` tell us where to start, and where we want to end up
	start, final state,
	// `input` is a (possible empty) list of symbols to apply.
	input []rune,
) bool {
	// return true if the nfa accepts the input and can reach the final state with that input,
	// return false otherwise

	// base case #1, no transitions, start and final are different
	if len(input) == 0 && start != final {
		return false
	}

	// base case #2, no transitions, start and final are the same
	if len(input) == 0 && start == final {
		return true
	}

	// store all potential next states for the current char
	states := transitions(start, input[0])

	// return false if no next states
	if len(states) == 0 {
		return false
	}

	// loop through each next state and check if reachable, if reachable return back with true
	for _, nextstates := range states {
		if Reachable(transitions, nextstates, final, input[1:]) {
			return true
		}
	}
	return false
}
