package nfa

import (
	"sync"
)

// A state in the NFA is represented as an unsigned integer.
type state uint

// Given the current state and a symbol, the transition function
// of an NFA returns the set of next states the NFA can transition to
// on reading the given symbol.
// This set of next states could be empty.
type TransitionFunction func(st state, sym rune) []state

// Reachable returns true if there exists a sequence of transitions
// from `transitions` such that if the NFA starts at the start state
// `start` it would reach the final state `final` after reading the
// entire sequence of symbols `input`; Reachable returns false otherwise.
func Reachable(
	transitions TransitionFunction,
	start, final state,
	input []rune,
) bool {
	var wg sync.WaitGroup
	found := make(chan bool)
	done := make(chan bool)

	// Function to explore states
	var explore func(currentState state, input []rune)
	explore = func(currentState state, input []rune) {
		defer wg.Done()
		if len(input) == 0 {
			if currentState == final {
				found <- true
			}
			return
		}

		nextStates := transitions(currentState, input[0])
		for _, nextState := range nextStates {
			wg.Add(1)
			go explore(nextState, input[1:])
		}
	}

	wg.Add(1)
	go explore(start, input)

	// Goroutine to wait for all explorations to complete
	go func() {
		wg.Wait()
		close(done)
	}()

	select {
	case <-found:
		return true
	case <-done:
		return false
	}
}
