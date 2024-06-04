package smash

import (
	"bufio"
	"io"
	"sync"
)

type word string

// Smash reads text data from the provided io.Reader, processes each word using the smasher function,
// and returns a map[uint32]uint where the keys are the results of the smasher function and the values
// are the counts of words that produced the same result.
func Smash(r io.Reader, smasher func(word) uint32) map[uint32]uint {
	// Create a buffered channel to send words to worker goroutines.
	wordChan := make(chan word)
	// Create a buffered channel to collect results from worker goroutines.
	resultChan := make(chan uint32)

	// WaitGroup to wait for all worker goroutines to finish.
	var wg sync.WaitGroup

	// Number of worker goroutines.
	const numWorkers = 10

	// Start worker goroutines.
	for i := 0; i < numWorkers; i++ {
		wg.Add(1)
		go func() {
			defer wg.Done()
			for w := range wordChan {
				result := smasher(w)
				resultChan <- result
			}
		}()
	}

	// Goroutine to read words from the io.Reader and send them to the word channel.
	go func() {
		scanner := bufio.NewScanner(r)
		scanner.Split(bufio.ScanWords)
		for scanner.Scan() {
			wordChan <- word(scanner.Text())
		}
		close(wordChan)
	}()

	// Goroutine to wait for all workers to finish and then close the result channel.
	go func() {
		wg.Wait()
		close(resultChan)
	}()

	// Map to store the final counts.
	resultMap := make(map[uint32]uint)

	// Collect results from the result channel and update the result map.
	for res := range resultChan {
		resultMap[res]++
	}

	return resultMap
}
