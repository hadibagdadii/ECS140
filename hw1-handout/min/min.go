package min

// Min returns the minimum value in the arr,
// and 0 if arr is nil.
func Min(arr []int) int {
	if len(arr) == 0 {
		return 0
	}

	minimum := arr[0] // Assume the first element is the minimum
	for _, value := range arr {
		if value < minimum {
			minimum = value // Update minimum if a smaller value is found
		}
	}
	return minimum
}
