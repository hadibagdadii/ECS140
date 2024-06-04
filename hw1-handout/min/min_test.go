package min

import "testing"

type Test struct {
	in  []int
	out int
}

var tests = []Test{
	{
		in:  []int{-1, 0, 1, 4},
		out: -1,
	},
	{
		in:  []int{1, 3, 2, 4},
		out: 1,
	},
	{
		in:  []int{8, 7, 6, 5},
		out: 5,
	},
	{
		in:  []int{8, 2, 6, 8},
		out: 2,
	},
	{
		in:  []int{-1, -3, -2, -4},
		out: -4,
	},
	{
		in:  []int{1},
		out: 1,
	},
	{
		in:  []int{2, 2},
		out: 2,
	},
	{
		in:  []int{},
		out: 0,
	},
	{
		in:  nil,
		out: 0,
	},
	// TODO add more tests for 100% test coverage
}

func TestMin(t *testing.T) {
	for i, test := range tests {
		m := Min(test.in)
		if m != test.out {
			t.Errorf("#%d: Min(%v)=%d; want %d", i, test.in, m, test.out)
		}
	}
}
