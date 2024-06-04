package triangle

import "testing"

func TestGetTriangleType(t *testing.T) {
	type Test struct {
		a, b, c  int
		expected triangleType
	}

	var tests = []Test{
		// Test cases for unknown triangle where one side is too long
		{30001, 6, 2, UnknownTriangle},
		{30001, 5, 3, UnknownTriangle},
		{3, 20001, 2, UnknownTriangle},
		{1, 20001, 1, UnknownTriangle},
		{3, 2, 10001, UnknownTriangle},

		// Test cases for unknown triangle due to one side being less than 0
		{0, 1, 1, UnknownTriangle},
		{1, 0, 1, UnknownTriangle},
		{1, 1, 0, UnknownTriangle},
		{0, 0, 1, UnknownTriangle},
		{1, 0, 0, UnknownTriangle},
		{0, 1, 0, UnknownTriangle},
		{0, 0, 0, UnknownTriangle},
		{-1, 1, 1, UnknownTriangle},
		{1, -1, 1, UnknownTriangle},
		{1, 1, -1, UnknownTriangle},
		{100000, 200000, 300000, UnknownTriangle},

		// Test cases for invalid triangle
		{1, 2, 3, InvalidTriangle},
		{1, 6, 3, InvalidTriangle},
		{10, 2, 4, InvalidTriangle},

		// Test cases for different types of triangles
		{6, 4, 5, AcuteTriangle},
		{1, 1, 1, AcuteTriangle},
		{6, 3, 4, ObtuseTriangle},
		{5, 3, 4, RightTriangle},
	}

	for _, test := range tests {
		actual := getTriangleType(test.a, test.b, test.c)
		if actual != test.expected {
			t.Errorf("getTriangleType(%d, %d, %d)=%v; want %v", test.a, test.b, test.c, actual, test.expected)
		}
	}
}
