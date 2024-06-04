package depth

import (
	"hw1/expr"
)

// Depth calculates the depth of an expression tree.
func Depth(expression expr.Expr) uint {
	switch expression := expression.(type) {
	case expr.Var:
		// Depth of a variable is 1, as they are leaf nodes.
		return 1
	case expr.Literal:
		// Depth of a literal is 1, as they are leaf nodes.
		return 1
	case expr.Unary:
		// Depth of a unary operation is 1 plus the depth of its subtree.
		return 1 + Depth(expression.X)
	case expr.Binary:
		// Depth of a binary operation is 1 plus the maximum depth of its subtrees.
		leftDepth := Depth(expression.X)
		rightDepth := Depth(expression.Y)
		if leftDepth > rightDepth {
			return 1 + leftDepth
		}
		return 1 + rightDepth
	default:
		// Unexpected expression type.
		panic("expression")
	}
}
