package simplify

import (
	"hw1/expr"
)

// Simplify simplifies an expression 'expr' given the environment 'env'
func Simplify(exprToSimplify expr.Expr, environment expr.Env) expr.Expr {
	switch exprToSimplify := exprToSimplify.(type) {
	case expr.Literal:
		// Literal values are already simple, so return as is.
		return exprToSimplify

	case expr.Var:
		// If the variable has a value in the environment, return the value.
		if value, exists := environment[exprToSimplify]; exists {
			return expr.Literal(value)
		}
		// If not, return the variable itself.
		return exprToSimplify

	case expr.Unary:
		// Simplify the operand.
		simplifiedOperand := Simplify(exprToSimplify.X, environment)
		// Check for negation and simplify if possible.
		if exprToSimplify.Op == '-' {
			if literalOperand, isLiteral := simplifiedOperand.(expr.Literal); isLiteral {
				return expr.Literal(-literalOperand)
			}
		}
		// If simplification didn't change the operand, return the original unary operation.
		return expr.Unary{Op: exprToSimplify.Op, X: simplifiedOperand}

	case expr.Binary:
		// Simplify both operands.
		simplifiedLeft := Simplify(exprToSimplify.X, environment)
		simplifiedRight := Simplify(exprToSimplify.Y, environment)

		// Apply algebraic simplifications.
		switch exprToSimplify.Op {
		case '+':
			if literalLeft, isLiteral := simplifiedLeft.(expr.Literal); isLiteral && literalLeft == 0 {
				return simplifiedRight
			}
			if literalRight, isLiteral := simplifiedRight.(expr.Literal); isLiteral && literalRight == 0 {
				return simplifiedLeft
			}
		case '*':
			if literalLeft, isLiteral := simplifiedLeft.(expr.Literal); isLiteral && (literalLeft == 0 || literalLeft == 1) {
				if literalLeft == 0 {
					return expr.Literal(0)
				}
				return simplifiedRight
			}
			if literalRight, isLiteral := simplifiedRight.(expr.Literal); isLiteral && (literalRight == 0 || literalRight == 1) {
				if literalRight == 0 {
					return expr.Literal(0)
				}
				return simplifiedLeft
			}
		}
		// If both operands are literals, evaluate the expression.
		if _, isLiteralLeft := simplifiedLeft.(expr.Literal); isLiteralLeft {
			if _, isLiteralRight := simplifiedRight.(expr.Literal); isLiteralRight {
				return expr.Literal(exprToSimplify.Eval(environment))
			}
		}
		// If simplification didn't result in a literal, return the original binary operation.
		return expr.Binary{Op: exprToSimplify.Op, X: simplifiedLeft, Y: simplifiedRight}

	default:
		// Unexpected expression type.
		panic("can't simplify")
	}
}
