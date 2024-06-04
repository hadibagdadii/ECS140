package rewrite

import (
	"bytes"
	"go/ast"
	"go/format"
	"go/parser"
	"go/token"
	"hw1/expr"
	"hw1/simplify"
	"strconv"
)

// rewriteCalls should modify the passed AST
func rewriteCalls(node ast.Node) {
	// from here
	ast.Inspect(node, func(n ast.Node) bool {
		switch x := n.(type) {
		case *ast.CallExpr:
			// Check if the call expression has exactly two arguments
			if len(x.Args) != 2 {
				return false
			}
			// Check if the function being called is expr.ParseAndEval
			if sel, ok := x.Fun.(*ast.SelectorExpr); ok {
				if ident, ok := sel.X.(*ast.Ident); ok {
					if ident.Name == "expr" {
						if sel.Sel.Name == "ParseAndEval" {
							// Check if the first argument is a string literal
							if arg, ok := x.Args[0].(*ast.BasicLit); ok {
								// Unquote the string literal
								unquoted, _ := strconv.Unquote(arg.Value)
								// Parse the expression
								parsedExpr, err := expr.Parse(unquoted)
								if err == nil {
									// Simplify the parsed expression
									simplifiedExpr := simplify.Simplify(parsedExpr, expr.Env{})
									// Format the simplified expression as a string
									simplifiedString := strconv.Quote(expr.Format(simplifiedExpr))
									// Update the first argument with the simplified string
									arg.Value = simplifiedString
								}
							}
						}
					}
				}
			}
		}
		return true
	})
	// to here
}

func SimplifyParseAndEval(src string) string {
	fset := token.NewFileSet()
	f, err := parser.ParseFile(fset, "src.go", src, 0)
	if err != nil {
		panic(err)
	}

	rewriteCalls(f)

	var buf bytes.Buffer
	format.Node(&buf, fset, f)
	return buf.String()
}
