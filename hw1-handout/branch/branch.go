package branch

import (
	"go/ast"
	"go/parser"
	"go/token"
)

// branchCount returns the number of branching statements in the given function.
func branchCount(fn *ast.FuncDecl) uint {
	// Initialize a counter for the branching statements
	var count uint

	// Define a function to be called for each node in the AST
	ast.Inspect(fn, func(node ast.Node) bool {
		switch node.(type) {
		case *ast.IfStmt, *ast.ForStmt, *ast.RangeStmt, *ast.SwitchStmt, *ast.TypeSwitchStmt:
			count++
		}
		// Continue traversing the AST
		return true
	})
	return count
}

// ComputeBranchFactors returns a map from the name of the function in the given
func ComputeBranchFactors(src string) map[string]uint {
	fset := token.NewFileSet()
	f, err := parser.ParseFile(fset, "src.go", src, 0)
	if err != nil {
		panic(err)
	}

	m := make(map[string]uint)
	for _, decl := range f.Decls {
		switch fn := decl.(type) {
		case *ast.FuncDecl:
			m[fn.Name.Name] = branchCount(fn)
		}
	}

	return m
}
