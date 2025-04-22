package tree_sitter_rensai_test

import (
	"testing"

	tree_sitter "github.com/tree-sitter/go-tree-sitter"
	tree_sitter_rensai "github.com/xvw/kohai/bindings/go"
)

func TestCanLoadGrammar(t *testing.T) {
	language := tree_sitter.NewLanguage(tree_sitter_rensai.Language())
	if language == nil {
		t.Errorf("Error loading Rensai grammar")
	}
}
