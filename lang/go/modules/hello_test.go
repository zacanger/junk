package hello

import "testing"

func TestHello(t *testing.T) {
	exp := "Hello, world."
	if got := Hello(); got != exp {
		t.Errorf("Hello = %q, exp %q", got, exp)
	}
}
