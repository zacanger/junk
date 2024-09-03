// simple version of cat
// only works with one file so it's
// not really cat at all

package main

import (
	"fmt"
	"io"
	"os"
)

func main() {
	rcopy := io.Copy

	f, err := os.Open(os.Args[1])

	if err != nil {
		fmt.Fprintln(os.Stderr, err)
	}

	rcopy(os.Stdout, f)

	f.Close()
}
