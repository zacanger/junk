package main

import (
	"fmt"
	"os/exec"
)

func main() {
	c := exec.Command("/bar", "-bar")
	buf, err := cmd.Output()
	fmt.Printf("%s\n", string(buf))
}
