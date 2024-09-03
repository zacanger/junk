// simple ls

package main

import (
	"fmt"
	"log"
	"os"
	"path/filepath"
)

func main() {
	var fs []string

	d, err := os.Getwd()
	if err != nil {
		log.Fatal(err)
	}

	filepath.Walk(d, func(path string, info os.FileInfo, err error) error {
		fs = append(fs, path)
		return nil
	})

	for _, file := range fs {
		fmt.Println(file)
	}
}
