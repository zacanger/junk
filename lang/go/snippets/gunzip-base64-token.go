package main

import (
	"bytes"
	"compress/gzip"
	"encoding/base64"
	"fmt"
)

func main() {
	token := `
// stuff goes here
`
	var buf bytes.Buffer
	gz := gzip.NewWriter(&buf)
	if _, err := gz.Write([]byte(token)); err != nil {
		panic(err)
	}
	if err := gz.Flush(); err != nil {
		panic(err)
	}
	if err := gz.Close(); err != nil {
		panic(err)
	}
	str := base64.StdEncoding.EncodeToString(buf.Bytes())
	encoded, _ := base64.StdEncoding.DecodeString(str)

	fmt.Println(len(str))
	fmt.Println(len(token))

	fmt.Println(str)
	fmt.Println(encoded)
}
