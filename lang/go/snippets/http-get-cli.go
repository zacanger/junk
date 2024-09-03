package main

import (
	"fmt"
	"io/ioutil"
	"net/http"
	"os"
)

func main() {
	url := os.Args[1]
	res, err := http.Get(url)

	if err != nil {
		panic(err)
	}

	defer res.Body.Close()

	bodyBytes, err := ioutil.ReadAll(res.Body)

	if err != nil {
		panic(err)
	}

	bodyStr := string(bodyBytes)
	fmt.Println(bodyStr)

	// fmt.Println("status", res.Status)
}
