package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"net/http"
	"os"

	md "github.com/JohannesKaufmann/html-to-markdown"
)

func main() {
	baseURL := "https://html.duckduckgo.com/html/?q="
	query := os.Args[1]

	url := baseURL + query
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

	converter := md.NewConverter("", true, nil)

	converted, err := converter.ConvertString(bodyStr)
	if err != nil {
		log.Fatal(err)
	}

	fmt.Println(converted)
}
