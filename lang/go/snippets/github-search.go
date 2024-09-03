package main

import (
	"encoding/json"
	"flag"
	"fmt"
	"io/ioutil"
	"log"
	"net/http"
	"os"
	"text/tabwriter"
	"time"
)

func main() {
	lang := flag.String("lang", "go", "language")
	term := flag.String("term", "docker", "search term")
	flag.Parse()

	res, err := http.Get(
		"https://api.github.com/search/repositories?q=" +
			*term +
			"stars:>=10000+language:" +
			*lang +
			"&sort=stars&order=desc")
	if err != nil {
		log.Fatal(err)
	}

	body, err := ioutil.ReadAll(res.Body)
	res.Body.Close()
	if err != nil {
		log.Fatal(err)
	}
	if res.StatusCode != 200 {
		log.Fatal("oh no", res.StatusCode)
	}

	data := JSONData{}
	err = json.Unmarshal(body, &data)
	if err != nil {
		log.Fatal(err)
	}

	printData(data)
}

type Owner struct {
	Login string
}

type Item struct {
	Id              int
	Name            string
	FullName        string `json:"full_name"`
	Owner           Owner
	Description     string
	CreatedAt       string `json:"created_at"`
	StargazersCount int    `json:"stargazers_count"`
}

type JSONData struct {
	Count int `json:"total_count"`
	Items []Item
}

func printData(data JSONData) {
	const format = "%v\t%v\t%v\t%v\t\n"
	tw := new(tabwriter.Writer).Init(os.Stdout, 0, 8, 2, ' ', 0)
	fmt.Fprintf(tw, format, "Repo", "Stars", "Created", "Description")

	for _, i := range data.Items {
		desc := i.Description
		if len(desc) > 30 {
			desc = string(desc[:30]) + "…"
		}

		t, err := time.Parse(time.RFC3339, i.CreatedAt)
		if err != nil {
			log.Fatal(err)
		}

		fmt.Fprintf(tw, format, i.FullName, i.StargazersCount, t.Year(), desc)
	}

	tw.Flush()
}
