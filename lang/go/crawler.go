package main

import (
	"flag"
	"fmt"
	"net/http"
	"os"
	"strings"

	"golang.org/x/net/html"
)

func main() {
	var url string
	flag.StringVar(&url, "url", "", "the url to parse")
	flag.Parse()

	if url == "" {
		flag.PrintDefaults()
		os.Exit(1)
	}

	visited := map[string]string{}
	analyze(url, url, &visited)
	for link, title := range visited {
		fmt.Printf("%s -> %s\n", link, title)
	}
}

func analyze(url, baseurl string, visited *map[string]string) {
	page, err := parse(url)
	if err != nil {
		fmt.Printf("error %s on page %s\n", err, url)
		return
	}

	title := pageTitle(page)
	(*visited)[url] = title
	links := pageLinks(nil, page)

	for _, link := range links {
		if (*visited)[link] == "" && strings.HasPrefix(link, baseurl) {
			analyze(link, baseurl, visited)
		}
	}
}

func pageTitle(n *html.Node) string {
	var title string

	if n.Type == html.ElementNode && n.Data == "title" {
		return n.FirstChild.Data
	}

	for c := n.FirstChild; c != nil; c = c.NextSibling {
		title = pageTitle(c)
		if title != "" {
			break
		}
	}

	return title
}

func pageLinks(links []string, n *html.Node) []string {
	if n.Type == html.ElementNode && n.Data == "a" {
		for _, a := range n.Attr {
			if a.Key == "href" {
				fmt.Printf("%s", a.Val)
				if !sliceContains(links, a.Val) {
					links = append(links, a.Val)
				}
			}
		}
	}

	for c := n.FirstChild; c != nil; c = c.NextSibling {
		links = pageLinks(links, c)
	}

	return links
}

func sliceContains(slice []string, value string) bool {
	for _, v := range slice {
		if v == value {
			return true
		}
	}

	return false
}

func parse(url string) (*html.Node, error) {
	r, err := http.Get(url)
	if err != nil {
		return nil, fmt.Errorf("can't get url")
	}

	b, err := html.Parse(r.Body)
	if err != nil {
		return nil, fmt.Errorf("can't parse html")
	}

	return b, err
}
