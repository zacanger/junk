package main

import (
	"log"
	"net/http"
	"net/http/httputil"
	"strings"
)

func main() {
	proxy := &httputil.ReverseProxy{Director: func(req *http.Request) {
		originHost := "localhost:9999"

		req.Header.Add("X-Forwarded-Host", req.Host)
		req.Header.Add("X-Origin-Host", originHost)
		req.Host = originHost
		req.URL.Scheme = "http"
		req.URL.Host = originHost
		req.URL.Path = strings.Replace(req.URL.Path, "proxy/", "", 1)
	}}

	http.HandleFunc("/proxy/", func(w http.ResponseWriter, r *http.Request) {
		proxy.ServeHTTP(w, r)
	})

	log.Fatal(http.ListenAndServe(":8888", nil))
}
