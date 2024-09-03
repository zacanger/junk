// little thing that just sends back
// the client's ip address

package main

import (
	"log"
	"net/http"
	"os"
)

func handler(w http.ResponseWriter, req *http.Request) {
	xForwardedFor := req.Header.Get("x-forwarded-for")
	remoteAddr := req.RemoteAddr

	ip := xForwardedFor

	if ip == "" {
		ip = remoteAddr
	}

	w.Header().Set("content-type", "text/plain")
	w.WriteHeader(200)
	w.Write([]byte(ip))
}

func main() {
	port := os.Getenv("PORT")
	if port == "" {
		port = "3000"
	}
	http.HandleFunc("/", handler)
	log.Println("listening on http://localhost:" + port)
	log.Fatal(http.ListenAndServe(":"+port, nil))
}
