package main

import (
	"encoding/json"
	"io/ioutil"
	"log"
	"net/http"
)

type Member struct {
	Animal string `json:"animal"`
	Color  string `json:"color"`
	Name   string `json:"name"`
}

var members []Member

func init() {
	data, _ := ioutil.ReadFile("members.json")
	json.Unmarshal(data, &members)
}

func Handler(w http.ResponseWriter, r *http.Request) {
	response, _ := json.Marshal(members)
	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(200)
	w.Write(response)
}

func main() {
	http.HandleFunc("/", Handler)
	if err := http.ListenAndServe(":4000", nil); err != nil {
		log.Fatal(err)
	}
}
