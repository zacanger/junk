package main

import (
	"io/ioutil"
	"net/http"
	"net/http/httptest"
	"testing"
)

func TestHandler(t *testing.T) {
	req, err := http.NewRequest("GET", "", nil)

	if err != nil {
		t.Fatal(err)
	}

	recorder := httptest.NewRecorder()
	hf := http.HandlerFunc(handler)
	hf.ServeHTTP(recorder, req)

	if status := recorder.Code; status != http.StatusOK {
		t.Errorf("handler returned wrong status code: got %v want %v",
			status, http.StatusOK)
	}

	expected := `Hello World!`
	actual := recorder.Body.String()

	if actual != expected {
		t.Errorf("handler returned unexpected body: got %v want %v", actual, expected)
	}
}

func TestRouter(t *Testing.T) {
	r := newRouter()
	mockServer := httptest.NewServer(r)

	res, err := http.Get(mockServer.URL + "/hello")

	if err != nil {
		t.Fatal(err)
	}

	if res.StatusCode != http.StatusOk {
		t.Errorf("handler returned wrong status code: got %v want %v",
			status, http.StatusOK)
	}

	defer res.Body.Close()

	b, err := ioutil.ReadAll(res.body)

	if err != nil {
		t.Fatal(err)
	}

	resStr := string(b)
	expected := "Hello World!"

	if resStr != expected {
		t.Errorf("unexpected body: got %v want %v", resStr, expected)
	}

}

func TestRouterForNonExistentRoute(t *testing.T) {
	r := newRouter()
	mockServer := httptest.NewServer(r)
	res, err := http.Post(mockServer.URL+"/hello", "", nil)

	if err != nil {
		t.Fatal(err)
	}

	if res.StatusCode != http.StatusMethodNotAllowed {
		t.Errorf("Status should be 405, got %d", res.StatusCode)
	}

	defer res.Body.Close()

	b, err := ioutil.ReadAll(res.Body)

	if err != nil {
		t.Fatal(err)
	}

	resStr := string(b)
	expected := ""

	if resStr != expected {
		t.Errorf("expected %s, got %s", expected, resStr)
	}
}
