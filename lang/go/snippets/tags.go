package main

import (
	"fmt"
	"reflect"
)

type Data struct {
	Item string `something:"item"`
}

func main() {
	d := Data{"whatever"}
	t := reflect.TypeOf(d)
	field := t.Field(0)
	fmt.Print(field.Tag.Get("something"))
}
