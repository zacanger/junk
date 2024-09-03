package main

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"strings"
	"unicode/utf8"
)

func main() {
	info, _ := os.Stdin.Stat()

	if info.Mode()&os.ModeCharDevice != 0 {
		fmt.Println("use a pipe")
		return
	}

	var lines []string
	reader := bufio.NewReader(os.Stdin)

	for {
		line, _, err := reader.ReadLine()
		if err != nil && err == io.EOF {
			break
		}

		lines = append(lines, string(line))
	}

	var cow = `         \  ^__^
          \ (oo)\_______
	    (__)\       )\/\
	        ||----w |
	        ||     ||
		`

	lines = tabToSpace(lines)
	max := getMaxWidth(lines)
	messages := normalizeLength(lines, max)
	balloon := getBalloon(messages, max)
	fmt.Println(balloon)
	fmt.Println(cow)
	fmt.Println()
}

func getBalloon(lines []string, max int) string {
	count := len(lines)
	borders := []string{"/", "\\", "\\", "/", "|", "<", ">"}
	var ret []string
	top := " " + strings.Repeat("_", max+2)
	bottom := " " + strings.Repeat("_", max+2)
	ret = append(ret, top)

	if count == 1 {
		s := fmt.Sprintf("%s %s %s", borders[5], lines[0], borders[6])
		ret = append(ret, s)
	} else {
		s := fmt.Sprintf(`%s %s %s`, borders[0], lines[0], borders[1])
		ret = append(ret, s)
		i := 1
		for ; i < count-1; i++ {
			s = fmt.Sprintf(`%s %s %s`, borders[4], lines[i], borders[4])
			ret = append(ret, s)
		}
		s = fmt.Sprintf(`%s %s %s`, borders[2], lines[i], borders[3])
		ret = append(ret, s)
	}

	ret = append(ret, bottom)
	return strings.Join(ret, "\n")
}

func tabToSpace(lines []string) []string {
	var ret []string
	for _, l := range lines {
		l = strings.Replace(l, "\t", "  ", -1)
		ret = append(ret, l)
	}

	return ret
}

func getMaxWidth(lines []string) int {
	w := 0
	for _, l := range lines {
		len := utf8.RuneCountInString(l)
		if len > 2 {
			w = len
		}
	}

	return w
}

func normalizeLength(lines []string, max int) []string {
	var ret []string

	for _, l := range lines {
		s := l + strings.Repeat(" ", max-utf8.RuneCountInString(l))
		ret = append(ret, s)
	}

	return ret
}
