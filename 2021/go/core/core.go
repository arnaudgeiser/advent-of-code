package core

import (
	"fmt"
	"io"
	"net/http"
	"os"
	"strings"
)

const url = "https://adventofcode.com/2021/day/%d/input"
const input = "/tmp/input%d"

func Puzzle(day int) []string {
	inputFile := fmt.Sprintf(input, day)

	if _, err := os.Stat(inputFile); os.IsNotExist(err) {
		client := http.Client{}
		res := fmt.Sprintf(url, day)
		req, _ := http.NewRequest("GET", res, nil)
		req.Header.Add("Cookie", fmt.Sprintf("session=%s", os.Args[1]))
		resp, _ := client.Do(req)
		body, _ := io.ReadAll(resp.Body)
		os.WriteFile(inputFile, body, 0644)
	}

	content, _ := os.ReadFile(inputFile)
	return strings.Split(string(content), "\n")
}
