package main

import (
	"aoc/core"
	"fmt"
	"strconv"
)

var content []string = core.Puzzle(1)

func main() {
	part1()
	part2()
}

func part1() {
	nums := make([]int, len(content))
	for i, line := range content {
		nums[i], _ = strconv.Atoi(line)
	}

	res := 0
	for i := 1; i < len(nums); i++ {
		if nums[i] > nums[i-1] {
			res++
		}
	}
	fmt.Println(res)
}

func part2() {
	nums := make([]int, len(content))
	for i, line := range content {
		nums[i], _ = strconv.Atoi(line)
	}

	res := 0
	last := nums[0] + nums[1] + nums[2]
	for i := 1; i < len(nums)-2; i++ {
		sum := nums[i] + nums[i+1] + nums[i+2]
		if sum > last {
			res++
		}
		last = sum
	}
	fmt.Println(res)
}
