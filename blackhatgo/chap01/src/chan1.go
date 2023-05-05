package main

import (
	"fmt"
)

func strlen(s string, c chan int) {
	c <- len(s)
}

func main() {
	c := make(chan int)
	go strlen("Saluations", c)
	go strlen("World", c)
	x, y := <-c, <-c
	fmt.Println(x, y, x+y)
}
