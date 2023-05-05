package main

import (
	"fmt"
)

type Person struct {
	Name string
	Age int
}

func (p *Person) SayHello() { fmt.Println("Hello, ", p.Name)}

func main() {
	fmt.Println("Hello, Gophers")
	guy := new(Person)
	guy.Name = "Dave"
	guy.SayHello()
}
