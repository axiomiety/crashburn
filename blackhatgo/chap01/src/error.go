package main

import (
"fmt"
"errors"
)

func foo() error {
	return errors.New("woops")
}

func main() {
if err := foo(); err != nil {
  fmt.Println(err)
}
}
