package main

import (
"fmt"
"encoding/json"
)

type Foo struct {
Bar string
Baz string
}

func main() {
f := Foo{"Joe Junior", "hello Shabado"}
b, _ := json.Marshal(f)
fmt.Println(string(b))
json.Unmarshal(b, &f)
}
