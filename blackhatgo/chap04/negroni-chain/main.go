package main

import (
	"fmt"
	"github.com/gorilla/mux"
	"github.com/urfave/negroni"
	"net/http"
)

type trivial struct {
	Id string
}

func (t *trivial) ServeHTTP(w http.ResponseWriter, r *http.Request, next http.HandlerFunc) {
	fmt.Println("trivial middleware %s, calling next in chain", t.Id)
	next(w, r)
}

func main() {
	r := mux.NewRouter()
	n := negroni.Classic()
	t1 := trivial{Id: "mw1"}
	t2 := trivial{Id: "mw2"}
	n.UseHandler(r)
	n.Use(&t1)
	n.Use(&t2)
	http.ListenAndServe(":8000", n)
}
