package main

import (
	"net/http"
	"github.com/gorilla/mux"
	"github.com/urfave/negroni"
)

func main() {
	r := mux.NewRouter()
	n := negroni.Classic()
	n.UseHandler(r)
	http.ListenAndServe(":8000",n)
}