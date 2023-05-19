package main

import (
	"fmt"
	"net/http"
	"github.com/gorilla/mux"
)


func main() {
	r := mux.NewRouter()
	r.HandleFunc("/users/{user}", func(w http.ResponseWriter, r *http.Request) {
		user := mux.Vars(r)["user"]
		fmt.Fprintf(w, "hi %s\n", user)
	}).Methods("GET")
	http.ListenAndServe(":8000", r)
}