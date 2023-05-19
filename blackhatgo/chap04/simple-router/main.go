package main
import (
	"fmt"
	"net/http"
)

type router struct {

}

func (r *router) ServeHTTP(w http.ResponseWriter, req *http.Request) {
	switch req.URL.Path {
	case "/a":
		fmt.Fprint(w, "exec /a")
	case "/b":
		fmt.Fprint(w, "exec /b")
	case "/c":
		fmt.Fprint(w, "exec /c")
	default:
		http.Error(w, "404 don't know what", 404)
	}
}

func main() {
	var r router
	http.ListenAndServe(":8000", &r)
}