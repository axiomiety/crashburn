package main

import (
	"github.com/gorilla/mux"
	"log"
	"net/http"
	"net/http/httputil"
	"net/url"
)

var (
	hostProxy = make(map[string]string)
	proxies   = make(map[string]*httputil.ReverseProxy)
)

func init() {
	hostProxy["domain1.com"] = "http://0.0.0.0:10080"
	hostProxy["domain2.com"] = "http://0.0.0.0:10090"

	for k, v := range hostProxy {
		remote, err := url.Parse(v)
		if err != nil {
			log.Fatal("unable to parse proxy target")
		}

		proxies[k] = httputil.NewSingleHostReverseProxy(remote)
	}
}

func main() {
	r := mux.NewRouter()
	for host, proxy := range proxies {
		r.Host(host).Handler(proxy)
	}

	log.Fatal(http.ListenAndServe(":80", r))
}
