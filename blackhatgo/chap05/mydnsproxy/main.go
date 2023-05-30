package main

import (
	"github.com/miekg/dns"
	"log"
)

func main() {
	var serverAddr = "8.8.8.8:53"
	dns.HandleFunc(".", func(w dns.ResponseWriter, req *dns.Msg) {
		log.Printf("%+v\n",req)
		resp, err := dns.Exchange(req, serverAddr)
		if err != nil {
			dns.HandleFailed(w, req)
			return
		}
		if err := w.WriteMsg(resp); err != nil {
			dns.HandleFailed(w, req)
			return
		}

	})
	log.Fatal(dns.ListenAndServe(":53", "udp", nil))
}
