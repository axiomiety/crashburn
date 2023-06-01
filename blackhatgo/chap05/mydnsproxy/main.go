package main

import (
	"github.com/miekg/dns"
	"log"
	"net"
)

func forward(w dns.ResponseWriter, req *dns.Msg) {
	var serverAddr = "8.8.8.8:53"
	log.Printf("%+v\n", req)
	resp, err := dns.Exchange(req, serverAddr)
	if err != nil {
		dns.HandleFailed(w, req)
		return
	}
	if err := w.WriteMsg(resp); err != nil {
		dns.HandleFailed(w, req)
		return
	}
}

func localproxy(w dns.ResponseWriter, req *dns.Msg) {
	var resp dns.Msg
	resp.SetReply(req)
	for _, q := range req.Question {
		log.Printf("sinkholing req for %s", q.Name)
		a := dns.A{
			Hdr: dns.RR_Header{
				Name:   q.Name,
				Rrtype: dns.TypeA,
				Class:  dns.ClassINET,
				Ttl:    10,
			},
			A: net.ParseIP("127.0.0.1").To4(),
		}
		resp.Answer = append(resp.Answer, &a)
	}
	w.WriteMsg(&resp)
}

func main() {

	dns.HandleFunc(".", forward)
	dns.HandleFunc("facebook.com", forward)
	dns.HandleFunc("abc.facebook.com", localproxy)
	log.Fatal(dns.ListenAndServe(":53", "udp", nil))
}
