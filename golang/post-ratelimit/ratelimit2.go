package main

import (
	"context"
	"encoding/json"
	"flag"
	"io/ioutil"
	"log"
	"net/http"
	"sync"
	"time"
)

func rateLogger(ctx context.Context, countChan <-chan struct{}) {
    ticker := time.NewTicker(time.Second)
    count := 0
    defer ticker.Stop()
    intervalStart := time.Now()
    for {
        select {
        case <- ctx.Done():
            break
        case <- countChan:
            count += 1
        case intervalEnd := <- ticker.C:
            log.Printf("requests/second: %d, elapsed: %s ", count, intervalEnd.Sub(intervalStart))
            intervalStart = intervalEnd
            count = 0
        }
    }
}

func main() {
	var numWorkers = flag.Int("w", 20, "number of workers")
	var timeout = flag.Int("t", 5, "timeout, in seconds")
	flag.Parse()
	ctx, cancel := context.WithTimeout(context.Background(), time.Duration(*timeout)*time.Second)
	defer cancel()

	var wg sync.WaitGroup
    countChan := make(chan struct{}, 100)
    go rateLogger(ctx, countChan)
    for i := 0; i < *numWorkers; i++ {

		wg.Add(1)
		go func(ctx context.Context, idx int, countChan chan<- struct{}) {
			defer wg.Done()
			client := &http.Client{}
			req, _ := http.NewRequest("GET", "https://api.exchange.coinbase.com/products/ETH-USD/book?level=2", nil)
			req.Header.Add("Content-Type", "application/json")

			for {
				select {
				case <-ctx.Done():
					return
				default:
					res, _ := client.Do(req)
                    // we completed a request! let the counter know before we even
                    // start to parse the contents
                    countChan <- struct{}{}
					body, _ := ioutil.ReadAll(res.Body)
					defer res.Body.Close()
					var data map[string]any
					_ = json.Unmarshal(body, &data)
					if val, ok := data["message"]; ok {
						log.Printf("[worker %d] %s", idx, val)
					}
				}

			}
			log.Printf("coroutine exiting...")
		}(ctx, i, countChan)
	}
	wg.Wait()
	log.Println("done")
}
