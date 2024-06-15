package main

import (
	"context"
	"encoding/json"
	"flag"
	"golang.org/x/time/rate"
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
	var reqsPerSecond = flag.Int("r", 15, "number of requests per seconds")
	flag.Parse()
	ctx, cancel := context.WithTimeout(context.Background(), time.Duration(*timeout)*time.Second)
	defer cancel()

	var wg sync.WaitGroup
    limiter := rate.NewLimiter(rate.Limit(*reqsPerSecond),1)
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
					if err := limiter.Wait(ctx); err != nil {
                        // if we get there, waiting would exceed the context's deadline
                        // so let's assume we're done
                        break
					}
					//log.Printf("worker[%d] fetching URL\n", idx)
					res, _ := client.Do(req)
                    countChan <- struct{}{}
					body, _ := ioutil.ReadAll(res.Body)
					defer res.Body.Close()
                    // we completed a request! let the counter know
					var data map[string]any
					_ = json.Unmarshal(body, &data)
					if val, ok := data["message"]; ok {
						log.Println(val)
					}
				}

			}
			log.Printf("coroutine exiting...")
		}(ctx, i, countChan)
	}
	wg.Wait()
	log.Println("done")
}
