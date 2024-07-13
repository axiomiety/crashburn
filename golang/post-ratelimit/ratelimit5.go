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

func rateLogger(ctx context.Context, limit *rate.Limiter, countChan <-chan struct{}) {
    //ticker := time.NewTicker(500*time.Millisecond)
    ticker := time.NewTicker(1*time.Second)
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
            tokens := limit.Tokens()
            timeToNextToken := tokens/-10 // hardcoded
            log.Printf("requests/second: %d, tokens available: %.2f, time to next token: %.2f, elapsed: %s ", count, tokens, timeToNextToken, intervalEnd.Sub(intervalStart))
            intervalStart = intervalEnd
            count = 0
        }
    }
}

func main() {
	var numWorkers = flag.Int("w", 20, "number of workers")
	var timeout = flag.Int("t", 5, "timeout, in seconds")
	var reqsPerSecond = flag.Int("r", 10, "number of requests per seconds")
	flag.Parse()
	ctx, cancel := context.WithTimeout(context.Background(), time.Duration(*timeout)*time.Second)
	defer cancel()

	var wg sync.WaitGroup
    limiter := rate.NewLimiter(rate.Limit(*reqsPerSecond),15)
    countChan := make(chan struct{}, 1000)
    go rateLogger(ctx, limiter, countChan)

    for i := 0; i < *numWorkers; i++ {

		wg.Add(1)
		go func(ctx context.Context, idx int, countChan chan<- struct{}) {
			defer wg.Done()
            //time.Sleep(10*time.Second)
			client := &http.Client{}
			req, _ := http.NewRequest("GET", "https://api.exchange.coinbase.com/products/ETH-USD/book?level=2", nil)
			req.Header.Add("Content-Type", "application/json")

			for {
				select {
				case <-ctx.Done():
					return
				default:
                    beforeWait := time.Now()
					if err := limiter.Wait(ctx); err != nil {
                        // if we get there, waiting would exceed the context's deadline
                        // so let's assume we're done
                        break
					}
                    afterWait := time.Now()
                    waitTime := afterWait.Sub(beforeWait)
                    if waitTime > 1*time.Second {
						log.Printf("[worker %d] waited %s for a token", idx, waitTime)
                    }
					res, _ := client.Do(req)
                    countChan <- struct{}{}
					body, _ := ioutil.ReadAll(res.Body)
					defer res.Body.Close()
                    // we completed a request! let the counter know
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
