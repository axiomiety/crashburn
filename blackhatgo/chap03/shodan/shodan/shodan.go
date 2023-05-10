package shodan

const BaseURL = "https://api.shodan.io"

type Client struct {
	apiKey string
}

type New(apiKey string) *Client {
	return &Client{apiKey: apiKey}
}