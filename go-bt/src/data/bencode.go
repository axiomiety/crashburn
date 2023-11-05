package data

import (
	"bufio"
	"io"
	"log"
	"strconv"
)

func parseInt(data []byte) int {
	i, err := strconv.Atoi(string(data))
	check(err)
	return i
}

func readUntilE(r *bufio.Reader) []byte {
	buff := make([]byte, 4098)
	offset := 0
	for {
		b, err := r.ReadByte()
		check(err)
		switch b {
		case 'e':
			return buff[:offset]
		default:
			buff[offset] = b
			offset += 1
		}
	}
}

type Container struct {
	List []interface{}
	Dict map[string]interface{}
}

func (c *Container) add(value interface{}) {
	c.List = append(c.List, value)
}

type Curried func(val interface{}) interface{}

/*
would some sort of stack-based approach work best?
*/
func ParseBencoded(r io.Reader) interface{} {
	reader := bufio.NewReader(r)
	var container interface{}

	parse = func(reader *bufio.Reader) interface{} {
		b, err := reader.ReadByte()
		if err != nil {
			log.Printf("end reached")
		}
		switch b {
		case 'e':
		case 'i':
			val, err := strconv.Atoi(string(readUntilE(reader)))
			check(err)

		case 'l':

		}
	}
	return container
}
