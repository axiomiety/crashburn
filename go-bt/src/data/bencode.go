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

func ParseBencoded(r io.Reader) {
	reader := bufio.NewReader(r)
	for {
		b, err := reader.ReadByte()
		check(err)
		switch b {
		case 'i':
			val, err := strconv.Atoi(string(readUntilE(reader)))
			check(err)
			log.Printf("found int: %d", val)
			break
		}
		break
	}
}
