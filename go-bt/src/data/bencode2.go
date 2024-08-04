package data

import (
	"bufio"
	"bytes"
	"fmt"
	"io"
	"log"
	"reflect"
	"slices"
	"strconv"
)

// this is the interface that various containers implement

type Holder interface {
	Add(value any)
	Obj() any
}

type ListHolder struct {
	List []any
}

type DictHolder struct {
	Dict map[string]any
	// tracks the current key
	Key string
}

type ValueHolder struct {
	Val any
}

// Add methods

func (c *ListHolder) Add(value any) {
	c.List = append(c.List, value)
}

func (c *DictHolder) Add(value any) {
	if c.Key == "" {
		c.Key = value.(string)
	} else {
		c.Dict[c.Key] = value
		// reset
		c.Key = ""
	}
}

func (c *ValueHolder) Add(value any) {
	c.Val = value
}

// Obj methods

func (c *ListHolder) Obj() any {
	return c.List
}

func (c *DictHolder) Obj() any {
	return c.Dict
}

func (c *ValueHolder) Obj() any {
	return c.Val
}

// now let's do the actual parsing
func parseBencodeStream(container Holder, reader *bufio.Reader) Holder {
	b, err := reader.ReadByte()
	if err != nil {
		return container
	}
	switch b {
	case 'e':
		return container
	case 'i':
		buff, err := reader.ReadBytes('e')
		check(err)
		val, err := strconv.Atoi(string(buff[:len(buff)-1]))
		check(err)
		container.Add(val)
		return parseBencodeStream(container, reader)
	case 'l':
		c := parseBencodeStream(&ListHolder{List: make([]any, 0)}, reader)
		container.Add(c.(*ListHolder).List)
		return parseBencodeStream(container, reader)
	case 'd':
		c := parseBencodeStream(&DictHolder{Dict: make(map[string]any)}, reader)
		container.Add(c.(*DictHolder).Dict)
		return parseBencodeStream(container, reader)
	case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
		buff, err := reader.ReadBytes(':')
		check(err)
		strLen := string(b)
		if len(buff) > 1 {
			strLen += string(buff[:len(buff)-1])
		}
		strLenInt, err := strconv.Atoi(strLen)
		check(err)
		val := make([]byte, strLenInt)
		for i := 0; i < strLenInt; i++ {
			b, err = reader.ReadByte()
			check(err)
			val[i] = b
		}
		container.Add(string(val[:]))
		return parseBencodeStream(container, reader)
	}
	return container
}

func ParseBencoded2(r io.Reader) any {
	reader := bufio.NewReader(r)

	// kick this off by passing an empty holder
	container := parseBencodeStream(&ValueHolder{}, reader)
	return container.Obj()
}

func fillStruct(o any, d map[string]any) {
	var structure reflect.Type
	if reflect.TypeOf(o).Kind() != reflect.Struct {
		structure = reflect.TypeOf(o).Elem()
	} else {
		structure = reflect.TypeOf(o)
	}
	for i := 0; i < structure.NumField(); i++ {
		f := structure.Field(i)
		tag := f.Tag.Get("bencode")

		if val, ok := d[tag]; ok {
			if f.Type.Kind() == reflect.Slice {
				s := reflect.ValueOf(val)
				valueSlice := reflect.MakeSlice(reflect.SliceOf(f.Type.Elem()), s.Len(), s.Len())
				reflect.ValueOf(o).Elem().Field(i).Set(valueSlice)
				for i := 0; i < s.Len(); i++ {
					item := reflect.New(f.Type.Elem())
					fillStruct(item.Interface(), s.Index(i).Interface().(map[string]any))
					valueSlice.Index(i).Set(item.Elem())
				}
			} else if f.Type.Kind() != reflect.Struct {
				bindat := reflect.ValueOf(val).Convert(f.Type)
				reflect.ValueOf(o).Elem().Field(i).Set(bindat)
			} else {
				oo := reflect.New(f.Type)
				fillStruct(oo.Interface(), val.(map[string]any))
				reflect.ValueOf(o).Elem().Field(i).Set(oo.Elem())
			}
		}
	}
}

func ParseFromReader[S BETorrent | BETrackerResponse](r io.Reader) *S {
	obj := ParseBencoded2(r)
	d, ok := obj.(map[string]any)
	if !ok {
		panic("Unable to parse torrent")
	}
	var s S
	fillStruct(&s, d)
	return &s
}

func Encode(buffer *bytes.Buffer, o any) {
	value := reflect.ValueOf(o)
	log.Printf("found %s for %s", value.Kind(), o)
	switch value.Kind() {
	case reflect.Int, reflect.Int16, reflect.Int32, reflect.Int64:
		buffer.WriteByte('i')
		buffer.WriteString(strconv.Itoa(int(value.Int())))
		buffer.WriteByte('e')
	case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64:
		buffer.WriteByte('i')
		buffer.WriteString(strconv.Itoa(int(value.Uint())))
		buffer.WriteByte('e')
	case reflect.String:
		buffer.WriteString(strconv.Itoa(len(value.Interface().(string))))
		buffer.WriteString(":")
		buffer.WriteString(value.Interface().(string))
	case reflect.Slice:
		buffer.WriteByte('l')
		// so this is a bit funky - we can't convert e.g. []int to []any
		// directly
		temp := make([]any, value.Len())

		for i := 0; i < value.Len(); i++ {
			temp[i] = value.Index(i).Interface()
		}
		for _, val := range temp {
			Encode(buffer, val)
		}
		buffer.WriteByte('e')
	case reflect.Map:
		buffer.WriteByte('d')
		temp := make(map[string]any, value.Len())

		// we need a map[string]any
		iter := value.MapRange()
		for iter.Next() {
			k := iter.Key()
			v := iter.Value()
			temp[k.Interface().(string)] = v.Interface()
		}

		// keys need to be sorted alphabetically
		keys := make([]string, 0, len(temp))
		for k := range temp {
			keys = append(keys, k)
		}
		slices.Sort(keys)

		for _, key := range keys {
			// first we write the key
			Encode(buffer, key)
			// then the value
			Encode(buffer, temp[key])
		}
		buffer.WriteByte('e')
	default:
		panic(fmt.Sprintf("can't handle type %s", value.Kind()))
	}
}
