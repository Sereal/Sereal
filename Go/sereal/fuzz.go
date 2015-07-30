// +build gofuzz

package sereal

func Fuzz(data []byte) int {
	var m interface{}

	header, err := readHeader(data)
	if err != nil {
		return 0
	}

	bodyStart := headerSize + header.suffixSize

	if bodyStart > len(data) || bodyStart < 0 {
		return 0
	}

	switch header.version {
	case 1, 2, 3:
		break
	default:
		return 0
	}

	switch header.doctype {
	case serealRaw:
		break
	case serealSnappy, serealSnappyIncremental, serealZlib:
		// ignore compressed data
		return 0
	}

	if err := Unmarshal(data, &m); err != nil {
		return 0
	}

	return 1
}
