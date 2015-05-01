// +build gofuzz

package sereal

func Fuzz(data []byte) int {
	var m interface{}
	if err := Unmarshal(data, &m); err != nil {
		return 0
	}
	return 1
}
