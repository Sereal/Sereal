package sereal_test

import (
	"testing"

	"github.com/Sereal/Sereal/Go/sereal"
)

var solarSystemMeta = map[string]interface{}{
	"title": "Interesting facts about Solar system",
}

var solarSystem = map[string]interface{}{
	"galaxy": "Milky Way",
	"age":    4568,
	"stars":  []string{"Sun"},
	"planets": []struct {
		pos                int
		name               string
		mass_earths        float64
		notable_satellites []string
	}{
		{1, "Mercury", 0.055, []string{}},
		{2, "Venus", 0.815, []string{}},
		{3, "Earth", 1.0, []string{"Moon"}},
		{4, "Mars", 0.107, []string{"Phobos", "Deimos"}},
		{5, "Jupiter", 317.83, []string{"Io", "Europa", "Ganymede", "Callisto"}},
		{6, "Saturn", 95.16, []string{"Titan", "Rhea", "Enceladus"}},
		{7, "Uranus", 14.536, []string{"Oberon", "Titania", "Miranda", "Ariel", "Umbriel"}},
		{8, "Neptune", 17.15, []string{"Tritan"}},
	},
}

func BenchmarkEncodeComplexDataWithHeader(b *testing.B) {
	enc := sereal.NewEncoderV3()

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_, err := enc.MarshalWithHeader(solarSystemMeta, solarSystem)
		if err != nil {
			b.FailNow()
		}
	}
}

func BenchmarkEncodeAndSnappyComplexDataWithHeader(b *testing.B) {
	enc := sereal.NewEncoderV3()
	enc.Compression = sereal.SnappyCompressor{Incremental: true}
	enc.CompressionThreshold = 0

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_, err := enc.MarshalWithHeader(solarSystemMeta, solarSystem)
		if err != nil {
			b.FailNow()
		}
	}
}

func BenchmarkEncodeAndZlibComplexDataWithHeader(b *testing.B) {
	enc := sereal.NewEncoderV3()
	enc.Compression = sereal.ZlibCompressor{Level: sereal.ZlibDefaultCompression}
	enc.CompressionThreshold = 0

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_, err := enc.MarshalWithHeader(solarSystemMeta, solarSystem)
		if err != nil {
			b.FailNow()
		}
	}
}

type benchPlanet struct {
	Pos               int
	Name              string
	MassEarths        float64
	NotableSatellites []string
}

type benchSystem struct {
	Galaxy  string
	Age     int
	Stars   []string
	Planets []benchPlanet
}

var benchSolarSystem = benchSystem{
	Galaxy: "Milky Way",
	Age:    4568,
	Stars:  []string{"Sun"},
	Planets: []benchPlanet{
		{1, "Mercury", 0.055, []string{}},
		{2, "Venus", 0.815, []string{}},
		{3, "Earth", 1.0, []string{"Moon"}},
		{4, "Mars", 0.107, []string{"Phobos", "Deimos"}},
		{5, "Jupiter", 317.83, []string{"Io", "Europa", "Ganymede", "Callisto"}},
		{6, "Saturn", 95.16, []string{"Titan", "Rhea", "Enceladus"}},
		{7, "Uranus", 14.536, []string{"Oberon", "Titania", "Miranda", "Ariel", "Umbriel"}},
		{8, "Neptune", 17.15, []string{"Tritan"}},
	},
}

func benchSolarSystemDoc(b *testing.B) []byte {
	doc, err := sereal.NewEncoderV3().Marshal(benchSolarSystem)
	if err != nil {
		b.Fatal(err)
	}
	return doc
}

func BenchmarkDecodeComplexDataInterface(b *testing.B) {
	doc := benchSolarSystemDoc(b)
	dec := sereal.NewDecoder()

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		var v interface{}
		if err := dec.Unmarshal(doc, &v); err != nil {
			b.Fatal(err)
		}
	}
}

func BenchmarkDecodeComplexDataStruct(b *testing.B) {
	doc := benchSolarSystemDoc(b)
	dec := sereal.NewDecoder()

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		var v benchSystem
		if err := dec.Unmarshal(doc, &v); err != nil {
			b.Fatal(err)
		}
		if len(v.Planets) != 8 {
			b.Fatalf("decoded %d planets", len(v.Planets))
		}
	}
}
