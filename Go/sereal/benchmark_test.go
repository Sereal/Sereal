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
		Pos               int
		Name              string
		MassEarths        float64
		NotableSatellites []string `sereal:"NotableSatellites,omitempty"`
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
	enc.StructAsMap = false

	data, err := enc.MarshalWithHeader(solarSystemMeta, solarSystem)
	if err != nil {
		b.Log(err)
		b.FailNow()
	}

	b.ResetTimer()
	b.ReportMetric(float64(len(data)), "bytes")
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
	enc.StructAsMap = false

	data, err := enc.MarshalWithHeader(solarSystemMeta, solarSystem)
	if err != nil {
		b.FailNow()
	}

	b.ResetTimer()
	b.ReportMetric(float64(len(data)), "bytes")
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
	enc.StructAsMap = false

	data, err := enc.MarshalWithHeader(solarSystemMeta, solarSystem)
	if err != nil {
		b.FailNow()
	}

	b.ResetTimer()
	b.ReportMetric(float64(len(data)), "bytes")
	for i := 0; i < b.N; i++ {
		_, err := enc.MarshalWithHeader(solarSystemMeta, solarSystem)
		if err != nil {
			b.FailNow()
		}
	}
}
