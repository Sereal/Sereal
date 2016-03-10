package com.booking.sereal.benchmarks;

import com.booking.sereal.ByteArray;
import com.booking.sereal.Decoder;
import com.booking.sereal.Encoder;
import com.booking.sereal.SerealException;

import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.runner.Runner;
import org.openjdk.jmh.runner.RunnerException;
import org.openjdk.jmh.runner.options.Options;
import org.openjdk.jmh.runner.options.OptionsBuilder;

import java.io.IOException;
import java.util.Map;
import java.util.HashMap;

public class Sereal {
    private static final Map<String, Object> solarSystem = makeSolarSystem();
    private static final Object[] solarSystems;

    private static ByteArray encodedSmall, encodedLarge;
    private static Encoder encoderSmall = new Encoder();
    private static Encoder encoderLarge = new Encoder();
    private static Decoder decoderSmall = new Decoder();
    private static Decoder decoderLarge = new Decoder();

    private static Map<String, Object> makePlanet(int pos, String name, double earthMasses, String[] notableSatellites) {
        Map<String, Object> planet = new HashMap<String, Object>();

        planet.put("pos", pos);
        planet.put("name", name);
        planet.put("earthMasses", earthMasses);
        planet.put("notableSatellites", notableSatellites);

        return planet;
    }

    private static Map<String, Object> makeSolarSystem() {
        Map<String, Object> solarSystem = new HashMap<String, Object>();

        solarSystem.put("galaxy", "Milky Way");
        solarSystem.put("age", 4568);
        solarSystem.put("stars", new String[] { "Sun" });
        solarSystem.put("planets", new Object[] {
            makePlanet(1, "Mercury", 0.055, new String[0]),
            makePlanet(2, "Venus", 0.815, new String[0]),
            makePlanet(3, "Earth", 1.0, new String[] { "Moon" }),
            makePlanet(4, "Mars", 0.107, new String[] { "Phobos", "Deimos" }),
            makePlanet(5, "Jupiter", 317.83, new String[] { "Io", "Europa", "Ganymede", "Callisto" }),
            makePlanet(6, "Saturn", 95.16, new String[] { "Titan", "Rhea", "Enceladus" }),
            makePlanet(7, "Uranus", 14.536, new String[] { "Oberon", "Titania", "Miranda", "Ariel", "Umbriel" }),
            makePlanet(8, "Neptune", 17.15, new String[] { "Triton" }),
        });

        return solarSystem;
    }

    static {
        solarSystems = new Object[500];
        for (int i = 0; i < solarSystems.length; ++i)
            solarSystems[i] = makeSolarSystem();

        try {
            encodedSmall = new ByteArray(encoderSmall.write(solarSystem).getData());
            encodedLarge = new ByteArray(encoderLarge.write(solarSystems).getData());
        } catch (SerealException e) {
            throw new RuntimeException(e);
        }
    }

    @Benchmark
    public void decodeSmall() throws SerealException, IOException {
        decoderSmall.setData(encodedSmall);
        decoderSmall.decode();
    }

    @Benchmark
    public void encodeSmall() throws SerealException, IOException {
        encoderSmall.write(solarSystem);
    }

    @Benchmark
    public void decodeLarge() throws SerealException, IOException {
        decoderLarge.setData(encodedLarge);
        decoderLarge.decode();
    }

    @Benchmark
    public void encodeLarge() throws SerealException, IOException {
        encoderLarge.write(solarSystems);
    }

    public static void main(String[] args) throws RunnerException {
        Options opt = new OptionsBuilder()
                .include(Sereal.class.getSimpleName())
                .forks(1)
                .build();

        new Runner(opt).run();
    }
}
