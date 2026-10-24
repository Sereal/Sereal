package com.booking.sereal;

import static org.junit.Assert.fail;
import static org.junit.Assume.assumeTrue;

import java.io.IOException;
import java.lang.ref.WeakReference;
import java.math.BigInteger;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.regex.Pattern;
import org.junit.Test;

/**
 * Deterministic mutation fuzzer for the decoders.
 *
 * <p>Seeds are {@link Encoder} outputs for protocol versions 1-6 with every compression and each
 * checksum type, plus a few hand-written bodies. Each mutated packet is decoded with several
 * {@link DecoderOptions} variants by {@link Decoder} and drained by {@link TokenDecoder}. A
 * {@link SerealException} is the documented failure mode; any other {@link Throwable}, a token
 * slice outside the decoder buffer, or a single decode slower than 2 seconds is recorded (with
 * the packet) and fails the test at the end.
 *
 * <p>Disabled unless {@code -Dsereal.fuzz.iterations=N} is given. Other properties:
 * {@code sereal.fuzz.seconds} (wall clock bound, default 300), {@code sereal.fuzz.seed}
 * (default 1), {@code sereal.fuzz.repro} (directory for reproducer packets). Run with a small
 * heap so that huge allocations fail fast, e.g.
 * {@code mvn -B -pl sereal test -Dtest=MalformedInputFuzzTest -DargLine=-Xmx512m
 * -Dsereal.fuzz.iterations=300000 -Dsereal.fuzz.repro=/some/dir}.
 */
public class MalformedInputFuzzTest {
  private static final long SLOW_NANOS = TimeUnit.SECONDS.toNanos(2);
  private static final byte[] KEY0 = "0123456789abcdef".getBytes(StandardCharsets.ISO_8859_1);
  private static final byte[] KEY1 = "fedcba9876543210".getBytes(StandardCharsets.ISO_8859_1);
  private static final byte[] KEY2 = "Sereal fuzz key!".getBytes(StandardCharsets.ISO_8859_1);
  private static final long[] INTERESTING = {
    0, 1, 2, 0x7f, 0x80, 0xff, 0x100, 0x7fff, 0xffff, 1 << 20, 100 * 1024 * 1024 + 1,
    0x7ffffff0L, 0x7fffffffL, 0x80000000L, 0xfffffffeL, 0xffffffffL, 0x100000000L,
    Long.MAX_VALUE, -1L, -2L
  };

  private static final class Finding {
    final String key;
    final String detail;
    byte[] packet;
    int count;

    Finding(String key, String detail, byte[] packet) {
      this.key = key;
      this.detail = detail;
      this.packet = packet;
    }
  }

  private final Map<String, Finding> findings = new LinkedHashMap<>();
  private Path reproDir;
  private ExecutorService executor;
  private int abandonedThreads;

  @Test
  public void fuzzDecoders() throws Exception {
    long iterations = Long.getLong("sereal.fuzz.iterations", 0);
    assumeTrue("set -Dsereal.fuzz.iterations=N to run the fuzzer", iterations > 0);
    long seconds = Long.getLong("sereal.fuzz.seconds", 300);
    long seed = Long.getLong("sereal.fuzz.seed", 1);
    String repro = System.getProperty("sereal.fuzz.repro");
    if (repro != null && !repro.isEmpty()) {
      reproDir = Paths.get(repro);
      Files.createDirectories(reproDir);
    }

    List<byte[]> seeds = buildSeeds();
    Random rnd = new Random(seed);
    executor = newExecutor();
    long deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(seconds);
    long done = 0;
    for (; done < iterations && System.nanoTime() < deadline; done++) {
      byte[] base = seeds.get(rnd.nextInt(seeds.size()));
      byte[] packet = mutate(base, seeds, rnd);
      runGuarded(packet);
      if (abandonedThreads > 4) {
        break;
      }
    }
    executor.shutdownNow();

    // minimise each unique finding (same key), bounded
    StringBuilder report = new StringBuilder();
    for (Finding f : findings.values()) {
      byte[] min = minimise(f.key, f.packet);
      f.packet = min;
      save(f, "-min");
      report.append("\n  [").append(f.count).append("x] ").append(f.key).append("\n    ")
          .append(f.detail.replace("\n", "\n    ")).append("\n    min packet: ")
          .append(hex(min));
    }
    System.out.println("malformed input fuzz: " + done + " iterations, " + seeds.size()
        + " seeds, " + findings.size() + " unique findings");
    if (!findings.isEmpty()) {
      fail("decoders misbehaved on " + findings.size() + " unique malformed inputs:" + report);
    }
  }

  // ---- execution -------------------------------------------------------------------------

  private static ExecutorService newExecutor() {
    return Executors.newSingleThreadExecutor(r -> {
      Thread t = new Thread(null, r, "sereal-fuzz", 8L << 20);
      t.setDaemon(true);
      return t;
    });
  }

  private void runGuarded(byte[] packet) throws Exception {
    if (reproDir != null && packet.length > 4 && ((packet[4] >> 4) & 0x07) != 0) {
      // compressed documents reach native code: keep the packet in case the JVM dies
      Files.write(reproDir.resolve("last-native-packet.srl"), packet);
    }
    Future<List<String[]>> fut = executor.submit(() -> decodeAll(packet));
    List<String[]> problems;
    try {
      problems = fut.get(20, TimeUnit.SECONDS);
    } catch (TimeoutException e) {
      fut.cancel(true);
      executor.shutdownNow();
      abandonedThreads++;
      executor = newExecutor();
      problems = new ArrayList<>();
      problems.add(new String[] {"any: hang (no result after 20s)", "decode did not finish"});
    }
    for (String[] p : problems) {
      record(p[0], p[1], packet);
    }
  }

  /** Returns a list of {key, detail} for each misbehaviour; empty when everything was fine. */
  private static List<String[]> decodeAll(byte[] packet) {
    List<String[]> out = new ArrayList<>();
    DecoderOptions[] variants = decoderVariants();
    for (int i = 0; i < variants.length; i++) {
      final DecoderOptions o = variants[i];
      check(out, "Decoder[v" + i + "]", () -> {
        Decoder d = new Decoder(o);
        d.setData(packet);
        if (d.hasHeader()) {
          d.decodeHeader();
        }
        d.decode();
        d.close();
      });
    }
    check(out, "TokenDecoder", () -> drainTokens(packet, variants[0]));
    check(out, "TokenDecoder[cksum]", () -> drainTokens(packet, variants[5]));
    return out;
  }

  private interface Body {
    void run() throws Exception;
  }

  private static void check(List<String[]> out, String api, Body body) {
    long t0 = System.nanoTime();
    try {
      body.run();
    } catch (SerealException expected) {
      // documented failure mode
    } catch (Throwable t) {
      out.add(new String[] {key(api, t), describe(t)});
    }
    long elapsed = System.nanoTime() - t0;
    if (elapsed > SLOW_NANOS) {
      out.add(new String[] {api + ": slow decode",
          "single decode took " + TimeUnit.NANOSECONDS.toMillis(elapsed) + "ms"});
    }
  }

  private static void drainTokens(byte[] packet, DecoderOptions options) throws Exception {
    TokenDecoder t = new TokenDecoder(options);
    t.setData(packet);
    if (t.hasHeader()) {
      t.prepareDecodeHeader();
      drain(t);
    }
    t.prepareDecodeBody();
    drain(t);
    t.close();
  }

  private static void drain(TokenDecoder t) throws SerealException {
    SerealToken tok;
    long n = 0;
    while ((tok = t.nextToken()) != SerealToken.END) {
      if (++n > 50_000_000L) {
        throw new IllegalStateException("more than 50M tokens");
      }
      switch (tok) {
        case BINARY:
        case UTF8:
        case FLOAT128:
        case VSTRING:
          checkSlice(t.decoderBuffer(), t.binarySliceStart(), t.binarySliceEnd(), tok);
          break;
        case REGEXP:
          checkSlice(t.decoderBuffer(), t.binarySliceStart(), t.binarySliceEnd(), tok);
          checkSlice(t.decoderBuffer(), t.regexpFlagsSliceStart(), t.regexpFlagsSliceEnd(), tok);
          break;
        default:
          break;
      }
    }
  }

  /** Raised (and recorded) when a token slice points outside the decoder buffer. */
  static final class SliceOutOfBounds extends RuntimeException {
    SliceOutOfBounds(String msg) {
      super(msg);
    }
  }

  private static void checkSlice(byte[] buf, int start, int end, SerealToken tok) {
    if (start < 0 || end < start || end > buf.length) {
      throw new SliceOutOfBounds(tok + " slice [" + start + ", " + end + ") outside buffer of "
          + buf.length + " bytes");
    }
  }

  private static DecoderOptions[] decoderVariants() {
    java.util.function.Consumer<String> quiet = s -> { };
    return new DecoderOptions[] {
      new DecoderOptions().checksumWarningHandler(quiet),
      new DecoderOptions().perlReferences(true).perlAliases(true).preserveUndef(true)
          .checksumWarningHandler(quiet),
      new DecoderOptions().refuseObjects(true).checksumWarningHandler(quiet),
      new DecoderOptions().stripObjects(true).preferLatin1(true).checksumWarningHandler(quiet),
      new DecoderOptions().forceJavaStringForByteArrayValues(true).perlReferences(true)
          .checksumWarningHandler(quiet),
      new DecoderOptions().checksumV0Key(KEY0).checksumV1Key(KEY1).checksumV2Key(KEY2)
          .checksumWarningHandler(quiet),
      new DecoderOptions().checksumPolicy(SerealChecksum.Policy.IGNORE)
          .checksumWarningHandler(quiet),
      new DecoderOptions().maxNumArrayEntries(1000).maxNumMapEntries(1000).maxStringLength(1000)
          .maxBufferSize(1 << 20).maxRecursionDepth(100).checksumWarningHandler(quiet),
    };
  }

  private static String key(String api, Throwable t) {
    String frame = "?";
    for (StackTraceElement e : t.getStackTrace()) {
      if (e.getClassName().startsWith("com.booking.sereal")
          && !e.getClassName().startsWith(MalformedInputFuzzTest.class.getName())) {
        frame = e.getClassName().substring("com.booking.sereal.".length()) + "." + e.getMethodName()
            + ":" + e.getLineNumber();
        break;
      }
    }
    if (t instanceof SliceOutOfBounds) {
      frame = "token slice";
    }
    return api.replaceAll("\\[v\\d+\\]", "") + ": " + t.getClass().getName() + " @ " + frame;
  }

  private static String describe(Throwable t) {
    StringBuilder sb = new StringBuilder();
    sb.append(t.getClass().getName()).append(": ").append(t.getMessage());
    StackTraceElement[] st = t.getStackTrace();
    for (int i = 0; i < Math.min(5, st.length); i++) {
      sb.append("\n  at ").append(st[i]);
    }
    return sb.toString();
  }

  private void record(String key, String detail, byte[] packet) {
    Finding f = findings.get(key);
    if (f == null) {
      f = new Finding(key, detail, packet.clone());
      findings.put(key, f);
      save(f, "");
    } else if (packet.length < f.packet.length) {
      f.packet = packet.clone();
    }
    f.count++;
  }

  private void save(Finding f, String suffix) {
    if (reproDir == null) {
      return;
    }
    String name = "fuzz-" + f.key.replaceAll("[^A-Za-z0-9.]+", "_");
    if (name.length() > 150) {
      name = name.substring(0, 150);
    }
    try {
      Files.write(reproDir.resolve(name + suffix + ".srl"), f.packet);
      Files.write(reproDir.resolve(name + suffix + ".txt"),
          (f.key + "\n" + f.detail + "\n" + hex(f.packet) + "\n").getBytes(StandardCharsets.UTF_8));
    } catch (IOException e) {
      throw new RuntimeException(e);
    }
  }

  // ---- minimisation ----------------------------------------------------------------------

  private byte[] minimise(String key, byte[] packet) throws Exception {
    byte[] cur = packet;
    int budget = 1500;
    for (int chunk = Math.max(1, cur.length / 2); chunk >= 1 && budget > 0; chunk /= 2) {
      boolean progress = true;
      while (progress && budget > 0) {
        progress = false;
        for (int i = 0; i + chunk <= cur.length && budget > 0; ) {
          byte[] cand = new byte[cur.length - chunk];
          System.arraycopy(cur, 0, cand, 0, i);
          System.arraycopy(cur, i + chunk, cand, i, cur.length - i - chunk);
          budget--;
          if (reproduces(key, cand)) {
            cur = cand;
            progress = true;
          } else {
            i += chunk;
          }
        }
      }
    }
    return cur;
  }

  private boolean reproduces(String key, byte[] packet) throws Exception {
    if (key.contains("hang") || key.contains("slow")) {
      return false;
    }
    if (executor == null || executor.isShutdown()) {
      executor = newExecutor();
    }
    Future<List<String[]>> fut = executor.submit(() -> decodeAll(packet));
    try {
      for (String[] p : fut.get(20, TimeUnit.SECONDS)) {
        if (p[0].equals(key)) {
          return true;
        }
      }
    } catch (TimeoutException e) {
      fut.cancel(true);
      executor.shutdownNow();
      executor = null;
    }
    return false;
  }

  // ---- mutation --------------------------------------------------------------------------

  private static byte[] mutate(byte[] base, List<byte[]> seeds, Random rnd) {
    byte[] b = base.clone();
    int n = 1 + rnd.nextInt(4);
    for (int k = 0; k < n; k++) {
      if (b.length == 0) {
        b = new byte[] {(byte) rnd.nextInt(256)};
      }
      int pos = rnd.nextInt(b.length);
      switch (rnd.nextInt(11)) {
        case 0: // bit flip
          b[pos] ^= (byte) (1 << rnd.nextInt(8));
          break;
        case 1: // random byte
          b[pos] = (byte) rnd.nextInt(256);
          break;
        case 2: { // interesting byte
          int[] vals = {0x00, 0x01, 0x7f, 0x80, 0xff, 0x3f, 0x28, 0x29, 0x2e, 0x2f, 0x30, 0x31};
          b[pos] = (byte) vals[rnd.nextInt(vals.length)];
          break;
        }
        case 3: // truncate
          b = Arrays.copyOf(b, rnd.nextInt(b.length + 1));
          break;
        case 4: { // insert random bytes
          int len = 1 + rnd.nextInt(8);
          byte[] ins = new byte[len];
          rnd.nextBytes(ins);
          b = splice(b, pos, 0, ins);
          break;
        }
        case 5: { // delete a range
          int len = 1 + rnd.nextInt(Math.min(16, b.length - pos));
          b = splice(b, pos, len, new byte[0]);
          break;
        }
        case 6: { // tag substitution, optionally with the track flag
          int tag = 0x20 + rnd.nextInt(0x60);
          if (rnd.nextInt(4) == 0) {
            tag |= 0x80;
          }
          b[pos] = (byte) tag;
          break;
        }
        case 7: { // replace a varint (or byte) with an interesting varint
          int old = 1;
          while (pos + old - 1 < b.length && (b[pos + old - 1] & 0x80) != 0 && old < 10) {
            old++;
          }
          old = Math.min(old, b.length - pos);
          b = splice(b, pos, old, varint(INTERESTING[rnd.nextInt(INTERESTING.length)]));
          break;
        }
        case 8: { // declared size bump right after the header (compressed lengths live there)
          int at = Math.min(b.length, b.length > 5 && b[0] == '=' ? 6 : pos);
          int old = 0;
          while (at + old < b.length && (b[at + old] & 0x80) != 0 && old < 9) {
            old++;
          }
          old = Math.min(old + 1, b.length - at);
          b = splice(b, at, old, varint(INTERESTING[rnd.nextInt(INTERESTING.length)]));
          break;
        }
        case 9: { // duplicate a chunk
          int len = 1 + rnd.nextInt(Math.min(32, b.length - pos));
          b = splice(b, rnd.nextInt(b.length + 1), 0, Arrays.copyOfRange(b, pos, pos + len));
          break;
        }
        default: { // splice in a chunk of another seed
          byte[] other = seeds.get(rnd.nextInt(seeds.size()));
          int from = rnd.nextInt(other.length);
          int len = 1 + rnd.nextInt(Math.min(32, other.length - from));
          b = splice(b, pos, rnd.nextInt(Math.min(8, b.length - pos) + 1),
              Arrays.copyOfRange(other, from, from + len));
          break;
        }
      }
    }
    return b;
  }

  private static byte[] splice(byte[] b, int pos, int remove, byte[] insert) {
    byte[] out = new byte[b.length - remove + insert.length];
    System.arraycopy(b, 0, out, 0, pos);
    System.arraycopy(insert, 0, out, pos, insert.length);
    System.arraycopy(b, pos + remove, out, pos + insert.length, b.length - pos - remove);
    return out;
  }

  private static byte[] varint(long n) {
    byte[] buf = new byte[10];
    int i = 0;
    while (Long.compareUnsigned(n, 0x7f) > 0) {
      buf[i++] = (byte) ((n & 0x7f) | 0x80);
      n >>>= 7;
    }
    buf[i++] = (byte) n;
    return Arrays.copyOf(buf, i);
  }

  private static String hex(byte[] b) {
    StringBuilder sb = new StringBuilder();
    for (int i = 0; i < Math.min(b.length, 512); i++) {
      sb.append(String.format("%02x", b[i] & 0xff));
    }
    if (b.length > 512) {
      sb.append("...(").append(b.length).append(" bytes)");
    }
    return sb.toString();
  }

  // ---- seeds -----------------------------------------------------------------------------

  private static List<byte[]> buildSeeds() throws SerealException {
    List<byte[]> seeds = new ArrayList<>();
    List<Object> plain = new ArrayList<>();
    List<Object> perl = new ArrayList<>();

    Map<String, Object> m = new LinkedHashMap<>();
    m.put("alpha", 1L);
    m.put("beta", -7L);
    m.put("gamma", "héllo ☃");
    m.put("delta", Arrays.asList(1L, 2L, 300L, -300L, Long.MIN_VALUE, Long.MAX_VALUE));
    m.put("eps", 3.25d);
    m.put("zeta", 1.5f);
    m.put("eta", new BigInteger("18446744073709551615"));
    m.put("theta", true);
    m.put("iota", null);
    m.put("kappa", "latin1 bytes".getBytes(StandardCharsets.ISO_8859_1));
    m.put("lambda", Pattern.compile("a.*b", Pattern.CASE_INSENSITIVE | Pattern.MULTILINE));
    plain.add(m);
    List<Object> rows = new ArrayList<>();
    for (int i = 0; i < 6; i++) {
      Map<String, Object> row = new HashMap<>();
      row.put("name", "row");
      row.put("value", (long) i);
      row.put("text", "repeated text for compression repeated text for compression");
      rows.add(row);
    }
    plain.add(rows);
    plain.add(Arrays.asList(Arrays.asList(Arrays.asList("deep", Arrays.asList(1L)))));
    plain.add("x");

    Map<String, Object> shared = new HashMap<>();
    shared.put("k", "v");
    PerlReference ref = new PerlReference(shared);
    List<Object> withRefs = new ArrayList<>();
    withRefs.add(ref);
    withRefs.add(ref);
    withRefs.add(new PerlReference(ref));
    withRefs.add(new PerlObject("Some::Class", new PerlReference(shared)));
    withRefs.add(new PerlObject("Some::Class", new PerlReference(Arrays.asList(1L, 2L))));
    withRefs.add(new WeakReference<Object>(ref));
    perl.add(withRefs);

    for (int version = 1; version <= 6; version++) {
      for (EncoderOptions.CompressionType ct : EncoderOptions.CompressionType.values()) {
        if (version < ct.minProtocolVersion) {
          continue;
        }
        for (int cksum = -1; cksum <= (version == 6 ? 2 : -1); cksum++) {
          for (int kind = 0; kind < 2; kind++) {
            EncoderOptions eo = new EncoderOptions().protocolVersion(version).compressionType(ct)
                .compressionThreshold(0).perlReferences(kind == 1).perlAliases(kind == 1);
            if (cksum >= 0) {
              byte[] key = cksum == 0 ? KEY0 : cksum == 1 ? KEY1 : KEY2;
              if (cksum == 0) {
                eo.checksumV0Key(key);
              } else if (cksum == 1) {
                eo.checksumV1Key(key);
              } else {
                eo.checksumV2Key(key);
              }
            }
            List<Object> objs = new ArrayList<>(kind == 1 ? perl : plain);
            if (version >= 5) {
              byte[] q = new byte[16];
              q[15] = 0x3f;
              objs.add(Arrays.asList(new Float128(q), "after"));
            }
            if (version >= 6) {
              objs.add(Arrays.asList(new PerlVString("v1.2.3"), new PerlVString("v1.200.3"), -5L));
            }
            for (Object obj : objs) {
              try {
                Encoder enc = new Encoder(eo);
                enc.write(obj);
                seeds.add(enc.getData());
                if (version >= 2) {
                  Encoder enc2 = new Encoder(eo);
                  enc2.write(obj, Arrays.asList("hdr", 1L));
                  seeds.add(enc2.getData());
                }
              } catch (SerealException | RuntimeException e) {
                // some combinations are not encodable; skip them
              }
            }
          }
        }
      }
    }

    // hand-written raw v3 bodies exercising REFP / ALIAS / COPY / OBJECTV / REGEXP
    byte[] h = {0x3d, (byte) 0xf3, 0x72, 0x6c, 0x03, 0x00};
    byte[][] bodies = {
      {0x42, (byte) 0xa8, 0x01, 0x29, 0x02},
      {0x43, (byte) 0x81, 0x2e, 0x02, 0x2f, 0x02},
      {0x42, 0x2c, 0x63, 'F', 'o', 'o', 0x40, 0x2d, 0x03, 0x40},
      {0x31, 0x63, 'a', '.', 'b', 0x62, 'i', 'm'},
      {0x52, 0x61, 'a', 0x01, 0x2f, 0x02, 0x02},
      {0x2b, 0x02, (byte) 0xab, 0x01, 0x01, 0x29, 0x02},
    };
    for (byte[] body : bodies) {
      byte[] p = Arrays.copyOf(h, h.length + body.length);
      System.arraycopy(body, 0, p, h.length, body.length);
      seeds.add(p);
    }
    return seeds;
  }
}
