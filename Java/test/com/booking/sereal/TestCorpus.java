package com.booking.sereal;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FilenameFilter;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;

import org.junit.Assert;

import com.booking.sereal.Utils.Function;

/**
 * To make the corpus of test files:
 * perl -I Perl/shared/t/lib/ -MSereal::TestSet -MSereal::Encoder -e'Sereal::TestSet::write_test_files("test_dir")'
 *
 * This runs the Decoder/Encoder over every file in the supplied directory and tells you when the bytes the encoder
 * outputs do not match the bytes in the test file. The purpose is to check if roundtripping to Perl type
 * datastructures works.
 *
 * If you pass a file as parameter it will do the same but do more detailed logging.
 *
 */
public class TestCorpus {

	private static StructureDecoder sd = new StructureDecoder();
	private static Decoder dec;
	private static Encoder enc;
	static boolean writeEncoded = false;

	static {
		Map<String, Object> decoder_options = new HashMap<String, Object>();
		decoder_options.put( "use_perl_refs", true ); // so ref to int will give a Reference object and not just an int
		decoder_options.put( "preserve_pad_tags", true ); // so pad bytes are saved

		dec = new Decoder( decoder_options );
		enc = new Encoder( null );
	}

	/**
	 * @param args
	 * @throws IOException
	 * @throws SerealException
	 */
	public static void main(String[] args) throws IOException {

		String manual = null; //"../test_dir/test_data_00026";

		if( args.length == 0 && manual == null ) {
			throw new UnsupportedOperationException( "Usage: Example [test_dir OR test_data_00XXXX]" );
		}

		final File target = new File( (manual == null ? args[0] : manual) ).getCanonicalFile(); // to absorb ".." in paths

		if( !target.exists() ) {
			throw new FileNotFoundException( "No such file or directory: " + target.getAbsolutePath() );
		}

		if( target.isDirectory() ) {
			System.out.println( "Running decoder on all test files in " + target.getCanonicalPath() );
			decodeAllTests( target );
		} else {
			System.out.println( "Decoding a single file: " + target.getAbsolutePath() );
			// more logging
			dec.log.setLevel( Level.FINE );
			enc.log.setLevel( Level.FINE );
			roundtrip( target );
		}

	}

	private static boolean roundtrip(File target) {

		enc.reset();

		try {
			System.out.println( "Roundtrip encoding " + target.getName() );

			System.out.println("For the puny humans: " + sd.decodeFile( target ));

			Object data = dec.decodeFile( target );
			System.out.println( "\nDecoding Done: " + Utils.dump( data ) + "\n");
			ByteBuffer encoded = enc.write( data );

			if( writeEncoded ) {
				FileOutputStream fos = new FileOutputStream( new File(target.getAbsolutePath() + "_java_encoded") );
				fos.write( encoded.array() );
				fos.close();
			}

			FileInputStream fis = new FileInputStream( target );
			ByteBuffer buf = ByteBuffer.allocate( (int) target.length() );
			fis.getChannel().read( buf );
			System.out.println( "From file: " + Utils.hexStringFromByteArray( buf.array() ) );
			System.out.println( "Encoded  : " + Utils.hexStringFromByteArray( encoded.array() ) );
			Assert.assertArrayEquals( "Roundtrip fail for: " + target.getName(), buf.array(), encoded.array() );
			System.out.println( "Roundtrip Success!" );
		} catch (SerealException e) {
			e.printStackTrace( System.out );
			return false;
		} catch (IOException e) {
			e.printStackTrace();
			return false;
		} catch (AssertionError a) {
			System.out.println( a.getMessage() );
			return false;
		}

		return true;

	}

	protected static void decodeAllTests(final File test_dir) throws IOException {

		// get all files called test_data_...
		String[] filenames = test_dir.list( new FilenameFilter() {

			@Override
			public boolean accept(File arg0, String arg1) {
				return arg1.contains( "test_data_" );
			}
		} );

		// turn them into Files
		List<File> tests = Utils.map( filenames, new Function<String, File>() {

			@Override
			public File apply(String o) {
				return new File( test_dir, o );
			}

		} );

		int win = 0;
		for(File test : tests) {

			boolean success = roundtrip( test );
			if( !success ) {
				System.out.println( "Aborting after first error" );
				return;
			}
			win += success ? 1 : 0;

		}
		System.out.printf( "Ratio: %d/%d = %.2f%%\n", win, tests.size(), ((double) 100*win / tests.size()) );

	}

}
