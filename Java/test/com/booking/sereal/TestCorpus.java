package com.booking.sereal;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FilenameFilter;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.logging.Level;

import org.junit.Assert;

import com.booking.sereal.Utils.Function;

public class TestCorpus {

	private static Decoder dec;
	private static Encoder enc;

	static {
		Map<String, Object> decoder_options = new HashMap<String, Object>();
		decoder_options.put( "use_perl_refs", true ); // so ref to int will give a Reference object and not just an int

		dec = new Decoder( decoder_options );
		enc = new Encoder( null );
	}

	/**
	 * @param args
	 * @throws IOException
	 * @throws SerealException
	 */
	public static void main(String[] args) throws IOException {

		if( args.length == 0 ) {
			throw new UnsupportedOperationException( "Usage: Example [test_dir OR test_data_00XXXX]" );
		}

		final File target = new File( args[0] ).getCanonicalFile();

		if( !target.exists() ) {
			throw new FileNotFoundException( "No such file or directory: " + target.getAbsolutePath() );
		}

		if( target.isDirectory() ) {
			System.out.println( "Running decoder on all test files" );
			decodeAllTests( target );
		} else {
			System.out.println( "Decoding a single file: " + target.getAbsolutePath() );
			dec.log.setLevel( Level.FINE );
			enc.log.setLevel( Level.FINE );
			try {
				Object result = dec.decodeFile( target );
				System.out.println( "Success!" );
				System.out.println( Utils.dump( result ) );
				// now compare to input after encoding
				roundtrip( target );
			} catch (SerealException e) {
				e.printStackTrace();
			}
		}

	}

	private static boolean roundtrip(File target) {

		enc.reset();
		
		try {
			System.out.println( "Roundtrip encoding " + target.getName() );

			Object data = dec.decodeFile( target );
			dec.log.fine( "Data: " + Utils.dump( data ) );
			ByteBuffer encoded = enc.write( data );

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
		Map<String, String> fileToError = new HashMap<String, String>();
		Map<String, List<String>> results = new HashMap<String, List<String>>();
		results.put( "Success", new ArrayList<String>() );

		String[] filenames = test_dir.list( new FilenameFilter() {

			@Override
			public boolean accept(File arg0, String arg1) {
				return arg1.contains( "_data_" );
			}
		} );

		List<File> tests = Utils.map( filenames, new Function<String, File>() {

			@Override
			public File apply(String o) {
				return new File( test_dir, o );
			}

		} );

		for(File f : tests) {
			try {
				Decoder.decode_sereal( f, null );
				results.get( "Success" ).add( f.getName() );
			} catch (SerealException se) {
				System.out.println( se.getMessage() );
				List<String> files = results.get( se.getMessage() );
				files = files == null ? new ArrayList<String>() : files;
				files.add( f.getName() );
				results.put( se.getMessage(), files );
				fileToError.put( f.getName(), se.getMessage() );
			}
		}

		System.out.println( "\nFile - error\n\n" );
		for(Entry<String, String> entry : fileToError.entrySet()) {
			System.out.println( entry.getKey() + " -> " + entry.getValue() );
		}

		System.out.println( "\n\nResults:" );
		for(String msg : results.keySet()) {
			System.err.println( "\n" + msg + ":\t count=" + results.get( msg ).size() );
			if( results.get( msg ).size() < 20 ) {
				System.err.println( "\tFiles:\t" + results.get( msg ) );
			}

		}

		// now take all successes and run them through the encoder and see if the bytes are the same
		// as the file contents
		roundtrip( test_dir, results.get( "Success" ) );

	}

	private static void roundtrip(File test_dir, List<String> list) {

		int win = 0;
		for(String fn : list) {

			File test = new File( test_dir, fn );
			if( !roundtrip( test ) ) {
				System.out.println( "Aborting after first error" );
				System.out.printf( "Ratio: %d/%d = %.2f%%\n", win, list.size(), ((double)win/list.size()));
				return;
			}
			win++;

		}

	}

}
