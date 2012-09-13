package com.booking.sereal;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FilenameFilter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.logging.Level;

import com.booking.sereal.Utils.Function;

public class Example {

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
			Decoder dec = new Decoder( null );
			System.out.println( "Decoding a single file: " + target.getAbsolutePath() );
			dec.log.setLevel( Level.FINE );
			try {
				Object result = dec.decodeFile( target );
				System.out.println( "Success!" );
				System.out.println( Utils.dump( result ) );
			} catch (SerealException e) {
				e.printStackTrace();
			}
		}

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

	}

}
