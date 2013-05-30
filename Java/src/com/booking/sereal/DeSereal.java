package com.booking.sereal;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

public class DeSereal {
	/**
	 * @param args
	 * @throws IOException
	 * @throws SerealException
	 */
	public static void main(String[] args) throws IOException, SerealException {
		if( args.length == 0 ) {
			throw new UnsupportedOperationException( "Usage: DeSereal test_data" );
		}

		Map<String, Object> decoder_options = new HashMap<String, Object>();
		decoder_options.put( "snappy_support", true );
		decoder_options.put( "use_perl_refs", false ); // so ref to int will give a Reference object and not just an int
		decoder_options.put( "preserve_pad_tags", true ); // so pad bytes are saved
        decoder_options.put("prefer_latin1", true); // Use latin1 instead of byte[] if possible

		Decoder dec = new Decoder( decoder_options );
		final File target = new File( args[0] ).getCanonicalFile(); // to absorb ".." in paths
//		dec.log.setLevel( Level.FINE );
		Object data = dec.decodeFile( target );
		System.out.println( Utils.dump( data ) );
	}

}
